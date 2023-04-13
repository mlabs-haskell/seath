module Test.Examples.Addition.SeathNetwork
  ( mainTest
  , userHandlerRefuseToSign
  ) where

import Contract.Prelude

import Aeson (class EncodeAeson, decodeJsonString)
import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Natural as Natural
import Contract.Test (withKeyWallet)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe, throwError, try)
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UUID (UUID, parseUUID)
import Data.Unit (Unit)
import Effect.Aff (delay, error)
import Payload.ResponseTypes (Response(Response))
import Payload.Server as Payload.Server
import Prelude (show)
import Seath.Common.Types (UID(UID))
import Seath.Core.ChainBuilder as ChainBuilder
import Seath.Core.Types (CoreConfiguration(CoreConfiguration), UserAction)
import Seath.Core.Utils as Core.Utils
import Seath.HTTP.Client (UserClient)
import Seath.HTTP.Client as Client
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.Types (IncludeRequest(IncludeRequest), SendSignedRequest(..))
import Seath.Network.Leader as Leader
import Seath.Network.Types
  ( AcceptSignedTransactionError
  , ActionStatus
  , GetStatusError(GSOtherError)
  , IncludeActionError(IAOtherError)
  , LeaderConfiguration(LeaderConfiguration)
  , LeaderNode
  , SendSignedTransaction
  , UserConfiguration(UserConfiguration)
  , UserNode
  , readSentActions
  )
import Seath.Network.Users as Users
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.Actions (queryBlockchainState) as Addition.Actions
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.Contract (initialSeathContract) as Addition.Contract
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum
  , AdditionRedeemer
  , AdditionValidator
  )
import Type.Proxy (Proxy(Proxy))
import Undefined (undefined)

mainTest :: ContractEnv -> KeyWallet -> KeyWallet -> Array KeyWallet -> Aff Unit
mainTest env admin leader users = do
  let initializationOptions = { waitingTime: 3, maxAttempts: 10 }

  checkInitSctipt env admin initializationOptions

  let
    serverConf :: SeathServerConfig
    serverConf = undefined

  user <- liftMaybe (error "No user wallet") (users !! 0)

  coreConfig <- runContractInEnv env $ withKeyWallet leader $
    mkAdditionCoreConfig

  let
    makeAction action =
      runContractInEnv env $ withKeyWallet user
        $ Core.Utils.makeActionContract action

    buildChain actions =
      runContractInEnv env $ withKeyWallet leader $ fst
        <$> ChainBuilder.buildChain coreConfig actions Nothing

  log "Starting user node"
  (userFiber /\ (userNode :: UserNode AdditionAction)) <- liftAff $
    Users.startUserNode
      makeAction
      _testUserConf

  log "Starting leader node"
  (leaderNode :: LeaderNode AdditionAction) <- liftAff $
    Leader.startLeaderNode _testLeaderConf

  log "Starting server"
  server <- Server.runServer serverConf leaderNode >>= liftEither <<< lmap error
  log "Leader server started"

  log "Delay before user include action request"
  delay $ Milliseconds 1000.0
  log "Fire user include action request 1"
  Users.performAction userNode
    (AddAmount $ BigInt.fromInt 1)
  delay $ Milliseconds 1000.0
  log "Fire user include action request 2"
  Users.performAction userNode
    (AddAmount $ BigInt.fromInt 2)
  Leader.showDebugState leaderNode >>= log

  -- test sending rejections
  uids <- (map fst) <$> readSentActions userNode
  maybe (throwError $ error "ff")
    (Users.sendRejectionToLeader userNode)
    (uids !! 0)

  delay (Milliseconds 15000.0)
  -- we don't really need this as all is run in supervise, but is good to have 
  -- the option
  -- killFiber (error "can't cleanup user") userFiber
  Payload.Server.close server
  log "end"

-- LeaderNode config
_testLeaderConf :: LeaderConfiguration AdditionAction
_testLeaderConf =
  LeaderConfiguration
    { maxWaitingTimeForSignature: 0
    , maxQueueSize: 4
    , numberOfActionToTriggerChainBuilder: 2
    , maxWaitingTimeBeforeBuildChain: 0
    }

-- UserNode config and handlers
_testUserConf :: UserConfiguration AdditionAction
_testUserConf = UserConfiguration
  { maxQueueSize: undefined
  , clientHandlers:
      { submitToLeader: userHandlerSendAction httpClient
      , sendSignedToLeader: userHandlerSendSignedToLeader httpClient
      , refuseToSign: userHandlerRefuseToSign httpClient
      , getActionStatus: userHandlerGetStatus httpClient
      }

  }
  where
  httpClient :: UserClient AdditionAction
  httpClient = Client.mkUserClient (Proxy :: Proxy AdditionAction)
    "http://localhost:3000"

userHandlerSendAction
  :: forall a
   . EncodeAeson a
  => UserClient a
  -> UserAction a
  -> Aff (Either IncludeActionError UUID)
userHandlerSendAction client action = do
  res <- client.leader.includeAction
    { body: IncludeRequest action }
  pure $ case res of
    Right resp -> do
      convertResonse resp
    Left r -> Left $ IAOtherError $ "Leader failed to respond: " <> show r
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      note (IAOtherError "Can't parse request ID") $ parseUUID r.body.data
    else either (show >>> IAOtherError >>> Left) Left
      (decodeJsonString r.body.data)

userHandlerGetStatus
  :: UserClient AdditionAction
  -> UUID
  -> Aff (Either GetStatusError ActionStatus)
userHandlerGetStatus client uuid = do
  res <-
    client.leader.actionStatus
      { params: { uid: UID uuid } }
  pure $ case res of
    Right resp -> convertResonse resp
    Left r -> Left $ GSOtherError $ "Leader failed to respond: " <> show r
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      lmap (show >>> GSOtherError) (decodeJsonString r.body.data)
    else either (show >>> GSOtherError >>> Left) Left
      (decodeJsonString r.body.data)

userHandlerSendSignedToLeader
  :: UserClient AdditionAction
  -> SendSignedTransaction
  -> Aff (Either AcceptSignedTransactionError Unit)
userHandlerSendSignedToLeader client sendSig = do
  res <- client.leader.acceptSignedTransaction
    { body: SendSignedRequest sendSig }
  case res of
    -- FIXME: leader responds witn Unit always
    -- but client expects AcceptSignedTransactionError as well
    Right _resp -> pure $ Right unit
    Left e -> throwError (error $ show e)

userHandlerRefuseToSign
  :: UserClient AdditionAction
  -> UUID
  -> Aff Unit
userHandlerRefuseToSign client uuid = do
  res <-
    client.leader.refuseToSign
      { params: { uid: UID uuid } }
  case res of
    Right _ -> pure unit
    Left r -> throwError (error $ show r)

checkInitSctipt
  :: ContractEnv
  -> KeyWallet
  -> { waitingTime :: Int, maxAttempts :: Int }
  -> Aff Unit
checkInitSctipt env admin waitingTime = runContractInEnv env
  $ withKeyWallet admin
  $ do
      waitForFunding waitingTime
      scriptState <- try $ Addition.Actions.queryBlockchainState
      case scriptState of
        Left _ -> do
          logInfo' "Initializing Addition script state"
          void $ Addition.Contract.initialSeathContract
        Right _ -> do
          logInfo' "Addition script state already initialized"

waitForFunding :: { waitingTime :: Int, maxAttempts :: Int } -> Contract Unit
waitForFunding options = do
  slotsTime <-
    liftMaybe
      (error $ "can't convert waiting time: " <> show options.waitingTime) $
      Natural.fromInt options.waitingTime
  _ <- waitNSlots slotsTime
  loop slotsTime options.maxAttempts
  where
  loop :: Natural -> Int -> Contract Unit
  loop slots remainingAttempts =
    if remainingAttempts == 0 then pure unit
    else do
      logInfo' "Waiting for funds in wallet"
      mutxos <- getWalletUtxos
      case mutxos of
        Just utxos ->
          if Map.isEmpty utxos then
            waitNSlots slots *> loop slots (remainingAttempts - 1)
          else pure unit
        Nothing -> waitNSlots slots *> loop slots (remainingAttempts - 1)

mkAdditionCoreConfig
  ∷ Contract
      ( CoreConfiguration
          AdditionAction
          BigInt
          AdditionValidator
          AdditionDatum
          AdditionRedeemer
      )
mkAdditionCoreConfig = do
  vaildatorHash <- Addition.fixedValidatorHash
  leaderPkh <- getPublicKeyHash
  pure $ CoreConfiguration
    { leader: leaderPkh
    , stateVaildatorHash: vaildatorHash
    , actionHandler: Addition.handleAction
    , queryBlockchainState: Addition.queryBlockchainState
    , numberOfBuiltChains: 0
    }
    