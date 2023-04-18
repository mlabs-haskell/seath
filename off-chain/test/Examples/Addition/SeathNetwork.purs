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
import Contract.Transaction
  ( BalancedSignedTransaction
  , FinalizedTransaction
  , signTransaction
  )
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
import Effect.Aff (delay, error, forkAff, killFiber)
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
import Seath.HTTP.Types
  ( IncludeRequest(IncludeRequest)
  , SendSignedRequest(SendSignedRequest)
  )
import Seath.Network.Leader as Leader
import Seath.Network.Types
  ( AcceptSignedTransactionError
  , ActionStatus
  , FunctionToPerformContract(FunctionToPerformContract)
  , IncludeActionError
  , LeaderConfiguration(LeaderConfiguration)
  , LeaderNode
  , SendSignedTransaction
  , UserConfiguration(UserConfiguration)
  , UserNode
  )
import Seath.Network.Users as Users
import Seath.Network.Utils (getPublicKeyHash, readSentActions)
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
  let
    initializationOptions = { waitingTime: 3, maxAttempts: 10 }
    _testLeaderConf = makeTestLeaderConf env admin

  checkInitSctipt env admin initializationOptions

  let
    serverConf :: SeathServerConfig
    serverConf = undefined

  user <- liftMaybe (error "No user wallet") (users !! 0)

  coreConfig <- runContractInEnv env $ withKeyWallet leader $
    mkAdditionCoreConfig

  let -- hadler for the User to make action using CTL Contract
    makeAction action =
      runContractInEnv env $ withKeyWallet user
        $ Core.Utils.makeActionContract action

    -- handler for the Leader to build chain using CTL contract
    buildChain actions =
      runContractInEnv env $ withKeyWallet leader $ fst
        <$> ChainBuilder.buildChain coreConfig actions Nothing

    signTx :: FinalizedTransaction -> Aff BalancedSignedTransaction
    signTx tx =
      runContractInEnv env $ withKeyWallet user
        $ signTransaction tx

  log "Starting user node"
  (userFiber /\ (userNode :: UserNode AdditionAction)) <- Users.startUserNode
    makeAction
    signTx
    _testUserConf

  log "Initializing leader node"
  (leaderNode :: LeaderNode AdditionAction) <- Leader.newLeaderNode
    _testLeaderConf
    buildChain

  log "Starting leader node loop"
  leaderLoopFiber <- forkAff $ Leader.leaderLoop leaderNode

  log "Starting server"
  server <- Server.runServer serverConf leaderNode >>= liftEither <<< lmap error
  log "Leader server started"

  log "Delay before user include action request"
  delay $ Milliseconds 1000.0
  log "Fire user include action request 1"
  Users.performAction userNode
    (AddAmount $ BigInt.fromInt 1)

  log "Fire user include action request 2"
  Users.performAction userNode
    (AddAmount $ BigInt.fromInt 2)
  Leader.showDebugState leaderNode >>= log

  delay (Milliseconds 10000.0)
  -- we don't really need this as all is run in supervise, but is good to have 
  -- the option
  killFiber (error "can't cleanup user") userFiber
  killFiber (error "can't cleanup leader loop") leaderLoopFiber
  Payload.Server.close server
  log "end"

-- LeaderNode config
makeTestLeaderConf
  :: ContractEnv -> KeyWallet -> LeaderConfiguration AdditionAction
makeTestLeaderConf env kw =
  LeaderConfiguration
    { maxWaitingTimeForSignature: 3000
    , maxQueueSize: 4
    , numberOfActionToTriggerChainBuilder: 2
    , maxWaitingTimeBeforeBuildChain: 2
    , fromContract: FunctionToPerformContract (makeToPerformContract env kw)
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
  case res of
    Right resp -> do
      convertResonse resp
    Left r -> throwError
      (error $ "Leader failed to respond to send action: " <> show r)
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      maybe (throwError $ error "Can't parse request ID") (Right >>> pure)
        (parseUUID r.body.data)
    else either (show >>> error >>> throwError) (Left >>> pure)
      (decodeJsonString r.body.data)

userHandlerGetStatus
  :: UserClient AdditionAction
  -> UUID
  -> Aff ActionStatus
userHandlerGetStatus client uuid = do
  res <-
    client.leader.actionStatus
      { params: { uid: UID uuid } }
  case res of
    Right resp -> convertResonse resp
    Left r -> throwError (error $ "Leader failed to respond: " <> show r)
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      either (show >>> error >>> throwError) pure
        (decodeJsonString r.body.data)
    else either (show >>> error >>> throwError) (error >>> throwError)
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

makeToPerformContract
  :: forall b. ContractEnv -> KeyWallet -> Contract b -> Aff b
makeToPerformContract env admin contract = runContractInEnv env
  $ withKeyWallet admin contract

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
      (error $ "Can't convert waiting time: " <> show options.waitingTime) $
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
