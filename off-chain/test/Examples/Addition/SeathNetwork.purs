module Test.Examples.Addition.SeathNetwork (mainTest) where

import Contract.Prelude

import Aeson (class EncodeAeson, decodeJsonString)
import Contract.Log (logInfo')
import Contract.Monad (ContractEnv, runContractInEnv, throwContractError)
import Contract.Test (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Error.Class (try)
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UUID (UUID, parseUUID)
import Data.Unit (Unit)
import Effect.Aff (delay, error, forkAff)
import Payload.ResponseTypes (Response(..))
import Prelude (show)
import Seath.Core.Types (UserAction)
import Seath.Core.Utils as CoreUtils
import Seath.HTTP.Client (UserClient)
import Seath.HTTP.Client as Client
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.Types (IncludeRequest(IncludeRequest), UID(UID))
import Seath.Network.Leader as Leader
import Seath.Network.Types (ActionStatus, GetStatusError(..), IncludeActionError(..), LeaderConfiguration(..), LeaderNode, UserConfiguration(..), UserNode)
import Seath.Network.Users as Users
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.Contract as Addition
import Seath.Test.Examples.Addition.Types (AdditionAction(..))
import Type.Proxy (Proxy(Proxy))
import Undefined (undefined)

mainTest :: ContractEnv -> KeyWallet -> KeyWallet -> Array KeyWallet -> Aff Unit
mainTest env admin _leader users = do

  checkInitSctipt env admin 

  let
    serverConf :: SeathServerConfig
    serverConf = undefined

  user <- liftMaybe (error "No user wallet") (users !! 0)

  let
    makeAction action =
      runContractInEnv env $ withKeyWallet user
        $ CoreUtils.mkActionContract action

  (userNode :: UserNode AdditionAction) <- liftAff $ Users.startUserNode
    makeAction
    _testUserConf

  (leaderNode :: LeaderNode AdditionAction) <- liftAff $
    Leader.startLeaderNode _testLeaderConf

  liftAff $ do
    _ <- forkAff $ do
      log "Starting server"
      liftEffect $ Server.runServer serverConf leaderNode
      log "Leader server started"

    log "Delay before user include action request"
    delay $ Milliseconds 1000.0
    log "Fire user include action request 1"
    Users.performAction userNode
      (AddAmount $ BigInt.fromInt 1)
    delay $ Milliseconds 5000.0
    log "Fire user include action request 2"
    Users.performAction userNode
      (AddAmount $ BigInt.fromInt 2)
    Leader.showDebugState leaderNode >>= log
  log "end"

-- LeaderNode config
_testLeaderConf :: LeaderConfiguration AdditionAction
_testLeaderConf =
  LeaderConfiguration
    { maxWaitingTimeForSignature: 0
    , maxQueueSize: 4
    , numberOfActionToTriggerChainBuilder: 0
    , maxWaitingTimeBeforeBuildChain: 0
    }

-- UserNode config and handlers
_testUserConf :: UserConfiguration AdditionAction
_testUserConf = UserConfiguration
  { maxQueueSize: undefined
  , clientHandlers:
      { submitToLeader: userHandlerSendAction httpClient
      , acceptSignedTransaction: undefined
      , rejectToSign: undefined
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
userHandlerSendAction clent action = do
  res <- clent.leader.includeAction
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

checkInitSctipt :: ContractEnv -> KeyWallet -> Aff Unit
checkInitSctipt env admin = runContractInEnv env $ withKeyWallet admin $ do
  scriptState <- try $ Addition.queryBlockchainState
  case scriptState of
    Left _ -> do
      logInfo' "Initializing Addition script state"
      void $ Addition.initialSeathContract
    Right _ -> do
      logInfo' "Addition script state already initialized"
