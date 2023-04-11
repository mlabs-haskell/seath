module Seath.Test.HttpDebug
  ( config
  , genAction
  , main
  , runWithPlutip
  , userHandlerSendAction
  ) where

import Contract.Prelude

import Aeson (class EncodeAeson, decodeJsonString)
import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.Config (emptyHooks)
import Contract.Monad (Contract, launchAff_, liftedM, runContractInEnv)
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip
  ( PlutipConfig
  , runPlutipContract
  , withPlutipContractEnv
  )
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Milliseconds(Milliseconds), Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.UUID (UUID, parseUUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (delay, forkAff)
import Payload.ResponseTypes (Response(..))
import Prelude (show)
import Seath.Core.Types (ChangeAddress(..), UserAction(..))
import Seath.HTTP.Client (UserClient)
import Seath.HTTP.Client as Client
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.Types (IncludeRequest(IncludeRequest), UID(UID))
import Seath.Network.Leader as Leader
import Seath.Network.Types
  ( ActionStatus
  , GetStatusError(..)
  , IncludeActionError(..)
  , LeaderConfiguration(..)
  , LeaderNode
  , UserConfiguration(..)
  , UserNode
  )
import Seath.Network.Users as Users
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.SeathSetup (stateChangePerAction)
import Seath.Test.Examples.Addition.Types (AdditionAction(..))
import Type.Proxy (Proxy(Proxy))
import Undefined (undefined)

main :: Effect Unit
main = do
  runWithPlutip

runWithPlutip :: Effect Unit
runWithPlutip = launchAff_ $ withPlutipContractEnv config distrib $
  \env (leader /\ user) -> do
    let
      serverConf :: SeathServerConfig
      serverConf = undefined

      makeAction action =
        runContractInEnv env $ withKeyWallet leader
          $ mkActionContract action

    (userNode :: UserNode AdditionAction) <-
      liftAff $ Users.startUserNode makeAction _testUserConf

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

  where
  distrib =
    ([ BigInt.fromInt 1_000_000_000 ] /\ [ BigInt.fromInt 1_000_000_000 ])

genAction :: KeyWallet -> Contract (UserAction AdditionAction)
genAction w =
  withKeyWallet w $ do
    ownUtxos <- liftedM "no UTxOs found" getWalletUtxos
    publicKeyHash <- withKeyWallet w getPublicKeyHash
    changeAddress <- liftedM "can't get Change address" $ head <$>
      getWalletAddressesWithNetworkTag
    pure $ UserAction
      { action: AddAmount stateChangePerAction
      , publicKey: publicKeyHash
      , userUTxOs: ownUtxos
      , changeAddress: ChangeAddress changeAddress
      }

-- Assembling LeaderNode

_testLeaderConf :: LeaderConfiguration AdditionAction
_testLeaderConf =
  LeaderConfiguration
    { maxWaitingTimeForSignature: 0
    , maxQueueSize: 4
    , numberOfActionToTriggerChainBuilder: 0
    , maxWaitingTimeBeforeBuildChain: 0
    }

-- Assembling UserNode
_testUserConf :: UserConfiguration AdditionAction
_testUserConf = UserConfiguration
  { maxQueueSize: undefined
  , clientHandlers:
      { submitToLeader: userHandlerSendAction httpClient -- TODO: arch: naming
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

-- Plutip config
config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Info
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 1.0 }
  }

mkActionContract :: forall a. a -> Contract (UserAction a)
mkActionContract action = do
  ownUtxos <- liftedM "Error making action: no UTxOs found" getWalletUtxos
  publicKeyHash <- getPublicKeyHash
  changeAddress <- liftedM "can't get Change address" $ head <$>
    getWalletAddressesWithNetworkTag
  pure $ UserAction
    { action: action
    , publicKey: publicKeyHash
    , userUTxOs: ownUtxos
    , changeAddress: (wrap changeAddress)
    }