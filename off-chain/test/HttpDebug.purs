module Seath.Test.HttpDebug
  ( _testUserNode
  , config
  , genAction
  , main
  , runWithPlutip
  , userHandlerSubmitAction
  ) where

import Aeson (class EncodeAeson)
import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.Config (LogLevel(Info), ServerConfig, emptyHooks)
import Contract.Monad (Aff, Contract, launchAff_, liftedM)
import Contract.Prelude
  ( class Monad
  , Either(..)
  , bind
  , discard
  , liftAff
  , liftEffect
  , log
  , note
  , pure
  , (<$>)
  , (<<<)
  , (<>)
  , (==)
  , (>>=)
  )
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Milliseconds(..), Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.UUID (UUID, genUUID, parseUUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (delay, forkAff)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Payload.ResponseTypes (Response(..))
import Prelude (show)
import Prelude (($))
import Seath.Core.Types (ChangeAddress(..), UserAction(..))
import Seath.HTTP.Client as Client
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.Types (IncludeRequest(..))
import Seath.Network.Leader (showDebugState)
import Seath.Network.OrderedMap as OMap
import Seath.Network.Types
  ( IncludeActionError(..)
  , LeaderConfiguration(..)
  , LeaderNode(..)
  , LeaderState(..)
  , UserConfiguration(..)
  , UserNode(..)
  , UserState(..)
  )
import Seath.Network.Users (sendActionToLeader)
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.SeathSetup (stateChangePerAction)
import Seath.Test.Examples.Addition.Types (AdditionAction(..))
import Undefined (undefined)

main :: Effect Unit
main = do
  runWithPlutip

runWithPlutip :: Effect Unit
runWithPlutip = launchAff_ $ runPlutipContract config distrib $
  \(a /\ _b) -> do
    testAction <- genAction a
    let
      serverConf :: SeathServerConfig
      serverConf = undefined

      userNode :: UserNode AdditionAction
      userNode = _testUserNode

    leaderNode <- liftEffect newTestLeaderNode

    liftAff $ do
      _ <- forkAff $ do
        log "Starting server"
        liftEffect $ Server.runServer serverConf leaderNode
        log "Leader server started"

      log "Delay before user include action request"
      delay $ Milliseconds 1000.0
      log "Fire user include action request"
      res <- userNode `sendActionToLeader` testAction
      log $ "Include request result: " <> show res
      showDebugState leaderNode >>= log
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

newTestLeaderNode :: Effect (LeaderNode AdditionAction)
newTestLeaderNode = do
  pending <- Ref.new OMap.empty
  pure $ LeaderNode
    { state: LeaderState
        { pendingActionsRequest: pending
        , prioritaryPendingActions: undefined
        , signatureResponses: undefined
        , stage: undefined
        , numberOfActionsRequestsMade: undefined

        }
    , configuration: LeaderConfiguration
        { maxWaitingTimeForSignature: undefined
        , maxQueueSize: 4
        , numberOfActionToTriggerChainBuilder: undefined
        , maxWaitingTimeBeforeBuildChain: undefined

        }
    }

-- Assembling UserNode
_testUserNode :: UserNode AdditionAction
_testUserNode = UserNode
  { state: UserState
      { pendingResponse: undefined
      , actionsSent: undefined
      , transactionsSent: undefined
      , submitedTransactions: undefined
      , numberOfActionsRequestsMade: undefined
      }
  , configuration: UserConfiguration
      { maxQueueSize: undefined
      , clientHandlers:
          { submitToLeader: userHandlerSubmitAction -- TODO: arch: naming
          , acceptSignedTransaction: undefined
          , rejectToSign: undefined
          , getActionStatus: undefined
          }

      }
  }

-- TODO: not real handler yet, just for server debugging
userHandlerSubmitAction
  :: forall a
   . EncodeAeson a
  => UserAction a
  -> Aff (Either IncludeActionError UUID)
userHandlerSubmitAction action = do
  res <- (Client.mkUserClient).leader.includeAction
    { body: IncludeRequest action }
  pure $ case res of
    Right resp -> convertResonse resp
    Left r -> Left $ OtherError $ "Leader failed to respond: " <> show r
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      note (OtherError "Can't parse request ID") $ parseUUID r.body.data
    -- FIXME: parse error from errData
    else Left $ OtherError $ "FIXME: parse error from errData: " <>
      r.body.errData

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
