module Seath.Test.ForkDebug where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeJsonString)
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Contract.Address (PubKeyHash, getWalletAddressesWithNetworkTag)
import Contract.Config (emptyHooks)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContractInEnv)
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Milliseconds(..), Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.UUID (UUID, parseUUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (delay, forkAff, try)
import Payload.ResponseTypes (Response(..))
import Prelude (show)
import Seath.Core.ChainBuilder as ChainBuilder
import Seath.Core.Types
  ( ChangeAddress(..)
  , CoreConfiguration(..)
  , UserAction(..)
  )
import Seath.HTTP.Client as Client
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.Types (IncludeRequest(..), JSend, UID(..))
import Seath.Network.Leader as Leader
import Seath.Network.OrderedMap as OMap
import Seath.Network.Types
  ( ActionStatus
  , GetStatusError(..)
  , IncludeActionError(..)
  , LeaderConfiguration(..)
  , LeaderNode
  , UserConfiguration(..)
  , UserNode
  , getPending
  , readSentActions
  )
import Seath.Network.Users as Users
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.Contract (initialSeathContract)
import Seath.Test.Examples.Addition.SeathSetup (stateChangePerAction)
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(..)
  , AdditionDatum
  , AdditionRedeemer
  , AdditionValidator
  )
import Undefined (undefined)

main :: Effect Unit
main = do
  runWithPlutip
  -- runTestnet

runWithPlutip :: Effect Unit
runWithPlutip = launchAff_ $ withPlutipContractEnv config distrib $
  \env (leader /\ user) -> do
    -- TODO: ? wait till funds appear in wallets
    let wallet = leader
    let
      logUtxos = runContractInEnv env $ withKeyWallet wallet $ do
        utxos <- getWalletUtxos
        logInfo' $ "UTXOS: " <> show utxos

    logUtxos
    logUtxos
    _ <- forkAff logUtxos
    log "end"

  
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


runTestnet :: Effect Unit
runTestnet = launchAff_ $ do
  seathKeys <- mekeSeathKeys "./test/keys/seath_keys"
  runnerConf <- liftMaybe (error "Could not build runner config") $ mkRunnerConf
    seathKeys
  withContractEnv contractConfig $ \enc -> do
    undefined
  -- runContract contractConfig (SeathAddition.mainTest runnerConf)

  where
  mekeSeathKeys keysDir = do
    keyDirs <- readdir keysDir
    for keyDirs $ \keyDir -> makeKeyWallet $ Path.concat [ keysDir, keyDir ]

  mkRunnerConf
    :: Array KeyWallet
    -> Maybe (RunnerConfiguration AdditionState AdditionAction)
  mkRunnerConf keys = do
    admin <- keys !! 0
    leader <- keys !! 1
    participants <- NE.fromArray $ drop 2 keys
    pure $
      RunnerConfiguration
        { admin: admin
        , leader: undefined leader
        , participants: undefined participants
        , minAdaRequired: BigInt.fromInt 200
        , expectedStateChange: (+) (length participants * stateChangePerAction)
        , logLevel: LogLevel.Debug
        }

contractConfig :: ContractParams
contractConfig = testnetConfig
  { backendParams = mkCtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig
      , kupoConfig:
          { port: UInt.fromInt 1442
          , host: "localhost"
          , secure: false
          , path: Nothing
          }
      }
  , logLevel = Info
  }
