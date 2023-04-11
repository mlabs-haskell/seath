module Seath.Test.ForkDebug where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , defaultOgmiosWsConfig
  , emptyHooks
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Log (logInfo')
import Contract.Monad (launchAff_, runContractInEnv, withContractEnv)
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Contract.Utxos (getWalletUtxos)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array ((!!))
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Milliseconds(..))
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (delay, error)
import Effect.Aff (forkAff)
import Node.FS.Aff (readdir)
import Node.Path as Path
import Prelude (show)
import Seath.Test.Utils (makeKeyWallet)

main :: Effect Unit
main = do
  -- runWithPlutip
  -- runTestnet
  testFork

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
  where
  distrib =
    ([ BigInt.fromInt 1_000_000_000 ] /\ [ BigInt.fromInt 1_000_000_000 ])

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
  withContractEnv contractConfig $ \env -> do
    wallet <- liftMaybe (error "No key") (seathKeys !! 0)
    let
      logUtxos = runContractInEnv env $ withKeyWallet wallet $ do
        utxos <- getWalletUtxos
        logInfo' $ "UTXOS: " <> show utxos

    logUtxos
    _ <- forkAff $ do
      log "in fork"
      logUtxos

    delay (Milliseconds 2000.0)

    log "end"

  where
  mekeSeathKeys keysDir = do
    keyDirs <- readdir keysDir
    for keyDirs $ \keyDir -> makeKeyWallet $ Path.concat [ keysDir, keyDir ]

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

testFork :: Effect Unit
testFork = launchAff_ $ do
  withContractEnv contractConfig $ \_env -> do
    _ <- forkAff loop
    delay (Milliseconds 2000.0)
    log "end"
  where
    loop = do
      log "loop"
      delay (Milliseconds 500.0)
      loop
