module Seath.Test.PreprodRunner (run) where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , defaultOgmiosWsConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Monad (launchAff_, withContractEnv)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (drop, (!!))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect.Aff (error)
import Node.FS.Aff (readdir)
import Node.Path as Path
import Seath.Test.Types (RunnerSetup)
import Seath.Test.Utils (makeKeyWallet)

run :: (RunnerSetup -> Aff Unit) -> Effect Unit
run runSeath = launchAff_ $ do
  seathKeys <- mekeSeathKeys "./test/keys/seath_keys"
  (admin /\ leader /\ users) <-
    liftMaybe (error "Could not build runner config") $ mkRunnerConf
      seathKeys

  withContractEnv config $ \env -> do
    runSeath
      { contractEnv: env
      , adminWallet: admin
      , leaderWallet: leader
      , userWallets: users
      }

  where
  mekeSeathKeys keysDir = do
    keyDirs <- readdir keysDir
    for keyDirs $ \keyDir -> makeKeyWallet $ Path.concat [ keysDir, keyDir ]

  mkRunnerConf keys = do
    admin <- keys !! 0
    leader <- keys !! 1
    users <- pure $ drop 2 keys
    pure (admin /\ leader /\ users)

config :: ContractParams
config = testnetConfig
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
