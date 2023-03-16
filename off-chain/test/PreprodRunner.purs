module Seath.Test.PreprodRunner (run) where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , defaultOgmiosWsConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Monad (launchAff_, runContract)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (drop, (!!))
import Data.Array.NonEmpty as NE
import Data.BigInt as BigInt
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect.Aff (error)
import Node.FS.Aff (readdir)
import Node.Path as Path
import Seath.Test.Examples.Addition.ContractSeathNew as SeathAddition
import Seath.Test.TestSetup (RunnerConfig(..), makeKeyWallet)

run :: Effect Unit
run = launchAff_ $ do
  seathKeys <- mekeSeathKeys "./test/keys/seath_keys"
  runnerConf <- liftMaybe (error "Could not build runner config") $ mkRunnerConf
    seathKeys

  runContract config (SeathAddition.mainTest runnerConf)

  where
  mekeSeathKeys keysDir = do
    keyDirs <- readdir keysDir
    for keyDirs $ \keyDir -> makeKeyWallet $ Path.concat [ keysDir, keyDir ]

  mkRunnerConf :: Array KeyWallet -> Maybe RunnerConfig
  mkRunnerConf keys = do
    admin <- keys !! 0
    leader <- keys !! 1
    participants <- NE.fromArray $ drop 2 keys
    pure $
      RunnerConfig
        { admin: admin
        , seathLeader: leader
        , seathParticipants: participants
        , minAdaRequired: BigInt.fromInt 200
        , alreadyInitiated: true
        }

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
  }