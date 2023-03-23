module Seath.Test.PlutipRunner (run) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Monad (launchAff_)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array.NonEmpty as NE
import Data.BigInt as BigInt
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect.Aff (error)
import Prelude (($))
import Seath.Test.Examples.Addition.ContractSeath as SeathAddition
import Seath.Test.Examples.Addition.SeathSetup (stateChangePerAction)
import Seath.Test.QuickCheck (makeDistribution)
import Seath.Test.Types (RunnerConfig(RunnerConfig))

run :: Effect Unit
run = launchAff_
  $ runPlutipContract config (makeDistribution 4)
  $
    \((admin /\ leader) /\ participants) -> do
      participants' <- liftMaybe (error "No participants found")
        (NE.fromArray participants)
      let
        runnerConf =
          RunnerConfig
            { admin: admin
            , seathLeader: leader
            , seathParticipants: participants'
            , minAdaRequired: BigInt.fromInt 200
            , expectedStateChange: (+)
                (length participants * stateChangePerAction)
            }

      SeathAddition.mainTest runnerConf

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
