module Seath.Test.PlutipRunner (run) where

import Contract.Config (LogLevel(Info), emptyHooks)
import Contract.Monad (launchAff_)
import Contract.Prelude (Effect, Maybe(Nothing))
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (replicate)
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect.Aff (error)
import Prelude (($))
import Seath.Test.Examples.Addition.ContractSeath as SeathAddition
import Seath.Test.Examples.Addition.Types (AdditionDatum(AdditionDatum))
import Seath.Test.TestSetup (RunnerConfig(RunnerConfig))

run :: Effect Unit
run = launchAff_
  $ runPlutipContract config distribution
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
            , alreadyInitialized: false
            , expectedFinalState: AdditionDatum
                { lockedAmount: BigInt.fromInt 500 }
            }

      SeathAddition.mainTest runnerConf

  where

  distribution
    :: (Array BigInt /\ Array BigInt) /\ (Array (Array BigInt))
  distribution =
    ([ BigInt.fromInt 1_000_000_000 ] /\ [ BigInt.fromInt 1_000_000_000 ]) /\
      replicate 4 [ BigInt.fromInt 1_000_000_000 ]

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