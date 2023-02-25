-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module PlutipCanary (main) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Monad (launchAff_)
import Contract.Test.Plutip (PlutipConfig)
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Seath.Test.Examples.Addition.Contract (simpleTest) as Addition.Contract

main :: Effect Unit
main = launchAff_ (Addition.Contract.simpleTest config)

--  let
--    distribution =
--      [ BigInt.fromInt 1_000_000_000 ]
--        /\ [ BigInt.fromInt 1_000_000_000 ]
--        /\ [ BigInt.fromInt 1_000_000_000 ]
--  runPlutipContract config distribution \_ -> do
--    logInfo' "Test run"

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
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
      { slotLength: Seconds 0.05 }
  }
