-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module PlutipCanary where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_)
import Data.BigInt (fromInt) as BigInt
import Data.UInt (fromInt) as UInt
import Contract.Test.Plutip (runPlutipContract, PlutipConfig)
import Contract.Config (emptyHooks)
import Data.Time.Duration (Seconds(Seconds))

main :: Effect Unit
main = launchAff_ do
  let
    distribution =
      [ BigInt.fromInt 1_000_000_000 ]
        /\ [ BigInt.fromInt 1_000_000_000 ]
        /\ [ BigInt.fromInt 1_000_000_000 ]
  runPlutipContract config distribution \_ -> do
    logInfo' "Test run"

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
