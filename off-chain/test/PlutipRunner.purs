module Seath.Test.PlutipRunner (run) where

import Contract.Config (LogLevel(Info), emptyHooks)
import Contract.Monad (launchAff_)
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (supervise)
import Prelude (($))
import Seath.Test.Utils (makeDistribution)
-- import Test.Examples.Addition.SeathNetwork (mainTest)
-- import Test.Examples.Addition.SeathNetworkReject (mainTest)
import Test.Examples.Addition.SeathNetworkSameUserTwice (mainTest)

run :: Effect Unit
run = launchAff_
  $ withPlutipContractEnv config (makeDistribution 4)
  $
    \env ((adminWallet /\ leaderWallet) /\ participantsWallets) -> do
      ( supervise -- ! misha: I've added it here so we get the same behavior as with `withContractEnv``

          $ mainTest env adminWallet leaderWallet
              participantsWallets
      )

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Info
  , ogmiosConfig:
      { port: UInt.fromInt 8081
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 8080
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

