module Seath.Test.PlutipRunner (run) where

import Contract.Config (LogLevel(Info), emptyHooks)
import Contract.Monad (launchAff_)
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Data.Maybe (Maybe(Nothing))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (supervise)
import Prelude (($))
import Seath.Test.Utils (makeDistribution)
import Seath.Test.Utils as Test.Utils
import Test.Examples.Addition.SeathNetwork (mainTest)

run :: Effect Unit
run = launchAff_
  $ withPlutipContractEnv Test.Utils.plutipConfig (makeDistribution 4)
  $
    \env ((adminWallet /\ leaderWallet) /\ participantsWallets) -> do
      ( supervise -- ! misha: I've added it here so we get the same behavior as with `withContractEnv``

          $ mainTest env adminWallet leaderWallet
              participantsWallets
      )

