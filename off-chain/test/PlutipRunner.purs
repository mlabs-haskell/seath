module Seath.Test.PlutipRunner (run) where

import Contract.Monad (launchAff_)
import Contract.Test.Plutip (withPlutipContractEnv)
import Data.Tuple.Nested ((/\))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (supervise)
import Prelude (($))
import Seath.Test.Utils (makeDistribution)
import Seath.Test.Utils as Test.Utils
import Test.Examples.Addition.SeathNetwork (mainTest)

run :: Effect Unit
run = launchAff_
  $ withPlutipContractEnv Test.Utils.plutipConfig (makeDistribution 6)
  $
    \env ((adminWallet /\ leaderWallet) /\ userWallets) -> do
      ( supervise -- added it here so we get the same behavior as with `withContractEnv`

          $ mainTest
              { contractEnv: env
              , adminWallet
              , leaderWallet
              , userWallets
              }
      )
