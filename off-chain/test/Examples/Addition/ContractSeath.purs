module Seath.Test.Examples.Addition.ContractSeath (mainTest) where

import Contract.Log (logInfo')
import Contract.Monad (Aff)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Control.Monad (bind)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Prelude (($))
import Seath.HandleActions as Seath
import Seath.Test.Examples.Addition.Actions as Addition
import Test.Examples.Addition.SeathSetup as Setup

mainTest :: PlutipConfig -> Aff Unit
mainTest config = runPlutipContract config distribution $ \ws -> do
  let setup = Setup.mkSetup ws
  actions <- Setup.genUserActions setup.participants
  _ <- logInfo' $ "test " <> show actions

  let
    buildChain =
      Seath.actions2UTxOChain
        Addition.action2ConstraintsAndLookup
        actions
        Addition.initialState

  (finalizedTxs /\ finalState) <- buildChain
  _ <- Setup.submitChain setup.leader finalizedTxs
  logInfo' "end"
  where

  distribution :: Array BigInt /\ Array BigInt /\ Array BigInt
  distribution =
    [ BigInt.fromInt 1_000_000_000 ]
      /\ [ BigInt.fromInt 1_000_000_000 ]
      /\ [ BigInt.fromInt 1_000_000_000 ]