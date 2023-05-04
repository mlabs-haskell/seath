-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module Seath.Test.Main (main) where

import Contract.Prelude

import Data.Array ((!!))
import Effect.Exception (throw)
import Node.Process (argv)
import Seath.Test.PlutipRunner as PlutipRunner
import Seath.Test.PreprodRunner as PreprodRunner
import Seath.Test.Seath as Seath
import Test.Examples.Addition.AutomatedEndToEndTest as AutoE2E
import Test.Examples.Addition.Demo.FullLeaderNode as FullLeaderNode
import Test.Examples.Addition.Demo.SeathUsers as DemoUsers

main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Just "preprod" -> case args !! 3 of

      Just "start-leader" -> PreprodRunner.run FullLeaderNode.startNode
      Just "start-users" -> PreprodRunner.run DemoUsers.startScenario

      Just "auto-e2e-test" -> PreprodRunner.run AutoE2E.mainTest
      other -> throw $ "Unknown args: " <> show other

    Just "addition-e2e-plutip" -> PlutipRunner.run
    Just "seath-test" -> Seath.test
    Nothing -> Seath.test -- default for CI
    other -> throw $ "Unknown args: " <> show other
