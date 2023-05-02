-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module Seath.Test.Main (main) where

import Contract.Prelude

import Data.Array ((!!))
import Effect.Exception (throw)
import Node.Process (argv)
import Seath.Test.PlutipRunner as PlutipRunner
import Seath.Test.PreprodRunner as PreprodRunner
import Seath.Test.Seath as Seath
import Test.Examples.Addition.Demo.SeathServerNode as DemoServer
import Test.Examples.Addition.Demo.SeathUsers as DemoUsers
import Test.Examples.Addition.AutomatedEndToEndTest as AutoE2E

main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Just "preprod" -> case args !! 3 of
      Just "e2e-test" -> PreprodRunner.run AutoE2E.mainTest
      Just "start-server" -> PreprodRunner.run DemoServer.startLeaderSeathNode
      Just "start-users" -> PreprodRunner.run DemoUsers.startScenario
      other -> throw $ "Unknown args: " <> show other

    Just "addition-plutip" -> PlutipRunner.run
    Just "seath-test" -> Seath.test
    Nothing -> Seath.test -- default for CI
    other -> throw $ "Unknown args: " <> show other
