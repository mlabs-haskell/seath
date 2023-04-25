-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module Seath.Test.Main (main) where

import Contract.Prelude

import Data.Array ((!!))
import Effect.Exception (throw)
import Node.Process (argv)
import Seath.Test.PlutipRunner as PlutipRunner
import Seath.Test.PreprodRunner as PreprodRunner
import Seath.Test.Seath as Seath

main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Just "addition-preprod" -> PreprodRunner.run
    Just "addition-plutip" -> PlutipRunner.run
    Just "seath-test" -> Seath.test
    Nothing -> Seath.test -- default for CI
    other -> throw $ "Unknown args: " <> show other
