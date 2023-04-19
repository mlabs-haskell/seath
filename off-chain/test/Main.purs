-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module Seath.Test.Main (main) where

import Contract.Prelude

import Data.Array ((!!))
import Effect.Exception (throw)
import Node.Process (argv)
import Seath.Test.PlutipRunner as PlutipRunner
import Seath.Test.PreprodRunner as PreprodRunner
import Seath.Test.Spec as Spec

main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Just "preprod" -> PreprodRunner.run
    Just "plutip" -> PlutipRunner.run
    Just "unit" -> Spec.test
    Nothing -> Spec.test *> PlutipRunner.run -- default for CI
    other -> throw $ "Unknown args: " <> show other
