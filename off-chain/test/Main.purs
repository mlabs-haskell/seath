-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module Seath.Test.Main (main) where

import Contract.Prelude
  ( Effect
  , Maybe(..)
  , Unit
  , bind
  , discard
  , log
  , show
  , ($)
  , (<>)
  )
import Data.Array ((!!))
import Node.Process (argv)
import Seath.Test.PlutipRunner as PlutipRunner
import Seath.Test.PreprodRunner as PreprodRunner

main :: Effect Unit
main = do
  args <- argv
  log $ show args
  case args !! 2 of
    Just "preprod" -> PreprodRunner.run
    Just "plutip" -> PlutipRunner.run
    other -> log $ "Unknown args: " <> show other
