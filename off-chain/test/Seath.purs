module Seath.Test.Seath (test) where

import Contract.Prelude

import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip (testPlutipContracts)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT))
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (group)
import Seath.Test.Unit.Leader (suite) as Leader
import Seath.Test.Unit.OrderedMap (suite) as OrderedMap
import Seath.Test.Utils as Test.Utils
import Test.Spec.Runner (defaultConfig)

test :: Effect Unit
test = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $ do
      group "Nodes" do
        testPlutipContracts Test.Utils.plutipConfig Leader.suite
      group "Unit" do
        OrderedMap.suite
