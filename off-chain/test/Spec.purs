module Seath.Test.Spec (test) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Seath.Test.Unit.OrderedMap as OrderedMap
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

test :: Effect Unit
test = launchAff_ $ runSpec [ consoleReporter ] do
  describe "unit-tests" do
    OrderedMap.spec
