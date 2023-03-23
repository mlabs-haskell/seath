module Seath.Test.Fixtures where

import Seath.Network.Types (SeathHandlers(PlutipNetworkHandlers))

fixedTimeOut :: Int
fixedTimeOut = 100

fixedPort :: Int
fixedPort = 22

fixedHandlers :: SeathHandlers
fixedHandlers = PlutipNetworkHandlers {}
