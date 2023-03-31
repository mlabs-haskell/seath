module Seath.Test.QuickCheck where

import Contract.Address (AddressWithNetworkTag, PubKeyHash)
import Control.Alternative (pure)
import Control.Monad (bind)
import Data.Array (replicate)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Newtype (wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Seath.Network.Types
  ( LeaderConfiguration
  , LeaderNode(LeaderNode)
  , LeaderState
  , UserConfiguration
  , UserNode(UserNode)
  , UserState
  )
import Seath.Test.Examples.Addition.Types (AdditionAction)
import Seath.Test.Fixtures
  ( fixedPort
  , fixedTimeOut
  )
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)
import Type.Function (type ($))
import Undefined (undefined)

genLeaderConfiguration :: Gen LeaderConfiguration
genLeaderConfiguration = undefined

-- do
-- maxWaitingTimeForSignature :: MiliSeconds <- arbitrary
-- maxQueueSize :: Int <- arbitrary
-- numberOfActionToTriggerChainBuilder :: Int <- arbitrary
-- maxWaitingTimeBeforeBuildChain :: Int <- chooseInt 10 100
-- pure $ LeaderConfiguration{maxWaitingTimeForSignature, maxQueueSize,numberOfActionToTriggerChainBuilder,maxWaitingTimeBeforeBuildChain, clientHandlers:fixedLeaderClientHandlers}

genUserConfiguration :: Gen $ UserConfiguration AdditionAction
genUserConfiguration = undefined

genUserNodeWith :: UserState AdditionAction -> Gen $ UserNode AdditionAction
genUserNodeWith = undefined

genLeaderNodeWith
  :: LeaderState AdditionAction -> Gen $ LeaderNode AdditionAction
genLeaderNodeWith = undefined

type Distribution = ((Array BigInt /\ Array BigInt) /\ Array (Array BigInt))

makeDistribution :: Int -> Distribution
makeDistribution participantsNumber =
  let
    adminDistribution = [ BigInt.fromInt 1_000_000_000 ]
    leaderDistribution = [ BigInt.fromInt 1_000_000_000 ]
    usersDistribution = replicate participantsNumber
      [ BigInt.fromInt 1_000_000_000 ]
  in
    (adminDistribution /\ leaderDistribution) /\ usersDistribution

genDistribution :: Gen Distribution
genDistribution = makeDistribution <$> chooseInt 2 100
