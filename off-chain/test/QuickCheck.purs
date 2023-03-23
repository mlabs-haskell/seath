module Seath.Test.QuickCheck where

import Contract.Address (PubKeyHash)
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
  ( Ip
  , Node(NodeUser, NodeLeader)
  , NodeConfiguration(NodeConfiguration)
  )
import Seath.Test.Fixtures (fixedHandlers, fixedPort, fixedTimeOut)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)

makeNodeConfiguration :: PubKeyHash -> NodeConfiguration
makeNodeConfiguration pk = NodeConfiguration
  { timeout: fixedTimeOut
  , handlers: fixedHandlers
  , port: fixedPort
  , pubKeyHash: pk
  }

genUserNodeWith :: Ip -> NodeConfiguration -> Gen Node
genUserNodeWith ip configuration = do
  state <- arbitrary
  pure $ NodeUser { configuration, information: wrap { ip }, state }

genLeaderNodeWith :: Ip -> NodeConfiguration -> Gen Node
genLeaderNodeWith ip configuration = do
  state <- arbitrary
  pure $ NodeLeader { configuration, information: wrap { ip }, state }

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
