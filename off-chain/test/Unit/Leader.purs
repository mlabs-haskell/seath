module Seath.Test.Unit.Leader where

import Contract.Prelude

import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip (PlutipTest, withKeyWallet, withWallets)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Reader (ask)
import Data.Array ((!!))
import Data.BigInt (fromInt) as BigInt
import Data.UUID (genUUID)
import Effect.Aff (error)
import Effect.Ref as Ref
import Mote (group, test)
import Queue as Queue
import Seath.Core.Utils as Core.Utils
import Seath.Network.Leader as Leader
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.Types
  ( LeaderConfiguration(LeaderConfiguration)
  , RunContract(RunContract)
  )
import Seath.Network.Utils (getFromLeaderState, setToRefAtLeaderState)
import Seath.Test.Examples.Addition.Types (AdditionAction(AddAmount))
import Seath.Test.Utils (Distribution, makeDistribution)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Undefined (undefined)

testLeaderConfig :: LeaderConfiguration AdditionAction
testLeaderConfig =
  LeaderConfiguration
    { maxWaitingTimeForSignature: 5000
    , maxQueueSize: 1
    , numberOfActionToTriggerChainBuilder: 3
    , maxWaitingTimeBeforeBuildChain: 5000
    , runContract: RunContract undefined -- not used in this test case scenario
    , buildChain: \_ -> undefined -- not used in this test case scenario
    }

suite :: TestPlanM PlutipTest Unit
suite = do
  group "Leader" do
    test "Do not accept 2nd action" testDuplicateUserAddition

{- Do not accept 2nd action from same user if there is already
   action in procass form this user
-}
testDuplicateUserAddition ∷ PlutipTest
testDuplicateUserAddition = do
  let
    distribution :: Distribution
    distribution = makeDistribution 1
  withWallets distribution \((_admin /\ _leader) /\ users) -> do
    _env <- ask
    user1 <- liftMaybe (error "No user wallet") (users !! 0)
    let
      newNode = liftAff
        $ Leader.newLeaderNode testLeaderConfig

    node1 <- newNode

    testAction1 <- withKeyWallet user1 do
      Core.Utils.makeActionContract (AddAmount $ BigInt.fromInt 1)
    testAction2 <- withKeyWallet user1 do
      Core.Utils.makeActionContract (AddAmount $ BigInt.fromInt 11)

    -- check adding actions "intended way" through main mailbox queue
    result1 <- liftAff $ node1 `Leader.includeAction` testAction1
    result1 `shouldSatisfy` isRight
    result2 <- liftAff $ node1 `Leader.includeAction` testAction2
    result2 `shouldSatisfy` isLeft

    let
      testThroughRefInState' =
        testThroughRefInState newNode testAction1 testAction2

    -- check through adding actions directly to node state
    testThroughPendingQueue newNode testAction1 testAction2
    testThroughRefInState' _.prioritaryPendingActions
    testThroughRefInState' _.processing

  where

  testThroughPendingQueue newNode testAction1 testAction2 = do
    node <- newNode
    uuid <- liftEffect genUUID
    let pendingQueue = node `getFromLeaderState` _.pendingActionsRequest
    node2Pending <- liftEffect $ Queue.read pendingQueue
    node2Pending `shouldEqual` []
    liftEffect $ Queue.put pendingQueue (uuid /\ testAction1)
    addResult <- liftAff $ node `Leader.includeAction` testAction2
    addResult `shouldSatisfy` isLeft

  testThroughRefInState newNode testAction1 testAction2 mapRefInState = do
    node <- newNode
    uuid <- liftEffect genUUID
    orderedMap <- liftEffect $ Ref.read
      (node `getFromLeaderState` mapRefInState)
    orderedMap `shouldEqual` OrderedMap.empty
    liftAff $ setToRefAtLeaderState node
      (OrderedMap.fromFoldable [ uuid /\ testAction1 ])
      mapRefInState
    addResult <- liftAff $ node `Leader.includeAction` testAction2
    addResult `shouldSatisfy` isLeft
