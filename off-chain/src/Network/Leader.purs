module Seath.Network.Leader
  ( actionStatus
  , getNextBatchOfActions
  , includeAction
  , newLeaderState
  , showDebugState
  , startLeaderNode
  , startLeaderServer
  , stopLeaderNode
  , stopLeaderServer
  , submitChain
  , waitForChainSignatures
  ) where

import Contract.Prelude

import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OMap
import Seath.Network.Types
  ( ActionStatus(ToBeProcessed, NotFound)
  , IncludeActionError(RejectedServerBussy)
  , LeaderConfiguration
  , LeaderNode(LeaderNode)
  , LeaderServerStage(WaitingForActions)
  , LeaderServerStateInfo(LeaderServerInfo)
  , LeaderState(LeaderState)
  , SignedTransaction
  , addAction
  , getPending
  , maxPendingCapacity
  , numberOfPending
  , signTimeout
  )
import Type.Function (type ($))
import Undefined (undefined)

includeAction
  :: forall a
   . LeaderNode a
  -> UserAction a
  -> Aff (Either IncludeActionError UUID)
includeAction ln@(LeaderNode node) action = do
  liftEffect $ log "Leader: accepting action"
  pendingCount <- numberOfPending node.state
  if (pendingCount < maxPendingCapacity node.configuration) then
    -- if (pendingCount > maxPendingCapacity node.configuration) then -- DEBUG
    (Right <$> addAction action node.state)
  else (Left <<< RejectedServerBussy) <$> leaderStateInfo ln

actionStatus :: forall a. LeaderNode a -> UUID -> Aff ActionStatus
actionStatus leaderNode actionId = do
  pending <- getPending leaderNode
  let maybeInPending = OMap.lookupPostion actionId pending
  -- todo: check in other OMaps and get correct status
  pure $ case maybeInPending of
    Just i -> ToBeProcessed i
    Nothing -> NotFound

-- | It's going to wait for the responses of the given `OrderedMap`  until the 
-- | configured timeout is reached.
waitForChainSignatures
  :: forall a
   . LeaderNode a
  -> OrderedMap UUID FinalizedTransaction
  -> Aff $ OrderedMap UUID $ Either String SignedTransaction
waitForChainSignatures = undefined

-- | Submit a Chain of `SignedTransaction`s
submitChain
  :: forall a
   . LeaderNode a
  -> OrderedMap UUID SignedTransaction
  -> Aff $ OrderedMap UUID $ Either String $ TransactionHash
submitChain = undefined

-- To use inside getNextBatchOfActions
getAbatchOfPendingActions
  :: forall a
   . LeaderNode a
  -> Int
  -> Aff $ Array $ UUID /\ UserAction a
getAbatchOfPendingActions = undefined

-- The array it takes is the output of tellUsersWeNeedNewSignature.
getNextBatchOfActions
  :: forall a
   . LeaderNode a
  -> OrderedMap UUID $ UserAction a
  -> Aff $ OrderedMap UUID $ UserAction a
getNextBatchOfActions = undefined

-- (getAbatchOfPendingActions undefined undefined)

startLeaderNode :: forall a. LeaderConfiguration a -> Aff (LeaderNode a)
startLeaderNode conf = do
  pending <- liftEffect $ Ref.new OMap.empty
  pure $ LeaderNode
    { state: LeaderState
        { pendingActionsRequest: pending
        , prioritaryPendingActions: undefined
        , signatureResponses: undefined
        , stage: undefined
        , numberOfActionsRequestsMade: undefined

        }
    , configuration: conf
    }

stopLeaderNode :: forall a. LeaderNode a -> Aff Unit
stopLeaderNode = undefined

-- TODO: left to not to break compilation
startLeaderServer :: forall a. LeaderNode a -> Aff Unit
startLeaderServer = undefined

stopLeaderServer :: forall a. LeaderNode a -> Aff Unit
stopLeaderServer = undefined

-- TODO: left to not to break compilation - END

-- | To build a new mutable `LeaderState`
newLeaderState :: forall a. Aff $ LeaderState a
newLeaderState = undefined

showDebugState :: forall a. LeaderNode a -> Aff String
showDebugState leaderNode = do
  let state = (unwrap leaderNode).state
  pending <- numberOfPending state
  pure $ "\nLeader debug state:"
    <> "\n Num of pending actions: "
    <> show pending

-- TODO: partially mocked
leaderStateInfo :: forall a. LeaderNode a -> Aff LeaderServerStateInfo
leaderStateInfo (LeaderNode node) = do
  numberOfActionsToProcess <- numberOfPending node.state
  let maxNumberOfPendingActions = maxPendingCapacity node.configuration
  let maxTimeOutForSignature = signTimeout node.configuration
  let serverStage = WaitingForActions -- TODO: get the real one
  pure $ LeaderServerInfo
    { numberOfActionsToProcess
    , maxNumberOfPendingActions
    , maxTimeOutForSignature
    , serverStage
    }