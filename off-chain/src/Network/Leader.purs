module Seath.Network.Leader
  ( acceptRefuseToSign
  , acceptSignedTransaction
  , actionStatus
  , getNextBatchOfActions
  , includeAction
  , newLeaderState
  , showDebugState
  , startLeaderNode
  , stopLeaderNode
  , submitChain
  , waitForChainSignatures
  ) where

import Contract.Prelude

import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Data.Either (Either)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple.Nested (type (/\))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, delay, forkAff)
import Effect.Ref as Ref
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.Types
  ( ActionStatus
      ( NotFound
      , ToBeProcessed
      , ToBeSubmited
      , NotFound
      , Processing
      , RejectedAtChainBuilder
      )
  , IncludeActionError(RejectedServerBussy)
  , LeaderConfiguration
  , LeaderNode(LeaderNode)
  , LeaderServerStage(WaitingForActions)
  , LeaderServerStateInfo(LeaderServerInfo)
  , LeaderState(LeaderState)
  , SendSignedTransaction
  , SignedTransaction
  , addAction
  , chaintriggerTreshold
  , getFromRefAtLeaderState
  , maxPendingCapacity
  , numberOfPending
  , signTimeout
  , takeFromPending
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
  pendingCount <- numberOfPending ln
  if (pendingCount < maxPendingCapacity node.configuration) then
    (Right <$> addAction action node.state)
  else (Left <<< RejectedServerBussy) <$> leaderStateInfo ln

actionStatus :: forall a. LeaderNode a -> UUID -> Aff ActionStatus
actionStatus leaderNode actionId = do
  pending <- getFromRefAtLeaderState leaderNode _.pendingActionsRequest

  let maybeInPending = lookupPosition pending
  -- todo: check in other OrderedMaps and get correct status
  pure $ case maybeInPending of
    Just i -> ToBeProcessed i
    Nothing -> NotFound
  where
  lookupPosition = OrderedMap.lookupPosition actionId

acceptSignedTransaction
  :: forall a
   . LeaderNode a
  -> SendSignedTransaction
  -> Aff Unit
acceptSignedTransaction _leaderNode signedTx = do
  log $ "Leader accepts Signed Transaction " <> show
    (unwrap signedTx).uuid

acceptRefuseToSign
  :: forall a
   . LeaderNode a
  -> UUID
  -> Aff Unit
acceptRefuseToSign leaderNode uuid = do
  log $ "Leader accepts signing refusal for " <> show uuid

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

startLeaderNode
  :: forall a. Show a => LeaderConfiguration a -> Aff (LeaderNode a)
startLeaderNode conf = do
  pendingActionsRequest <- liftEffect $ Ref.new OrderedMap.empty
  prioritaryPendingActions <- liftEffect $ Ref.new OrderedMap.empty
  processing <- liftEffect $ Ref.new OrderedMap.empty
  rejectedByChainBuilder <- liftEffect $ Ref.new OrderedMap.empty
  waitingForSignature <- liftEffect $ Ref.new OrderedMap.empty
  waitingForSubmission <- liftEffect $ Ref.new OrderedMap.empty
  errorAtSubmission <- liftEffect $ Ref.new OrderedMap.empty
  let
    node = LeaderNode
      { state: LeaderState
          { pendingActionsRequest
          , prioritaryPendingActions
          , processing
          , rejectedByChainBuilder
          , waitingForSignature
          , waitingForSubmission
          , errorAtSubmission
          , stage: WaitingForActions
          }
      , configuration: conf
      }

  -- ! this is early and experimental to see if batching will work correctly
  -- ! uncomment if needed
  -- startBatcherThread node

  pure node

-- ! this is early and experimental to see if batching will work correctly
startBatcherThread :: forall a. Show a => LeaderNode a -> Aff Unit
startBatcherThread ln = do
  void $ forkAff loop
  where
  loop = do
    let tHold = chaintriggerTreshold ln
    pendingNum <- numberOfPending ln
    if pendingNum >= tHold then do
      toProcess <- takeFromPending tHold ln
      log $ "------> To process: " <> show (OrderedMap.orderedElems toProcess)
    else pure unit
    delay (Milliseconds 3000.0)
    loop

stopLeaderNode :: forall a. LeaderNode a -> Aff Unit
stopLeaderNode = undefined

-- | To build a new mutable `LeaderState`
newLeaderState :: forall a. Aff $ LeaderState a
newLeaderState = undefined

showDebugState :: forall a. LeaderNode a -> Aff String
showDebugState leaderNode = do
  pending <- numberOfPending leaderNode
  pure $ "\nLeader debug state:"
    <> "\n Num of pending actions: "
    <> show pending

-- TODO: partially mocked
leaderStateInfo :: forall a. LeaderNode a -> Aff LeaderServerStateInfo
leaderStateInfo ln@(LeaderNode node) = do
  numberOfActionsToProcess <- numberOfPending ln
  let maxNumberOfPendingActions = maxPendingCapacity node.configuration
  let maxTimeOutForSignature = signTimeout node.configuration
  let serverStage = WaitingForActions -- TODO: get the real one
  pure $ LeaderServerInfo
    { numberOfActionsToProcess
    , maxNumberOfPendingActions
    , maxTimeOutForSignature
    , serverStage
    }
