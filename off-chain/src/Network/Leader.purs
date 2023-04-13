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

import Contract.Transaction
  ( FinalizedTransaction
  , Transaction(..)
  , TransactionHash
  )
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Semigroup.Foldable (foldl1Default)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple.Nested (type (/\))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, delay, error, forkAff, throwError, try)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.TxHex as TxHex
import Seath.Network.Types
  ( ActionStatus(..)
  , IncludeActionError(RejectedServerBussy)
  , LeaderConfiguration
  , LeaderNode(LeaderNode)
  , LeaderServerStage(WaitingForActions)
  , LeaderServerStateInfo(LeaderServerInfo)
  , LeaderState(LeaderState)
  , LeaderStateInner
  , SendSignedTransaction
  , SignedTransaction
  , addAction
  , chaintriggerTreshold
  , getFromRefAtLeaderState
  , maxPendingCapacity
  , numberOfPending
  , setToRefAtLeaderState
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
  let
    check
      :: forall b
       . (LeaderStateInner a -> Ref (OrderedMap UUID b))
      -> (UUID -> Int -> b -> Aff ActionStatus)
      -> Aff ActionStatus
    check getter transform = do
      _map <- getFromRefAtLeaderState leaderNode getter
      checkIsIn actionId _map transform

  pendingCheck <- check _.pendingActionsRequest
    (\_ index _ -> pure $ ToBeProcessed index)
  prioritarycheck <- check _.prioritaryPendingActions
    (\_ index _ -> pure $ PrioritaryToBeProcessed index)
  processCheck <- check _.processing (\_ _ _ -> pure $ Processing)
  signatureCheck <- check _.waitingForSignature transformForSignature
  submissionCheck <- check _.waitingForSubmission
    (\_ index _ -> pure $ ToBeSubmitted index)

  pure $ foldl mergeChecks submissionCheck
    [ signatureCheck, processCheck, prioritarycheck, pendingCheck ]

  where
  mergeChecks :: ActionStatus -> ActionStatus -> ActionStatus
  mergeChecks NotFound second = second
  mergeChecks other _ = other

  -- TODO : We can make everyting return `() -> ActionStatus` to recover lazy
  -- behaviour but we still don't know if is worth the effort
  checkIsIn
    :: forall b
     . UUID
    -> OrderedMap UUID b
    -> (UUID -> Int -> b -> Aff ActionStatus)
    -> Aff (ActionStatus)
  checkIsIn uuid _map transform =
    case OrderedMap.lookupWithPosition uuid _map of
      Just (index /\ value) -> transform uuid index value
      Nothing -> pure $ NotFound

  transformForSignature :: UUID -> Int -> Transaction -> Aff ActionStatus
  transformForSignature uuid _ tx = do
    eitherTxCborHex <- try $ TxHex.toCborHex tx
    case eitherTxCborHex of
      Left e -> liftEffect $ throw $ "cbor encoding error: " <> show e
      Right txCborHex -> pure $ AskForSignature { uuid, txCborHex }

acceptSignedTransaction
  :: forall a
   . LeaderNode a
  -> SendSignedTransaction
  -> Aff Unit
acceptSignedTransaction _leaderNode signedTx = do
  log $ "Leader accepts Signed Transaction " <> show
    (unwrap signedTx).uuid
  tx <- try $ TxHex.fromCborHex (unwrap signedTx).txCborHex
  case tx of
    Left e -> log $ "Leader: failed to parse signed Tx: " <> show e
    Right _tx -> log "Leader received signed tx successfully"
  pure unit

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
  :: forall a
   . Show a
  => LeaderConfiguration a
  -> ( Array (UserAction a)
       -> Aff (Array (FinalizedTransaction /\ UserAction a))
     )
  -> Aff (LeaderNode a)
startLeaderNode conf buildChain = do
  pendingActionsRequest <- liftEffect $ Ref.new OrderedMap.empty
  prioritaryPendingActions <- liftEffect $ Ref.new OrderedMap.empty
  processing <- liftEffect $ Ref.new OrderedMap.empty
  waitingForSignature <- liftEffect $ Ref.new OrderedMap.empty
  waitingForSubmission <- liftEffect $ Ref.new OrderedMap.empty
  let
    node = LeaderNode
      { state: LeaderState
          { pendingActionsRequest
          , prioritaryPendingActions
          , processing
          , waitingForSignature
          , waitingForSubmission
          , stage: WaitingForActions
          }
      , configuration: conf
      , buildChain: buildChain
      }

  -- ! this is early and experimental to see if batching will work correctly
  startBatcherThread node

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
      txChain <- try $ (unwrap ln).buildChain
        (snd <$> OrderedMap.orderedElems toProcess)
      case txChain of
        Left e
        -> log $ "Leader: failed to build chain: " <> show e
        Right chain
        -> do
          let
            signedTxsUids :: Array (UUID /\ Transaction)
            signedTxsUids =
              Array.zip (OrderedMap.orderedKeys toProcess)
                (map (fst >>> unwrap) chain)
          setToRefAtLeaderState ln
            (OrderedMap.fromFoldable signedTxsUids)
            _.waitingForSignature
          log $ "Leader: built chain of size " <> show (Array.length chain)

    else pure unit
    delay (Milliseconds 1000.0)
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
