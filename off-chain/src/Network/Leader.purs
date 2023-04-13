module Seath.Network.Leader
  ( acceptRefuseToSign
  , acceptSignedTransaction
  , actionStatus
  , getNextBatchOfActions
  , includeAction
  , newLeaderState
  , showDebugState
  , newLeaderNode
  , leaderLoop
  , stopLeaderNode
  , submitChain
  , waitForChainSignatures
  ) where

import Contract.Prelude

import Contract.Transaction (FinalizedTransaction, Transaction, TransactionHash)
import Data.Array as Array
import Data.Either (Either)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple.Nested (type (/\))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, delay, try)
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
  , LeaderServerStage
      ( WaitingForActions
      , BuildingChain
      , WaitingForChainSignatures
      , SubmittingChain
      )
  , LeaderServerStateInfo(LeaderServerInfo)
  , LeaderState(LeaderState)
  , LeaderStateInner
  , SendSignedTransaction
  , SignedTransaction
  , addAction
  , getChaintriggerTreshold
  , getFromLeaderConfiguration
  , getFromRefAtLeaderState
  , getNumberOfPending
  , maxPendingCapacity
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
  pendingCount <- getNumberOfPending ln
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
  -> OrderedMap UUID Transaction
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

newLeaderNode
  :: forall a
   . Show a
  => LeaderConfiguration a
  -> ( Array (UserAction a)
       -> Aff (Array (FinalizedTransaction /\ UserAction a))
     )
  -> Aff (LeaderNode a)
newLeaderNode conf buildChain = do
  pendingActionsRequest <- liftEffect $ Ref.new OrderedMap.empty
  prioritaryPendingActions <- liftEffect $ Ref.new OrderedMap.empty
  processing <- liftEffect $ Ref.new OrderedMap.empty
  waitingForSignature <- liftEffect $ Ref.new OrderedMap.empty
  waitingForSubmission <- liftEffect $ Ref.new OrderedMap.empty
  stage <- liftEffect $ Ref.new WaitingForActions
  let
    node = LeaderNode
      { state: LeaderState
          { pendingActionsRequest
          , prioritaryPendingActions
          , processing
          , waitingForSignature
          , waitingForSubmission
          , stage: stage
          }
      , configuration: conf
      , buildChain: buildChain
      }
  pure node

-- ! this is early and experimental to see if batching will work correctly
getBatchOfResponses
  :: forall a. Show a => LeaderNode a -> Aff (OrderedMap UUID (UserAction a))
getBatchOfResponses ln = do
  prioritary <- getFromRefAtLeaderState ln _.prioritaryPendingActions
  let
    maxNumberToTake = getFromLeaderConfiguration ln
      _.numberOfActionToTriggerChainBuilder
    numberToTake = maxNumberToTake - OrderedMap.length prioritary
  pending <- takeFromPending numberToTake ln
  setToRefAtLeaderState ln OrderedMap.empty _.prioritaryPendingActions
  pure $ OrderedMap.union prioritary pending

buildChain
  :: forall a
   . LeaderNode a
  -> OrderedMap UUID (UserAction a)
  -> Aff (Either String (OrderedMap UUID Transaction))
buildChain leaderNode toProcess = do
  txChain <- try $ (unwrap leaderNode).buildChain
    (snd <$> OrderedMap.orderedElems toProcess)
  case txChain of
    Left e -> pure $ Left (show e)
    Right chain ->
      let
        txsUids :: Array (UUID /\ Transaction)
        txsUids =
          Array.zip (OrderedMap.orderedKeys toProcess)
            (map (fst >>> unwrap) chain)
      in
        pure $ Right $ OrderedMap.fromFoldable txsUids

waitForRequests :: forall a. LeaderNode a -> Aff Unit
waitForRequests ln = do
  let
    attempts = getFromLeaderConfiguration ln _.maxWaitingTimeBeforeBuildChain
  loop attempts
  where
  loop 0 = pure unit
  loop attempts = do
    let tHold = getChaintriggerTreshold ln
    pendingNum <- getNumberOfPending ln
    if pendingNum >= tHold then pure unit
    else do
      delay (Milliseconds 1000.0)
      loop (attempts - 1)

-- | Put an empty value in every ref of the `LeaderState` 
-- | except for `pendingActionsRequest` and `prioritaryPendingActions`
resetLeaderState :: forall a. LeaderNode a -> Aff Unit
resetLeaderState leaderNode = do
  setToRefAtLeaderState leaderNode OrderedMap.empty _.processing
  setToRefAtLeaderState leaderNode OrderedMap.empty _.waitingForSignature
  setToRefAtLeaderState leaderNode OrderedMap.empty _.waitingForSubmission
  setToRefAtLeaderState leaderNode WaitingForActions _.stage

leaderLoop
  :: forall actionType. Show actionType => LeaderNode actionType -> Aff Unit
leaderLoop leaderNode = do
  log "Leader: New leader loop begins"
  resetLeaderState leaderNode
  delay (wrap 1000.00)
  log "Leader: Waiting for request of actions"
  waitForRequests leaderNode
  log "Leader: Taking batch to process"
  batchToProcess <- getBatchOfResponses leaderNode
  setToRefAtLeaderState leaderNode batchToProcess _.processing
  log $ "Leader: Batch to process: " <> show batchToProcess
  log "Leader: Begin to process actions"
  eitherBuiltChain <- buildChain leaderNode batchToProcess
  case eitherBuiltChain of
    Left e -> do
      log $ "Leader: fail to built chain of size "
        <> show (OrderedMap.length batchToProcess)
        <> " "
        <> e
      -- TODO : We are discarting all the processing actions if the builder 
      -- fails, we can't add them to the prioritaryPendingActions since 
      -- the builder either sucess in all the chain or fails.
      leaderLoop leaderNode
    Right builtChain -> do
      setToRefAtLeaderState leaderNode builtChain _.waitingForSignature
  batchToSign <- getFromRefAtLeaderState leaderNode _.waitingForSignature
  log $ "Leader: builder result: " <> show batchToSign
  log "Leader: Waiting for signatures to arrive"
  -- TODO : implement waitForChainSignatures
  -- signatureResults <- waitForChainSignatures leaderNode batchToSign
  -- this must split the map in two, the longest contiguos set of 
  -- actions that where sucessfuly signed and the rest, 
  -- then we put the rest in the prioritary queue (except those with 
  -- rejection for signature) and continue to submission.
  -- let
  --   {success:batchToSubmit, failures:_}= undefined signatureCheck
  log "Leader: Transactions submission"
  log "Leader: Node loop complete!"
  leaderLoop leaderNode

----------------  setToRefAtLeaderState leaderNode Blog $ "Leader: failed to build chain: " <> show euildingChain _.stage

stopLeaderNode :: forall a. LeaderNode a -> Aff Unit
stopLeaderNode = undefined

-- | To build a new mutable `LeaderState`
newLeaderState :: forall a. Aff $ LeaderState a
newLeaderState = undefined

showDebugState :: forall a. LeaderNode a -> Aff String
showDebugState leaderNode = do
  pending <- getNumberOfPending leaderNode
  pure $ "\nLeader debug state:"
    <> "\n Num of pending actions: "
    <> show pending

leaderStateInfo :: forall a. LeaderNode a -> Aff LeaderServerStateInfo
leaderStateInfo ln@(LeaderNode node) = do
  numberOfActionsToProcess <- getNumberOfPending ln
  let maxNumberOfPendingActions = maxPendingCapacity node.configuration
  let maxTimeOutForSignature = signTimeout node.configuration
  serverStage <- getFromRefAtLeaderState ln _.stage
  pure $ LeaderServerInfo
    { numberOfActionsToProcess
    , maxNumberOfPendingActions
    , maxTimeOutForSignature
    , serverStage
    }
