module Seath.Network.Leader
  ( acceptRefuseToSign
  , acceptSignedTransaction
  , actionStatus
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
import Data.UUID (UUID, genUUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, delay, try)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Queue as Queue
import Seath.Core.Types (UserAction)
import Seath.Core.Utils (getFinalizedTransactionHash)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.TxHex as TxHex
import Seath.Network.Types
  ( ActionStatus
      ( NotFound
      , AskForSignature
      , ToBeProcessed
      , PrioritaryToBeProcessed
      , Processing
      , DiscardedBySignRejection
      , WaitingOtherChainSignatures
      )
  , FunctionToPerformContract(FunctionToPerformContract)
  , IncludeActionError(RejectedServerBussy)
  , LeaderConfiguration
  , LeaderNode(LeaderNode)
  , LeaderServerStage(WaitingForActions)
  , LeaderServerStateInfo(LeaderServerInfo)
  , LeaderState
  , LeaderStateInner
  , SendSignedTransaction
  , SignedTransaction
  , getChaintriggerTreshold
  , getFromLeaderConfiguration
  , getFromLeaderState
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
includeAction leaderNode action = do
  let receivedQueue = getFromLeaderState leaderNode _.receivedActionsRequests
  queueResponse <- liftEffect $ Ref.new (Right Nothing)
  _ <- liftEffect $ Queue.put receivedQueue (Right (action /\ queueResponse))
  value <- loop queueResponse
  case value of
    Nothing -> (Left <<< RejectedServerBussy) <$> leaderStateInfo leaderNode
    (Just uuid) -> pure $ Right uuid

  where
  loop :: Ref (Either Unit (Maybe UUID)) -> Aff (Maybe UUID)
  loop queueResponse = do
    delay (wrap 0.001)
    value <- liftEffect $ Ref.read queueResponse
    case value of
      Right v -> pure v
      Left _ -> loop queueResponse

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

  let pendingQueue = getFromLeaderState leaderNode _.pendingActionsRequest
  -- TODO : Doing this for every request is obviously inefficient, we know.
  pendingMap <- liftEffect $ OrderedMap.fromFoldable <$> Queue.read pendingQueue
  pendingCheck <- checkIsIn actionId pendingMap
    (\_ index _ -> pure $ ToBeProcessed index)
  prioritarycheck <- check _.prioritaryPendingActions
    (\_ index _ -> pure $ PrioritaryToBeProcessed index)
  processCheck <- check _.processing (\_ _ _ -> pure $ Processing)

  waitingSignCheck <- check _.waitingForSignature transformForSignature
  let sigResponseQueue = getFromLeaderState leaderNode _.signatureResponses

  signResponses <- liftEffect $
    OrderedMap.fromFoldable <$> Queue.read sigResponseQueue
  signResponseCheck <- checkIsIn actionId signResponses
    (\_ index _ -> pure $ ToBeProcessed index)

  submissionCheck <- checkIsIn actionId signResponses
    ( \_ _ v -> pure case v of
        Left _unit -> DiscardedBySignRejection
        Right _tx -> WaitingOtherChainSignatures
    )

  pure $ foldl mergeChecks submissionCheck
    [ signResponseCheck
    , waitingSignCheck
    , processCheck
    , prioritarycheck
    , pendingCheck
    ]

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
  -> Aff (Either String Unit)
acceptSignedTransaction leaderNode signedTx = do
  let uuid = (unwrap signedTx).uuid
  log $ "Leader accepts Signed Transaction " <> show uuid
  incomingTx <- try $ TxHex.fromCborHex (unwrap signedTx).txCborHex
  case incomingTx of
    Left e -> do
      let msg = "Leader: failed to parse signed Tx: " <> show e
      log msg
      pure $ Left msg
    Right _IncomingTx -> do
      log "Leader received signed tx successfully"
      waitingMap <- getFromRefAtLeaderState leaderNode _.waitingForSignature
      case OrderedMap.lookup uuid waitingMap of
        Just tx -> do
          let
            (FunctionToPerformContract fromContract) =
              getFromLeaderConfiguration leaderNode _.fromContract
          originalHash <- fromContract $ getFinalizedTransactionHash (wrap tx)
          newHash <- fromContract $ getFinalizedTransactionHash (wrap tx)
          if originalHash == newHash then
            do
              let
                requestsQueue = getFromLeaderState leaderNode
                  _.signatureResponses
              requests <- liftEffect $ OrderedMap.fromFoldable <$> Queue.read
                requestsQueue
              case OrderedMap.lookup uuid requests of
                Just _ -> pure $ Left $
                  "Already got a response for this uuid: " <> show uuid
                Nothing -> do
                  liftEffect $ Queue.put requestsQueue (uuid /\ Right tx)
                  pure $ Right $ unit
          else
            pure $ Left $ "The received transaction hash: " <> show newHash
              <> " , isn't the same as the expected one: "
              <> show originalHash

        Nothing -> pure $ Left
          "can't find transactions in the waiting for signature list"

acceptRefuseToSign
  :: forall a
   . LeaderNode a
  -> UUID
  -> Aff (Either String Unit)
acceptRefuseToSign leaderNode uuid = do
  log $ "Leader accepts signing refusal for " <> show uuid
  let requestsQueue = getFromLeaderState leaderNode _.signatureResponses
  waitingMap <- getFromRefAtLeaderState leaderNode _.waitingForSignature
  case OrderedMap.lookup uuid waitingMap of
    Just _ -> do
      requests <- liftEffect $ OrderedMap.fromFoldable <$> Queue.read
        requestsQueue
      case OrderedMap.lookup uuid requests of
        Just _ -> pure $ Left $
          "Already got a response for this uuid: " <> show uuid
        Nothing -> do
          liftEffect $ Queue.put requestsQueue (uuid /\ Left unit)
          pure $ Right $ unit
    Nothing -> pure $ Left
      "can't find transactions in the waiting for signature list"

-- | It's going to wait for the responses of the given `OrderedMap`  until the 
-- | configured timeout is reached.
waitForChainSignatures
  :: forall a
   . LeaderNode a
  -> Aff $ OrderedMap UUID $ Either Unit SignedTransaction
waitForChainSignatures leaderNode = undefined

-- | Submit a Chain of `SignedTransaction`s
submitChain
  :: forall a
   . LeaderNode a
  -> OrderedMap UUID SignedTransaction
  -> Aff $ OrderedMap UUID $ Either String $ TransactionHash
submitChain = undefined

newLeaderNode
  :: forall a
   . Show a
  => LeaderConfiguration a
  -> ( Array (UserAction a)
       -> Aff (Array (FinalizedTransaction /\ UserAction a))
     )
  -> Aff (LeaderNode a)
newLeaderNode conf buildChain' = do
  newState <- newLeaderState (unwrap conf).numberOfActionToTriggerChainBuilder
  let
    node = LeaderNode
      { state: newState
      , configuration: conf
      , buildChain: buildChain'
      }
  pure node

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
  delay (wrap 3000.0)
  -- TODO : implement waitForChainSignatures
  -- signatureResults <- waitForChainSignatures leaderNode 
  -- this must split the map in two, the longest contiguos set of 
  -- actions that where sucessfuly signed and the rest, 
  -- then we put the rest in the prioritary queue (except those with 
  -- rejection for signature) and continue to submission.
  -- let
  --   {success:batchToSubmit, failures:_}= undefined signatureCheck
  log "Leader: Transactions submission"
  log "Leader: Node loop complete!"
  leaderLoop leaderNode

stopLeaderNode :: forall a. LeaderNode a -> Aff Unit
stopLeaderNode = undefined

-- | To build a new `LeaderState`
newLeaderState :: forall a. Int -> Aff $ LeaderState a
newLeaderState maxRequestAllowed = do
  receivedActionsRequests <- liftEffect $ Queue.new
  pendingActionsRequest <- liftEffect $ Queue.new
  receivedActionsRequestsHandler <- liftEffect $ newActionRequestQueueHandler
    maxRequestAllowed
    pendingActionsRequest
  liftEffect $ Queue.on receivedActionsRequests receivedActionsRequestsHandler
  prioritaryPendingActions <- liftEffect $ Ref.new OrderedMap.empty
  processing <- liftEffect $ Ref.new OrderedMap.empty
  waitingForSignature <- liftEffect $ Ref.new OrderedMap.empty
  signatureResponses <- liftEffect $ Queue.new
  waitingForSubmission <- liftEffect $ Ref.new OrderedMap.empty
  stage <- liftEffect $ Ref.new WaitingForActions
  pure $ wrap
    { receivedActionsRequests
    , pendingActionsRequest
    , prioritaryPendingActions
    , processing
    , waitingForSignature
    , signatureResponses
    , waitingForSubmission
    , stage
    }

newActionRequestQueueHandler
  :: forall a
   . Int
  -> Queue.Queue (read :: Queue.READ, write :: Queue.WRITE)
       (UUID /\ UserAction a)
  -> Effect
       ( (Either Int (UserAction a /\ Ref (Either Unit (Maybe UUID))))
         -> Effect Unit
       )
newActionRequestQueueHandler maxCapacity otherQueue = do
  counterRef <- Ref.new 0
  Ref.write 0 counterRef
  pure $ handler counterRef

  where
  handler
    :: Ref Int
    -> (Either Int (UserAction a /\ Ref (Either Unit (Maybe UUID))))
    -> Effect Unit
  handler counterRef (Left takenByLeader) = do
    counterValue <- Ref.read counterRef
    let newCounterValue = max (counterValue - takenByLeader) 0
    Ref.write newCounterValue counterRef
  handler counterRef (Right (action /\ responseRef)) = do
    counterValue <- Ref.read counterRef
    if counterValue > maxCapacity then
      Ref.write (Left unit) responseRef
    else do
      uuid <- genUUID
      liftEffect $ Queue.put otherQueue (uuid /\ action)
      Ref.write (Right (Just uuid)) responseRef

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
