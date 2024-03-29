module Seath.Network.Leader
  ( acceptRefuseToSign
  , acceptSignedTransaction
  , actionStatus
  , includeAction
  , leaderLoop
  , newLeaderNode
  , newLeaderState
  , showDebugState
  , startLeaderNode
  , stopLeaderNode
  , waitForChainSignatures
  ) where

import Contract.Prelude

import Aeson (encodeAeson)
import Contract.Monad (Contract)
import Contract.Transaction
  ( FinalizedTransaction
  , Transaction
  , TransactionHash
  , awaitTxConfirmed
  , signTransaction
  , submit
  )
import Control.Monad.Except (ExceptT, lift, runExceptT)
import Control.Monad.State (StateT, modify_, runStateT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int (toNumber)
import Data.Tuple.Nested (type (/\))
import Data.UUID (UUID, genUUID)
import Effect.Aff
  ( Aff
  , delay
  , error
  , forkAff
  , killFiber
  , message
  , throwError
  , try
  )
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Queue as Queue
import Seath.Core.Types (UserAction)
import Seath.Core.Utils (getFinalizedTransactionHash)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.TxHex as TxHex
import Seath.Network.Types
  ( ActionStatus
      ( AskForSignature
      , ToBeProcessed
      , ToBeSubmitted
      , Processing
      , WaitingOtherChainSignatures
      , PrioritaryToBeProcessed
      , Submitted
      , SubmissionFailed
      , NotFound
      )
  , IncludeActionError(RejectedServerBussy)
  , LeaderConfiguration
  , LeaderNode(LeaderNode)
  , LeaderServerInfo(LeaderServerInfo)
  , LeaderServerStage
      ( WaitingForActions
      , BuildingChain
      , WaitingForChainSignatures
      , SubmittingChain
      )
  , LeaderState
  , LeaderStateInner
  , MilliSeconds
  , RunContract(RunContract)
  , SendSignedTransaction
  )
import Seath.Network.Utils
  ( actionToTriggerChainBuild
  , getChaintriggerTreshold
  , getFromLeaderConfiguration
  , getFromLeaderState
  , getFromRefAtLeaderState
  , getNumberOfPending
  , isAnotherActionInProcess
  , setToRefAtLeaderState
  , signTimeout
  , takeFromPending
  )
import Type.Function (type ($))

startLeaderNode
  :: forall a
   . Show a
  => LeaderConfiguration a
  -> Aff (LeaderNode a)
startLeaderNode conf = do
  node <- newLeaderNode conf
  fiber <- forkAff $ leaderLoop node
  liftEffect $ Ref.modify_ (Array.cons fiber) (unwrap node)._leaderFibers
  pure node

stopLeaderNode
  :: forall a
   . LeaderNode a
  -> Aff Unit
stopLeaderNode node = do
  fibers <- liftEffect $ Ref.read (unwrap node)._leaderFibers
  for_ fibers $
    \fiber -> killFiber (error "Failed to cleanup leader fibers") fiber

newLeaderNode
  :: forall a
   . Show a
  => LeaderConfiguration a
  -> Aff (LeaderNode a)
newLeaderNode conf = do
  newState <- newLeaderState (unwrap conf).numberOfActionToTriggerChainBuilder
  fibers <- liftEffect $ Ref.new []
  let
    node = LeaderNode
      { state: newState
      , configuration: conf
      , _leaderFibers: fibers
      }
  pure node

leaderLoop
  :: forall actionType. Show actionType => LeaderNode actionType -> Aff Unit
leaderLoop leaderNode = do
  log "Leader: New leader loop begins"
  setStage WaitingForActions
  waitForRequests leaderNode
  log "Leader: Taking batch to process"
  batchToProcess <- getBatchOfResponses leaderNode
  setToRefAtLeaderState leaderNode batchToProcess _.processing
  log $ "Leader: Batch to process: " <> showUUIDs batchToProcess
  setStage BuildingChain
  eitherBuiltChain <- buildChain leaderNode batchToProcess
  case eitherBuiltChain of
    Left e -> do
      log $ "Leader: Fail to built chain of size "
        <> show (OrderedMap.length batchToProcess)
        <> " "
        <> e
      -- TODO : We are discarting all the processing actions if the builder 
      -- fails, we can't add them to the prioritaryPendingActions since 
      -- the builder either sucess in all the chain or fails.
      leaderLoop leaderNode
    Right builtChain -> do
      setToRefAtLeaderState leaderNode builtChain _.waitingForSignature
  -- uncomment next 2 lines to log full chain (there will be a lot of output)
  -- getFromRefAtLeaderState leaderNode _.waitingForSignature
  --   >>= log <<< ("Leader: builder result: " <> _) <<< show
  setStage WaitingForChainSignatures
  signatureResults <- waitForChainSignatures leaderNode
  let
    { sucess: signatureSucess, failures: signatureFailures } = purgeResponses
      batchToProcess
      ( ( OrderedMap.fromFoldable <<< ((<$>) (\(k /\ v) -> (k /\ note unit v)))
            <<< OrderedMap.toArray
        ) signatureResults
      )
  log $ "Leader: Successfully signed by users: " <> showUUIDs signatureSucess
  log $ "Leader: For priority list (after signing): " <> showUUIDs
    signatureFailures
  log $ "Leader: Setting failures and submission list"
  setToRefAtLeaderState leaderNode signatureFailures _.prioritaryPendingActions
  setToRefAtLeaderState leaderNode signatureSucess _.waitingForSubmission
  setStage SubmittingChain
  { succeeded: submissionSucess
  , failedOne: failedAction
  , toRetry: priorityRetry
  } <- submitChain leaderNode batchToProcess signatureSucess
  -- } <- submitChainBr leaderNode batchToProcess signatureSucess
  log $ "Leader: Successfully submitted: " <> showUUIDs submissionSucess
  log $ "Leader: Failed chain breaking action: " <> show failedAction
  log $ "Leader: For priority list (after submitting): " <> showUUIDs
    priorityRetry
  let newPrioritary = OrderedMap.union signatureFailures priorityRetry
  log $ "Leader: Setting prioritary list. Total: " <> show
    (OrderedMap.length newPrioritary)
  setToRefAtLeaderState leaderNode newPrioritary _.prioritaryPendingActions
  resetLeaderState leaderNode
  -- To set this matters as we are cleaning up submissions in `resetLeaderNode`
  setToRefAtLeaderState leaderNode submissionSucess _.submitted
  maybeAddToFailed failedAction
  log $ "Leader: Awaiting chain confirmed"
  awaitChainConfirmed leaderNode submissionSucess
  log "Leader: Chain loop complete!"
  leaderLoop leaderNode

  where

  maybeAddToFailed maybeFailedAction =
    maybe
      (pure unit)
      ( \failed -> setToRefAtLeaderState leaderNode
          (OrderedMap.singleton failed)
          _.submissionFailed
      )
      maybeFailedAction

  setStage :: LeaderServerStage -> Aff Unit
  setStage stage = do
    log $ "Leader: Beginning stage " <> show stage
    setToRefAtLeaderState leaderNode stage _.stage

  showUUIDs :: forall b. OrderedMap UUID b -> String
  showUUIDs _map = show $ OrderedMap.orderedKeys _map

includeAction
  :: forall a
   . LeaderNode a
  -> UserAction a
  -> Aff (Either IncludeActionError UUID)
includeAction leaderNode action = do
  inProcess <- isAnotherActionInProcess leaderNode action
  if inProcess then (Left <<< RejectedServerBussy) <$> leaderStateInfo
    leaderNode
  else do
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
  let signResponseQueue = getFromLeaderState leaderNode _.signatureResponses

  signResponses <- liftEffect $
    OrderedMap.fromFoldable <$> Queue.read signResponseQueue
  signResponseCheck <- checkIsIn actionId signResponses
    transfromForSignResponses

  waitingSubmissionCheck <- check _.waitingForSubmission
    (\_ ind _ -> pure $ ToBeSubmitted ind)
  submittedCheck <- check _.submitted (\_ _ txH -> pure $ Submitted txH)
  submissionFailedCheck <- check _.submissionFailed
    (\_ _ err -> pure $ SubmissionFailed err)

  pure $ foldl mergeChecks submittedCheck
    [ waitingSubmissionCheck
    , submissionFailedCheck
    , signResponseCheck
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

  transfromForSignResponses
    :: UUID -> Int -> Maybe Transaction -> Aff ActionStatus
  transfromForSignResponses _ _ (Just tx) = do
    let
      (RunContract runContract) = getFromLeaderConfiguration
        leaderNode
        _.runContract
    WaitingOtherChainSignatures <<< Just <$>
      (runContract <<< getFinalizedTransactionHash <<< wrap) tx
  transfromForSignResponses _ _ Nothing = pure $ NotFound

-- `waitingForSignature` relies on this function and `acceptRefuseToSign`
-- to be the only ones that append things to `signatureResponses`
acceptSignedTransaction
  :: forall a
   . LeaderNode a
  -> SendSignedTransaction
  -> Aff (Either String Unit)
acceptSignedTransaction leaderNode signedTx = do
  let uuid = (unwrap signedTx).uuid
  log $ "Leader: Accepting Signed Transaction " <> show uuid
  incomingTx <- try $ TxHex.fromCborHex (unwrap signedTx).txCborHex
  case incomingTx of
    Left e -> do
      let msg = "Leader: failed to parse signed Tx: " <> show e
      log msg
      pure $ Left msg
    Right receivedSignedTx -> do
      log "Leader: Received signed tx successfully"
      waitingMap <- getFromRefAtLeaderState leaderNode _.waitingForSignature
      case OrderedMap.lookup uuid waitingMap of
        Just tx -> do
          let
            (RunContract runContract) =
              getFromLeaderConfiguration leaderNode _.runContract
          originalHash <- runContract $ getFinalizedTransactionHash (wrap tx)
          hashOfSigned <- runContract $ getFinalizedTransactionHash
            (wrap receivedSignedTx)
          if originalHash == hashOfSigned then
            do
              let
                sigResponsesQueue = getFromLeaderState leaderNode
                  _.signatureResponses
              requests <- liftEffect $ OrderedMap.fromFoldable <$> Queue.read
                sigResponsesQueue
              case OrderedMap.lookup uuid requests of
                Just _ -> pure $ Left $
                  "Already got a response for this uuid: " <> show uuid
                Nothing -> do
                  liftEffect $ Queue.put sigResponsesQueue
                    (uuid /\ Just receivedSignedTx)
                  pure $ Right $ unit
          else
            pure $ Left $ "The received transaction hash: " <> show hashOfSigned
              <> " , isn't the same as the expected one: "
              <> show originalHash

        Nothing -> pure $ Left
          "can't find transactions in the waiting for signature list"

-- `waitingForSignature` relies on this function and `acceptSignedTransaction`
-- to be the only ones that append things to `signatureResponses`
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
          liftEffect $ Queue.put requestsQueue (uuid /\ Nothing)
          pure $ Right $ unit
    Nothing -> pure $ Left
      "can't find transactions in the waiting for signature list"

-- | It's going to wait for the responses of the given `OrderedMap`  until the 
-- | configured timeout is reached.split
waitForChainSignatures
  :: forall a
   . LeaderNode a
  -> Aff $ OrderedMap UUID $ Maybe Transaction
waitForChainSignatures leaderNode = do
  mapOfTransactions <- getFromRefAtLeaderState leaderNode _.waitingForSignature
  let
    maxWaitingTime = getFromLeaderConfiguration leaderNode
      _.maxWaitingTimeForSignature
    numberOfTransactions = OrderedMap.length mapOfTransactions

  loop maxWaitingTime 0 numberOfTransactions

  where
  loop
    :: MilliSeconds
    -> MilliSeconds
    -> Int
    -> Aff $ OrderedMap UUID $ Maybe Transaction
  loop maxWaitingTime waitedTime numberOfTransactions =
    if waitedTime >= maxWaitingTime then
      endAction
    else
      do
        let responsesQueue = getFromLeaderState leaderNode _.signatureResponses
        numberOfResponses <- liftEffect $ Queue.length responsesQueue
        if numberOfResponses >= numberOfTransactions then
          endAction
        else
          do
            let stepDelay = 1000 -- TODO: should be configurable
            delay $ wrap (toNumber stepDelay)
            loop maxWaitingTime (waitedTime + stepDelay) numberOfTransactions

  endAction :: Aff $ OrderedMap UUID $ Maybe Transaction
  endAction = do
    signResponses <- liftEffect $ OrderedMap.fromFoldable <$>
      (Queue.takeAll $ getFromLeaderState leaderNode _.signatureResponses)
    waitingForSignatureMap <- getFromRefAtLeaderState leaderNode
      _.waitingForSignature
    let
      lookup uuid = maybe Nothing identity $ OrderedMap.lookup uuid
        signResponses
      results = (\(x /\ _) -> (x /\ lookup x)) <$> OrderedMap.toArray
        waitingForSignatureMap
    pure $ OrderedMap.fromFoldable results

purgeResponses
  :: forall a b e
   . OrderedMap UUID (UserAction a)
  -> OrderedMap UUID $ Either e b
  -> { sucess :: OrderedMap UUID b
     , failures :: OrderedMap UUID (UserAction a)
     }
purgeResponses originalRequest responses =
  let
    responsesArray = OrderedMap.toArray responses
  in
    case Array.findIndex (isLeft <<< snd) responsesArray of
      Nothing ->
        -- TODO: Add test to ensure is safe to use this
        { sucess: OrderedMap.fromFoldable (unsafeFromRight <$> responsesArray)
        , failures: OrderedMap.empty
        }
      Just ind ->
        let
          { before: sucess, after: failuresArray } = Array.splitAt ind
            responsesArray
          -- TODO: Add test to ensure is safe to use this
          lookup x = unsafeFromJust $ OrderedMap.lookup x originalRequest
          -- remove all items that were rejected or whose response we won't get in time.
          newFailures = Array.filter (isRight <<< snd) failuresArray
          failures = OrderedMap.fromFoldable
            ((\(uuid /\ _) -> (uuid /\ lookup uuid)) <$> newFailures)
        in
          -- TODO: Add test to ensure is safe to use this
          { sucess: OrderedMap.fromFoldable (unsafeFromRight <$> sucess)
          , failures
          }
  where
  unsafeFromRight :: UUID /\ Either e b -> UUID /\ b
  unsafeFromRight (uuid /\ v) = unsafePartial (\(Right x) -> (uuid /\ x)) v

  unsafeFromJust :: Maybe (UserAction a) -> UserAction a
  unsafeFromJust = unsafePartial fromJust

type SubmitM = ExceptT (UUID /\ String)
  (StateT (OrderedMap UUID TransactionHash) Aff)
  Unit

-- | Submit a Chain of `SignedTransaction`s
submitChain
  :: forall a
   . LeaderNode a
  -> OrderedMap UUID (UserAction a)
  -> OrderedMap UUID Transaction
  -> Aff $
       { succeeded :: (OrderedMap UUID TransactionHash)
       , failedOne :: Maybe (UUID /\ String)
       , toRetry :: OrderedMap UUID (UserAction a)
       }
submitChain leaderNode batch chain = do
  let
    (RunContract runContract) = getFromLeaderConfiguration
      leaderNode
      _.runContract

    removeExtra eth = if isLeft eth then 1 else 0
    submissions = traverse (submitTx runContract) (OrderedMap.toArray chain)

  (ethFailed /\ succeeded) <- runStateT (runExceptT submissions)
    OrderedMap.empty

  let
    {- dropping UUIDs that were submitted successfully
       and optionally dforpping failed transaction that broke the chain
       (we can just drop relying on the preservation of order in map)
    -}
    toRetrySigned :: OrderedMap UUID Transaction
    toRetrySigned =
      OrderedMap.drop
        (OrderedMap.length succeeded + removeExtra ethFailed)
        chain

    toRetry :: OrderedMap UUID (UserAction a)
    toRetry = foldl
      (\m k -> maybe identity (OrderedMap.push k) (lookupInBatch k) m)
      OrderedMap.empty
      (OrderedMap.orderedKeys toRetrySigned)

    lookupInBatch k = OrderedMap.lookup k batch

  pure $
    { succeeded
    , failedOne: getFailed ethFailed
    , toRetry
    }
  where

  submitTx
    :: (forall b. Contract b -> Aff b)
    -> (UUID /\ Transaction)
    -> SubmitM
  submitTx runContract (uuid /\ tx) = do
    ethTxId <- liftAff $ try do
      sginedByLeaderTx <- runContract
        $ signTransaction (wrap tx :: FinalizedTransaction)
      txId <- runContract $ submit sginedByLeaderTx
      log $ "Leader: Submitted chaned Tx ID: " <> show txId
      pure txId
    case lmap message ethTxId of
      Left e -> throwError (uuid /\ e)
      Right txId -> lift (modify_ $ OrderedMap.push uuid txId)

  getFailed v = case v of
    Left v' -> Just v'
    Right _ -> Nothing

-- For demo purposes: intentionally fails submission to break chain
submitChainBr
  :: forall a
   . LeaderNode a
  -> OrderedMap UUID (UserAction a)
  -> OrderedMap UUID Transaction
  -> Aff $
       { succeeded :: (OrderedMap UUID TransactionHash)
       , failedOne :: Maybe (UUID /\ String)
       , toRetry :: OrderedMap UUID (UserAction a)
       }
submitChainBr leaderNode batch chain = do
  let
    (RunContract runContract) = getFromLeaderConfiguration
      leaderNode
      _.runContract
  let
    elems :: Array (Int /\ (UUID /\ Transaction))
    elems =
      ( Array.zip
          (Array.range 1 (OrderedMap.length chain))
          (OrderedMap.toArray chain)
      )
    submissions = traverse (submitTx runContract) elems

  (ethFailed /\ succeeded) <- runStateT (runExceptT submissions)
    OrderedMap.empty

  let
    removeExtra eth = if isLeft eth then 1 else 0

    toRetrySigned :: OrderedMap UUID Transaction
    toRetrySigned =
      OrderedMap.drop
        (OrderedMap.length succeeded + removeExtra ethFailed)
        chain

    toRetry :: OrderedMap UUID (UserAction a)
    toRetry = foldl
      (\m k -> maybe identity (OrderedMap.push k) (lookupInBatch k) m)
      OrderedMap.empty
      (OrderedMap.orderedKeys toRetrySigned)

    lookupInBatch k = OrderedMap.lookup k batch

  pure $
    { succeeded
    , failedOne: getFailed ethFailed
    , toRetry
    }
  where

  submitTx
    :: (forall b. Contract b -> Aff b)
    -> (Int /\ (UUID /\ Transaction))
    -> SubmitM
  submitTx runContract (ix /\ (uuid /\ tx)) = do
    ethTxId <- liftAff $ try do
      when (ix == 2) $ throwError (error "Intentional Demo failure")
      sginedByLeaderTx <- runContract
        $ signTransaction (wrap tx :: FinalizedTransaction)
      txId <- runContract $ submit sginedByLeaderTx
      log $ "Leader: Submited chaned Tx ID: " <> show txId
      pure txId
    case lmap message ethTxId of
      Left e -> throwError (uuid /\ e)
      Right txId -> lift (modify_ $ OrderedMap.push uuid txId)

  getFailed v = case v of
    Left v' -> Just v'
    Right _ -> Nothing

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
  txChain <- try $ getFromLeaderConfiguration leaderNode _.buildChain
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
    maxWatingTime = getFromLeaderConfiguration ln
      _.maxWaitingTimeBeforeBuildChain
  loop maxWatingTime 0
  where
  -- loop 0 = pure unit
  loop maxWatingTime waitedTime =
    if waitedTime >= maxWatingTime then pure unit
    else do
      let tHold = getChaintriggerTreshold ln
      pendingNum <- getNumberOfPending ln
      if pendingNum >= tHold then pure unit
      else do
        let stepDelay = 1000 -- TODO: should be configurable
        delay (wrap $ toNumber stepDelay)
        loop maxWatingTime (waitedTime + stepDelay)

-- | Put an empty value in every ref of the `LeaderState` 
-- | except for `pendingActionsRequest` and `prioritaryPendingActions`
resetLeaderState :: forall a. LeaderNode a -> Aff Unit
resetLeaderState leaderNode = do
  setToRefAtLeaderState leaderNode OrderedMap.empty _.processing
  setToRefAtLeaderState leaderNode OrderedMap.empty _.waitingForSignature
  setToRefAtLeaderState leaderNode OrderedMap.empty _.waitingForSubmission
  setToRefAtLeaderState leaderNode WaitingForActions _.stage
  setToRefAtLeaderState leaderNode OrderedMap.empty _.submitted
  setToRefAtLeaderState leaderNode OrderedMap.empty _.submissionFailed

awaitChainConfirmed
  :: forall a. LeaderNode a -> OrderedMap UUID TransactionHash -> Aff Unit
awaitChainConfirmed node chainMap = do
  let
    (RunContract runContract) = getFromLeaderConfiguration
      node
      _.runContract
  for_ (OrderedMap.toArray chainMap) $
    \(uid /\ txHash) -> do
      log $ "Leader: Awaiting confirmation of " <> show uid <> " | " <> show
        (encodeAeson txHash)
      runContract $ awaitTxConfirmed txHash
  log "Leader: Chain confirmed"

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
  submitted <- liftEffect $ Ref.new OrderedMap.empty
  submissionFailed <- liftEffect $ Ref.new OrderedMap.empty
  stage <- liftEffect $ Ref.new WaitingForActions
  pure $ wrap
    { receivedActionsRequests
    , pendingActionsRequest
    , prioritaryPendingActions
    , processing
    , waitingForSignature
    , signatureResponses
    , waitingForSubmission
    , submitted
    , submissionFailed
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

leaderStateInfo :: forall a. LeaderNode a -> Aff LeaderServerInfo
leaderStateInfo ln@(LeaderNode node) = do
  numberOfActionsToProcess <- getNumberOfPending ln
  let maxNumberOfPendingActions = actionToTriggerChainBuild node.configuration
  let maxTimeOutForSignature = signTimeout node.configuration
  serverStage <- getFromRefAtLeaderState ln _.stage
  pure $ LeaderServerInfo
    { numberOfActionsToProcess
    , maxNumberOfPendingActions
    , maxTimeOutForSignature
    , serverStage
    }
