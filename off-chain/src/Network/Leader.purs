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

import Contract.Monad (Contract, runContractInEnv)
import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (ask)
import Ctl.Internal.Types.ScriptLookups (ownPaymentPubKeyHash)
import Data.Array as Array
import Data.Either (Either)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, delay, forkAff)
import Effect.Ref as Ref
import Options.Applicative (action)
import Seath.Core.ChainBuilder as ChainBuilder
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OMap
import Seath.Network.Types
  ( ActionStatus(..)
  , IncludeActionError(..)
  , LeaderConfiguration
  , LeaderNode(..)
  , LeaderServerStage(..)
  , LeaderServerStateInfo(..)
  , LeaderState(..)
  , SignedTransaction
  , addAction
  , canChain
  , currentBatchEmpty
  , fillCurrentBacth
  , getCurrentBatch
  , getPending
  , maxPendingCapacity
  , numberOfPending
  , signTimeout
  )
import Seath.Network.Utils (getPublicKeyHash)
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
  if (pendingCount < maxPendingCapacity node.configuration) then do
    -- if (pendingCount > maxPendingCapacity node.configuration) then -- DEBUG
    result <- (Right <$> addAction action node.state)
    -- FIXME: bad thing to do, addition sohuld not trigger chaining
    isCanChain <- canChain ln
    when isCanChain $ fillCurrentBacth ln
    pure result
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

startLeaderNode
  :: forall a
   . Show a
  => ( Array (UserAction a)
       -> Aff (Array (FinalizedTransaction /\ UserAction a))
     )
  -> LeaderConfiguration a
  -> Aff (LeaderNode a)
startLeaderNode chainBuild conf = do
  pending <- liftEffect $ Ref.new OMap.empty
  currentBatch <- liftEffect $ Ref.new OMap.empty
  chainedTransactions <- liftEffect $ Ref.new []
  let
    node = LeaderNode
      { state: LeaderState
          { pendingActionsRequest: pending
          , currentBatch: currentBatch
          , chainedTransactions: chainedTransactions
          , prioritaryPendingActions: undefined
          , signatureResponses: undefined
          , stage: undefined
          , numberOfActionsRequestsMade: undefined

          }
      , configuration: conf
      , testChainBuild: chainBuild
      }
  initChainBuilder node
  pure node

initChainBuilder :: forall a. Show a => LeaderNode a -> Aff Unit
initChainBuilder leaderNode@(LeaderNode node) = do
  log $ "initChainBuilder"
  _fiber <- forkAff (go)
  pure unit
  where
  go = do
    batchEmpty <- currentBatchEmpty leaderNode
    when (not batchEmpty) do --todo: get real one from config
      batch <- getCurrentBatch leaderNode
      let actions = snd <$> OMap.orderedElems batch
      txsAndActions <- node.testChainBuild actions
      log $ "The chain: " <> show txsAndActions
    delay $ Milliseconds 1000.0
    (go)

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
  pending <- numberOfPending leaderNode
  pure $ "\nLeader debug state:"
    <> "\n Num of pending actions: "
    <> show pending

-- TODO: partially mocked
leaderStateInfo :: forall a. LeaderNode a -> Aff LeaderServerStateInfo
leaderStateInfo leaderNode@(LeaderNode node) = do
  numberOfActionsToProcess <- numberOfPending leaderNode
  let maxNumberOfPendingActions = maxPendingCapacity node.configuration
  let maxTimeOutForSignature = signTimeout node.configuration
  let serverStage = WaitingForActions -- TODO: get the real one
  pure $ LeaderServerInfo
    { numberOfActionsToProcess
    , maxNumberOfPendingActions
    , maxTimeOutForSignature
    , serverStage
    }