module Seath.Network.Leader
  ( waitForChainSignatures
  , submitChain
  , startLeaderServer
  , stopLeaderServer
  , getNextBatchOfActions
  , newLeaderState
  ) where

import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.Types
  ( LeaderNode
  , LeaderState
  , SignedTransaction
  )
import Type.Function (type ($))
import Undefined (undefined)

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
  (getAbatchOfPendingActions undefined undefined)

startLeaderServer :: forall a. LeaderNode a -> Aff Unit
startLeaderServer = undefined

stopLeaderServer :: forall a. LeaderNode a -> Aff Unit
stopLeaderServer = undefined

-- | To build a new mutable `LeaderState`
newLeaderState :: forall a. Aff $ LeaderState a
newLeaderState = undefined
