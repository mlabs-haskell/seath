module Seath.Network.Leader
  ( sendChainToUsersForSignature
  , waitForChainSignatures
  , submitChain
  , sendConfirmationToUsers
  , startLeaderServer
  , stopLeaderServer
  , getPendingActions
  ) where

import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Seath.Core.Types (UserAction)
import Seath.Network.Types
  ( LeaderNode
  , NetworkError
  , Request
  , Response
  , SignatureRequestContent
  , SignedTransaction
  )
import Type.Function (type ($))
import Undefined (undefined)

-- | Used by Leader to send a built chain for signatures.
sendChainToUsersForSignature
  :: forall a
   . LeaderNode
  -> Array $ FinalizedTransaction /\ UserAction a
  -> Aff $ Array $ Either NetworkError $ Request
       (SignatureRequestContent /\ UserAction a)
sendChainToUsersForSignature = undefined

-- | It would wait for the responses of the given array  until the 
-- | configured timeout is reached.
waitForChainSignatures
  :: forall a
   . LeaderNode
  -> Array $ Request (SignatureRequestContent /\ UserAction a)
  -- TODO: How to handle the different errors? A network error should be
  -- different than a timeout and a user signature rejection.
  -> Aff $ Array $ Either NetworkError $ Response
       (SignedTransaction /\ UserAction a)
waitForChainSignatures = undefined

-- | Submit a Chain of SignedTransactions
submitChain
  :: forall a
   . LeaderNode
  -> Array (SignedTransaction /\ UserAction a)
  -> Aff $ Array $ Either String
       (TransactionHash /\ UserAction a)
submitChain = undefined

-- | 
sendConfirmationToUsers
  :: forall a
   . LeaderNode
  -> Array (TransactionHash /\ UserAction a)
  -> Aff Unit
sendConfirmationToUsers = undefined

getPendingActions :: forall a . LeaderNode -> Aff $ Array $ UserAction a
getPendingActions = undefined

startLeaderServer :: LeaderNode -> Aff Unit
startLeaderServer = undefined

stopLeaderServer :: LeaderNode -> Aff Unit
stopLeaderServer = undefined
