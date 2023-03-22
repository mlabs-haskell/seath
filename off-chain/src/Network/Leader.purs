module Seath.Network.Leader
  ( sendChainToUsersForSignature
  , waitForChainSignatures
  , submitChain
  , sendConfirmationToUsers
  ) where

import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Seath.Core.Types (UserAction)
import Seath.Network.Types
  ( NetworkError
  , Request
  , Response
  , SeathMonad
  , SignatureRequestContent
  , SignedTransaction
  )
import Type.Function (type ($))
import Undefined (undefined)

-- | Used by Leader to send a built chain for signatures.
sendChainToUsersForSignature
  :: forall a
   . Array (FinalizedTransaction /\ UserAction a)
  -> SeathMonad $ Array $ Either NetworkError $ Request
       (SignatureRequestContent /\ UserAction a)
sendChainToUsersForSignature = undefined

-- | It would wait for the responses of the given array  until the 
-- | configured timeout is reached.
waitForChainSignatures
  :: forall a
   . Array $ Request (SignatureRequestContent /\ UserAction a)
  -- TODO: How to handle the different errors? A network error should be
  -- different than a timeout and a user signature rejection.
  -> SeathMonad $ Array $ Either NetworkError $ Response
       (SignedTransaction /\ UserAction a)
waitForChainSignatures = undefined

-- | Submit a Chain of SignedTransactions
submitChain
  :: forall a
   . Array (SignedTransaction /\ UserAction a)
  -> SeathMonad $ Array $ Either String (TransactionHash /\ UserAction a)
submitChain = undefined

-- | 
sendConfirmationToUsers
  :: forall a. Array (TransactionHash /\ UserAction a) -> SeathMonad Unit
sendConfirmationToUsers = undefined
