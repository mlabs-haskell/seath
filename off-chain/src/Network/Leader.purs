module Seath.Network.Leader
  ( sendChainToUsersForSignature
  , waitForChainSignatures
  , submitChain
  , sendConfirmationToUsers
  , startLeaderServer
  , stopLeaderServer
  , getPendingActions
  , sendSignatureFailToUsers
  , splitSuccessFails
  ) where

import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Data.Array (findIndex, splitAt)
import Data.Either (Either(Right), isLeft)
import Data.Function ((>>>))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)
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

getPendingActions :: forall a. LeaderNode -> Aff $ Array $ UserAction a
getPendingActions = undefined

startLeaderServer :: LeaderNode -> Aff Unit
startLeaderServer = undefined

stopLeaderServer :: LeaderNode -> Aff Unit
stopLeaderServer = undefined

splitSuccessFails
  :: forall a b (t :: Type -> Type) (r :: Row Type)
   . Newtype (t (b /\ a)) { body :: b /\ a | r }
  => Array $ Either NetworkError $ t (b /\ a)
  -> { success :: Array b
     , fails ::
         Array
           (Either NetworkError $ t (b /\ a))
     }
splitSuccessFails responses =
  case findIndex isLeft responses of
    Just ind ->
      let
        cases = splitAt ind responses
        success = transformSuccess
          cases.before
      in
        { success, fails: cases.after }
    Nothing ->
      { success: transformSuccess responses, fails: [] }
  where
  transformSuccess
    :: Array $ Either NetworkError $ t (b /\ a)
    -> Array b
  transformSuccess success =
    (unsafePartial unsafeFromRight >>> unwrap >>> _.body >>> fst) <$> success

  unsafeFromRight :: forall c d. Partial => Either c d -> d
  unsafeFromRight (Right x) = x

-- | This inform to affected users that one transaction in the chain failed
-- | affecting it's action 
-- | and we would need to request later a new signed transaction
sendSignatureFailToUsers
  :: forall a
   . Array $ Either NetworkError $ Response (SignedTransaction /\ UserAction a)
  -> Aff $ UserAction a
sendSignatureFailToUsers = undefined
