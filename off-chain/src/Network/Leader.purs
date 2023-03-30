module Seath.Network.Leader
  ( sendChainToUsersForSignature
  , waitForChainSignatures
  , submitChain
  , sendConfirmationOfInclusionInSubmission
  , startLeaderServer
  , stopLeaderServer
  , getNextBatchOfActions
  , tellUsersWeNeedNewSignatures
  , newLeaderState
  , splitSuccessFails
  ) where

import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Seath.Core.Types (UserAction)
import Seath.Network.Types
  ( LeaderNode
  , LeaderState
  , OrderedMap
  , SignedTransaction
  , UserPetitionID
  )
import Type.Function (type ($))
import Undefined (undefined)

-- | Used by Leader to send a built chain for signatures.
sendChainToUsersForSignature
  :: forall a
   . LeaderNode a
  -> OrderedMap UserPetitionID FinalizedTransaction
  -> Aff $ OrderedMap UserPetitionID $ Either String $ FinalizedTransaction
sendChainToUsersForSignature = undefined

-- | It going to wait for the responses of the given array  until the 
-- | configured timeout is reached.
waitForChainSignatures
  :: forall a
   . LeaderNode a
  -> OrderedMap UserPetitionID FinalizedTransaction
  -> Aff $ OrderedMap UserPetitionID $ Either String SignedTransaction
waitForChainSignatures = undefined

-- | Submit a Chain of `SignedTransaction`s
submitChain
  :: forall a
   . LeaderNode a
  -> OrderedMap UserPetitionID SignedTransaction
  -> Aff $ OrderedMap UserPetitionID $ Either String $ TransactionHash
submitChain = undefined

-- | Send to users the confirmation that it's action is part of the 
-- | chain to be submitted.
-- | This function discard all errors.
sendConfirmationOfInclusionInSubmission
  :: forall a
   . LeaderNode a
  -> OrderedMap UserPetitionID TransactionHash
  -> Aff Unit
sendConfirmationOfInclusionInSubmission = undefined

-- | This inform to affected users that one transaction in the chain failed
-- | affecting it's action 
-- | and we would need to request later a new signed transaction
-- | all the transactions in the array would be discarded.
-- | We discard all errors in this function, we don't really need to 
-- | tell users in advance that we would need a new signature.
tellUsersWeNeedNewSignatures
  :: forall a
   . OrderedMap UserPetitionID $ Either String $ SignedTransaction
  -> Aff $ Array (UserPetitionID /\ UserAction a)
tellUsersWeNeedNewSignatures = undefined

-- To use inside getNextBatchOfActions
getAbatchOfPendingActions
  :: forall a
   . LeaderNode a
  -> Int
  -> Aff $ Array $ UserPetitionID /\ UserAction a
getAbatchOfPendingActions = undefined

-- The array it takes is the output of tellUsersWeNeedNewSignature.
getNextBatchOfActions
  :: forall a
   . LeaderNode a
  -> Array (UserPetitionID /\ UserAction a)
  -> Aff $ OrderedMap UserPetitionID $ UserAction a
getNextBatchOfActions = undefined

-- (getAbatchOfPendingActions undefined undefined)

startLeaderServer :: forall a. LeaderNode a -> Aff Unit
startLeaderServer = undefined

stopLeaderServer :: forall a. LeaderNode a -> Aff Unit
stopLeaderServer = undefined

-- | To build a new mutable `LeaderState`
newLeaderState :: forall a. Aff $ LeaderState a
newLeaderState = undefined

splitSuccessFails
  :: forall a b c
   . OrderedMap a $ Either b c
  -> { success :: OrderedMap a c, failures :: OrderedMap a b }
splitSuccessFails = undefined

-- splitSuccessFails
--   :: forall a b (t :: Type -> Type)
--    . Newtype (t (b /\ a)) { body :: b /\ a }
--   => Array $ Either NetworkError $ t (b /\ a)
--   -> { success :: Array b
--      , fails ::
--          Array
--            (Either NetworkError $ t (b /\ a))
--      }
-- splitSuccessFails responses =
--   case findIndex isLeft responses of
--     Just ind ->
--       let
--         cases = splitAt ind responses
--         success = transformSuccess
--           cases.before
--       in
--         { success, fails: cases.after }
--     Nothing ->
--       { success: transformSuccess responses, fails: [] }
--   where
--   transformSuccess
--     :: Array $ Either NetworkError $ t (b /\ a)
--     -> Array b
--   transformSuccess success =
--     (unsafePartial unsafeFromRight >>> unwrap >>> _.body >>> fst) <$> success
-- 
--   unsafeFromRight :: forall c d. Partial => Either c d -> d
--   unsafeFromRight (Right x) = x

