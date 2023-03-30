module Seath.Network.Types where

import Aeson (class DecodeAeson, class EncodeAeson, decodeJsonString)
import Contract.Prelude (class Show, bind, pure, show, ($))
import Contract.Transaction (FinalizedTransaction)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Debug (trace)
import Effect.Aff (Aff)
import Payload.Server.DecodeBody (class DecodeBody)
import Seath.Core.Types (UserAction)
import Type.Function (type ($))

-- TODO: replace this types with real ones.
-- The names are indicatives but can change.
type AsyncMutableQueueRef :: forall k. k -> k
type AsyncMutableQueueRef a = a

type Ip = String
type Port = String
type ControlNumber = Int
type QueueIndex = Int
type ChainNumber = Int
type ChainIndex = Int
type RequestCounter = Int
type MiliSeconds = Int
type Time = Int
type MutableInt = Int

-- This represent a Map that can remember the order of insertion.
-- it seems that we can use purescript-ordered-collections with a 
-- type like this.
-- I would like to use purescript-ordered-collections, but the 
-- Ord instance it has is over the keys, and then we would need 
-- to cast it to a plain map quite often.
type OrderedMap keys values =
  { map :: Map keys (values /\ Int), lastIndex :: Int }

newtype IncludeActionError = RejectedServerBussy LeaderServerStateInfo

data ProcessingActionError
  = RejectedAtChainBuilder String
  | RequireNewSignature
  | SubmitError String

data LeaderServerStage
  = WaitingForActions
  | BuildingChain
  | WaitingForChainSignatures
  | SubmittingChain

newtype SignedTransaction = SignedTransaction FinalizedTransaction

derive instance Generic SignedTransaction _

newtype LeaderServerStateInfo = LeaderServerInfo
  { numberOfActionsToProcess :: QueueIndex
  , maxNumberOfPendingActions :: QueueIndex
  , maxTimeOutForSignature :: MiliSeconds
  , serverStage :: LeaderServerStage
  }

newtype UserPetitionID = UserPetitionID
  {
    -- This intend to be the number of petitions that the leader 
    -- has processed until now, but I don't know if we really need it.
    -- A consequence of having it, can be that a lot of request in 
    -- the server would try to update it at the same time 
    -- droping performance.
    -- In the other side, this allow us to mantain the order of 
    -- the petitions.
    petitionNumber :: ControlNumber
  -- | This is provided by the user request
  , userControlNumber :: ControlNumber
  , ip :: Ip
  , port :: Port
  }

newtype IncludeActionRequest a = IncludeActionRequest
  {
    -- | This number is atached on every message that the 
    -- | server sends to the user related to this action.
    controlNumber :: ControlNumber
  , ip :: Ip
  , port :: Port
  , action :: UserAction a
  }

instance incReq :: Show a => Show (IncludeActionRequest a) where
  show (IncludeActionRequest iar) = show iar.action

instance decBodyIncludeActionRequest ::
  ( DecodeAeson a
  ) =>
  DecodeBody (IncludeActionRequest a) where
  decodeBody s = lmap show $ decodeJsonString s

derive newtype instance EncodeAeson a => EncodeAeson (IncludeActionRequest a)
derive newtype instance DecodeAeson a => DecodeAeson (IncludeActionRequest a)

-- The idea is that we would read the lenght of the pending request
-- queue and based on that we accept or reject the action
newtype IncludeActionResponse = IncludeActionResponse
  {
    -- | The number attached in the user request
    controlNumber :: ControlNumber
  , status :: Either IncludeActionError LeaderServerStateInfo
  }

-- | Only the Request, the expected response is empty, details in the `Spec`.
-- | The expected response is always empty
newtype AskForSignature = AskForSignature
  {
    -- | The control number that the user attached in it's request to
    -- | process the action.
    controlNumber :: ControlNumber
  -- | Seath `leader` maintains a control of the number of built chains
  -- | Every response corresponding to a request with a 
  -- | chain number below a threshold are ignored by the leader.
  , chainNumber :: ChainNumber
  -- | Index of the transaction in the chain built.
  , index :: ChainIndex
  , timeOfExecution :: Time
  -- | Transaction ready to be signed by the user.
  , transaction :: FinalizedTransaction
  }

-- | Only the Request, the expected response is empty, details in the `Spec`.
newtype SendSignedTransaction = SendSignedTransaction
  { chainNumber :: ChainNumber
  , index :: ChainIndex
  , transaction :: SignedTransaction
  }

-- | Only the Request, the expected response is empty, details in the `Spec`.
newtype SendSignatureRejection = SendSignatureRejection
  { chainNumber :: ChainNumber
  , index :: ChainIndex
  }

newtype ReportErrorAtProcessing = ReportErrorAtProcessing
  { userControlNumber :: ControlNumber
  , errorStatus :: ProcessingActionError
  }

newtype PendingAction a = PendingAction
  { petitionID :: UserPetitionID
  , action :: UserAction a
  }

-- I suspect that this `a` is a type like :
-- `UserPetitionID /\ UserAction a`
newtype LeaderState a = LeaderState
  { pendingActionsRequest :: AsyncMutableQueueRef a
  -- For actions that were part of a previous built chain 
  -- and were removed since a previous action failed.
  , prioritaryPendingActions :: Array a
  , signatureResponses :: AsyncMutableQueueRef a
  , processedRequest :: MutableInt
  , numberOfChainsBuilt :: ChainNumber
  , stage :: LeaderServerStage
  -- We really need to think if we really want this,
  -- see `UserPetitionID` comment.
  , numberOfActionsRequestsMade :: MutableInt
  }

newtype LeaderConfiguration = LeaderConfiguration
  { maxWaitingTimeForSignature :: MiliSeconds
  , maxQueueSize :: Int
  , numberOfActionToTriggerChainBuilder :: Int
  , maxWaitingTimeBeforeBuildChain :: Int
  -- Those are built on top of a network or IO interface
  , clientHandlers ::
      { transactionSignature :: AskForSignature -> Aff $ Either String Unit
      , reportError :: ReportErrorAtProcessing -> Aff $ Either String Unit
      }
  }

newtype LeaderNode a = LeaderNode
  { state :: LeaderState a
  , configuration :: LeaderConfiguration
  }

newtype UserState a = UserState
  {
    -- | This three types must store the state of the sent transactions.
    -- | `actionSent` is for actions that are confirmed to be received
    -- | by the leader but aren't in processes right now.
    -- TODO : put the right types in the following three
    -- Maybe is : `MutableReference (OrderedMap (SomeUniqueID a) UserAction a)
    actionsSent :: Array a
  -- | `transactionsSent` is intended for both the `action` and it's
  -- | corresponding `SignedTransaction` already send to the server.
  , transactionsSent :: Array a
  -- | Those signed transactions are confirmed to be in the chain 
  -- | ready for submission.
  -- | Once a transaction is confirmed to be done, we can safely 
  -- | remove it from this.
  -- | Is advisable to clean it from time to time, Seath WON'T clean it.
  -- Well maybe we can put a extra call back in the interface that
  -- is automatically called wen the transaction is confirmed in the 
  -- blockchain and then we autoremove this transactions.
  , submitedTransactions :: Array a
  -- | This is used to fill a requests as the `ControlNumber`
  , numberOfActionsRequestsMade :: MutableInt
  }

newtype UserConfiguration a = UserConfiguration
  { maxQueueSize :: Int
  , clientHandlers ::
      { includeAction ::
          IncludeActionRequest a -> Aff $ Either String IncludeActionResponse
      , acceptSignedTransaction ::
          SendSignedTransaction -> Aff $ Either String Unit
      , rejectToSign :: SendSignatureRejection -> Aff $ Either String Unit
      }
  }

newtype UserNode a = UserNode
  { state :: UserState a
  , configuration :: UserConfiguration a
  }
