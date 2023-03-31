module Seath.Network.Types where

import Contract.Prelude (genericShow)
import Contract.Transaction (FinalizedTransaction)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Prelude (class Show)
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Type.Function (type ($))

-- TODO: replace this types with real ones.
-- The names are indicatives but can change.
type AsyncMutableQueueRef a = a
type ControlNumber = Int
type QueueIndex = Int
type ChainNumber = Int
type ChainIndex = Int
type RequestCounter = Int
type MiliSeconds = Int
type Time = Int
type MutableInt = Int

data IncludeActionError 
  = RejectedServerBussy LeaderServerStateInfo
  | OtherError String

derive instance Generic IncludeActionError _

instance showIAE :: Show IncludeActionError where
  show = genericShow

newtype AcceptSignedTransactionError = AcceptSignedTransactionError
  StatusResponse

derive newtype instance Show AcceptSignedTransactionError

data LeaderServerStage
  = WaitingForActions
  | BuildingChain
  | WaitingForChainSignatures
  | SubmittingChain

derive instance Generic LeaderServerStage _
instance showLSS :: Show  LeaderServerStage where
  show = genericShow

newtype SignedTransaction = SignedTransaction FinalizedTransaction

derive instance Generic SignedTransaction _

newtype LeaderServerStateInfo = LeaderServerInfo
  { numberOfActionsToProcess :: QueueIndex
  , maxNumberOfPendingActions :: QueueIndex
  , maxTimeOutForSignature :: MiliSeconds
  , serverStage :: LeaderServerStage
  }

derive instance Generic LeaderServerStateInfo _
instance showLSSI :: Show  LeaderServerStateInfo where
  show = genericShow

newtype GetActionStatus = GetActionStatus
  { controlNumber :: UUID
  }

derive instance Newtype GetActionStatus _

data StatusResponse
  = AskForSignature
      {
        -- | The control number that the user attached in it's request to
        -- | process the action.
        controlNumber :: UUID
      , transaction :: FinalizedTransaction
      }
  | ToBeProcessed Int
  | ToBeSubmited Int
  | Processing
  | RejectedAtChainBuilder String
  | RequireNewSignature
  | SubmitError String
  | NotFound

derive instance Generic StatusResponse _
instance showSR :: Show  StatusResponse where
  show = genericShow

newtype SendSignedTransaction = SendSignedTransaction
  { controlNumber :: UUID
  , transaction :: SignedTransaction
  }

-- I suspect that this `a` is a type like :
-- `UUID /\ UserAction a`
newtype LeaderState a = LeaderState
  { pendingActionsRequest :: Ref $ OrderedMap UUID (UserAction a)
  -- For actions that were part of a previous built chain 
  -- and were removed since a previous action failed.
  , prioritaryPendingActions :: Ref $ OrderedMap UUID (UserAction a)
  , signatureResponses :: Ref $ OrderedMap UUID (UserAction a)
  , stage :: LeaderServerStage
  -- We really need to think if we really want this,
  -- see `UUID` comment.
  , numberOfActionsRequestsMade :: MutableInt
  }

derive instance Newtype (LeaderState a) _

newtype LeaderConfiguration = LeaderConfiguration
  { maxWaitingTimeForSignature :: MiliSeconds
  , maxQueueSize :: Int
  , numberOfActionToTriggerChainBuilder :: Int
  , maxWaitingTimeBeforeBuildChain :: Int
  }

derive instance Newtype LeaderConfiguration _

newtype LeaderNode a = LeaderNode
  { state :: LeaderState a
  , configuration :: LeaderConfiguration
  }

derive instance Newtype (LeaderNode a) _

newtype UserState a = UserState
  {
    -- | This three types must store the state of the sent transactions.
    -- | `actionSent` is for actions that are confirmed to be received
    -- | by the leader but aren't in processes right now.
    -- TODO : put the right types in the following three
    -- Maybe is : `MutableReference (OrderedMap (SomeUniqueID a) UserAction a)
    pendingResponse :: OrderedMap UUID a
  , actionsSent :: OrderedMap UUID a
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
      { includeAction :: UserAction a -> Aff $ Either IncludeActionError UUID
      , acceptSignedTransaction ::
          SendSignedTransaction
          -> Aff $ Either AcceptSignedTransactionError Unit
      , rejectToSign :: UUID -> Aff Unit
      , getActionStatus :: UUID -> StatusResponse
      }
  }

newtype UserNode a = UserNode
  { state :: UserState a
  , configuration :: UserConfiguration a
  }
