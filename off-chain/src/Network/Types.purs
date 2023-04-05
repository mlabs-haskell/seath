module Seath.Network.Types where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , fromString
  , getField
  , toString
  )
import Contract.Transaction (FinalizedTransaction)
import Ctl.Internal.Helpers (encodeTagged')
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.UUID (UUID, genUUID)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OMap
import Type.Function (type ($))

-- TODO: replace this types with real ones.
-- The names are indicatives but can change.
type AsyncMutableQueueRef :: forall k. k -> k
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

instance encodeAesonIncludeActionError :: EncodeAeson IncludeActionError where
  encodeAeson = case _ of
    RejectedServerBussy inf -> encodeTagged' "RejectedServerBussy" inf
    OtherError err -> encodeTagged' "OtherError" err

instance decodeAesonIncludeActionError :: DecodeAeson IncludeActionError where
  decodeAeson s = do
    obj <- decodeAeson s
    tag <- getField obj "tag"
    contents <- getField obj "contents"
    case tag of
      "RejectedServerBussy" -> RejectedServerBussy <$> decodeAeson contents
      "OtherError" -> OtherError <$> decodeAeson contents
      other -> Left
        (TypeMismatch $ "IncludeActionError: unexpected constructor " <> other)

derive instance Generic IncludeActionError _

instance showIncludeActionError :: Show IncludeActionError where
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
instance showLSS :: Show LeaderServerStage where
  show = genericShow

instance encodeAesonLeaderServerStage :: EncodeAeson LeaderServerStage where
  encodeAeson = show >>> fromString

instance decodeAesonLeaderServerStage :: DecodeAeson LeaderServerStage where
  decodeAeson s =
    do
      str <- note (TypeMismatch "Expected string") (toString s)
      case str of
        "WaitingForActions" -> Right WaitingForActions
        "BuildingChain" -> Right BuildingChain
        "WaitingForChainSignatures" -> Right WaitingForChainSignatures
        "SubmittingChain" -> Right SubmittingChain
        other -> Left
          (TypeMismatch $ "Expected " <> other <> " for LeaderServerStage")

newtype SignedTransaction = SignedTransaction FinalizedTransaction

derive instance Generic SignedTransaction _

newtype LeaderServerStateInfo = LeaderServerInfo
  { numberOfActionsToProcess :: QueueIndex
  , maxNumberOfPendingActions :: QueueIndex
  , maxTimeOutForSignature :: MiliSeconds
  , serverStage :: LeaderServerStage
  }

derive newtype instance EncodeAeson LeaderServerStateInfo
derive newtype instance DecodeAeson LeaderServerStateInfo

derive instance Generic LeaderServerStateInfo _
instance showLeaderServerStateInfo :: Show LeaderServerStateInfo where
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
instance showSR :: Show StatusResponse where
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

addAction :: forall a. UserAction a -> LeaderState a -> Aff UUID
addAction action st = liftEffect do
  actionUUID <- genUUID
  pushRefMap_ actionUUID action
    (unwrap st).pendingActionsRequest
  pure actionUUID

numberOfPending :: forall a. LeaderState a -> Aff Int
numberOfPending st = OMap.length <$>
  (liftEffect $ Ref.read (unwrap st).pendingActionsRequest)

derive instance Newtype (LeaderState a) _

newtype LeaderConfiguration :: forall k. k -> Type
newtype LeaderConfiguration a = LeaderConfiguration
  { maxWaitingTimeForSignature :: MiliSeconds
  , maxQueueSize :: Int
  , numberOfActionToTriggerChainBuilder :: Int
  , maxWaitingTimeBeforeBuildChain :: Int

  -- FIXME: not sure we should do it like this.
  -- We will need user node to build webserver. So if we are passing web-server
  -- handlers here, we'll get circular dependency.
  -- I think we need only hadlers for 3d party services, that leader will need to call. 
  -- , serverHandlers ::
  --     { includeAction :: UserAction a -> Aff $ Either IncludeActionError UUID
  --     , acceptSignedTransaction ::
  --         SendSignedTransaction
  --         -> Aff $ Either AcceptSignedTransactionError Unit
  --     , rejectToSign :: UUID -> Aff Unit
  --     , getActionStatus :: UUID -> Aff StatusResponse
  --     }
  }

maxPendingCapacity :: forall a. LeaderConfiguration a -> Int
maxPendingCapacity conf = (unwrap conf).maxQueueSize

signTimeout :: forall a. LeaderConfiguration a -> Int
signTimeout conf = (unwrap conf).maxWaitingTimeForSignature

derive instance Newtype (LeaderConfiguration a) _

newtype LeaderNode a = LeaderNode
  { state :: LeaderState a
  , configuration :: LeaderConfiguration a
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
  , actionsSent :: Ref (OrderedMap UUID a)
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

derive instance Newtype (UserState a) _

newtype UserConfiguration a = UserConfiguration
  { maxQueueSize :: Int
  , clientHandlers ::
      { submitToLeader :: UserAction a -> Aff $ Either IncludeActionError UUID
      , acceptSignedTransaction ::
          SendSignedTransaction
          -> Aff $ Either AcceptSignedTransactionError Unit
      , rejectToSign :: UUID -> Aff Unit
      , getActionStatus :: UUID -> Aff StatusResponse
      }
  }

derive instance Newtype (UserConfiguration a) _

newtype UserNode a = UserNode
  { state :: UserState a
  , configuration :: UserConfiguration a
  }

derive instance Newtype (UserNode a) _

addToSentActions :: forall a. UserNode a -> (UUID /\ a) -> Aff Unit
addToSentActions (UserNode node) (uuid /\ action) = do
  liftEffect $ pushRefMap_ uuid action (unwrap node.state).actionsSent

readSentActions :: forall a. UserNode a -> Aff (Array (UUID /\ a))
readSentActions (UserNode node) = liftEffect $
  OMap.orderedElems <$> Ref.read (unwrap node.state).actionsSent

-- userHandlers :: forall a. UserNode a
userHandlers (UserNode node) = (unwrap node.configuration).clientHandlers

pushRefMap_
  :: forall k v. Ord k => k -> v -> Ref (OrderedMap k v) -> Effect Unit
pushRefMap_ k v mutMap =
  Ref.modify_
    (OMap.push k v)
    mutMap
