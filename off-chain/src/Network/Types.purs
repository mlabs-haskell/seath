module Seath.Network.Types where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  , fromString
  , getField
  , toString
  )
import Contract.Transaction (FinalizedTransaction(..), Transaction(..))
import Ctl.Internal.Helpers (encodeTagged')
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.UUID (UUID, genUUID)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Seath.Common.Types (UID(UID))
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OrderedMap
import Type.Function (type ($))

type MilliSeconds = Int

data IncludeActionError
  = RejectedServerBussy LeaderServerStateInfo
  | IAOtherError String

instance encodeAesonIncludeActionError :: EncodeAeson IncludeActionError where
  encodeAeson = case _ of
    RejectedServerBussy inf -> encodeTagged' "RejectedServerBussy" inf
    IAOtherError err -> encodeTagged' "IAOtherError" err

instance decodeAesonIncludeActionError :: DecodeAeson IncludeActionError where
  decodeAeson s = do
    obj <- decodeAeson s
    tag <- getField obj "tag"
    contents <- getField obj "contents"
    case tag of
      "RejectedServerBussy" -> RejectedServerBussy <$> decodeAeson contents
      "IAOtherError" -> IAOtherError <$> decodeAeson contents
      other -> Left
        (TypeMismatch $ "IncludeActionError: unexpected constructor " <> other)

derive instance Generic IncludeActionError _

instance showIncludeActionError :: Show IncludeActionError where
  show = genericShow

newtype AcceptSignedTransactionError = AcceptSignedTransactionError
  ActionStatus

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

derive instance Newtype SignedTransaction _
derive newtype instance Show SignedTransaction
derive instance Generic SignedTransaction _

newtype LeaderServerStateInfo = LeaderServerInfo
  { numberOfActionsToProcess :: Int
  , maxNumberOfPendingActions :: Int
  , maxTimeOutForSignature :: MilliSeconds
  , serverStage :: LeaderServerStage
  }

derive newtype instance EncodeAeson LeaderServerStateInfo
derive newtype instance DecodeAeson LeaderServerStateInfo

derive instance Generic LeaderServerStateInfo _
instance showLeaderServerStateInfo :: Show LeaderServerStateInfo where
  show = genericShow

newtype GetActionStatus = GetActionStatus
  { uuid :: UUID
  }

derive instance Newtype GetActionStatus _

data ActionStatus
  = AskForSignature
      { uuid :: UUID
      , txCborHex :: String -- TODO: maybe separate type?
      }
  | ToBeProcessed Int
  | ToBeSubmited Int
  | Processing
  | RejectedAtChainBuilder String
  | RequireNewSignature
  | SubmitError String
  | NotFound

derive instance Generic ActionStatus _

instance showActionStatus :: Show ActionStatus where
  show = genericShow

instance encodeAesonActionStatus :: EncodeAeson ActionStatus where
  encodeAeson = case _ of
    AskForSignature askForSig -> encodeTagged' "AskForSignature"
      (encodeAskForSig askForSig)
    ToBeProcessed i -> encodeTagged' "ToBeProcessed" i
    ToBeSubmited i -> encodeTagged' "ToBeSubmited" i
    Processing -> encodeTagged' "Processing" ""
    RejectedAtChainBuilder reason -> encodeTagged' "RejectedAtChainBuilder"
      reason
    RequireNewSignature -> encodeTagged' "RequireNewSignature" ""
    SubmitError err -> encodeTagged' "SubmitError" err
    NotFound -> encodeTagged' "NotFound" ""

    where
    encodeAskForSig
      :: { uuid :: UUID
         , txCborHex :: String
         }
      -> Aeson
    encodeAskForSig askForSig =
      encodeAeson
        { "uuid": encodeAeson (UID askForSig.uuid)
        , "txCborHex": encodeAeson askForSig.txCborHex
        }

instance decodeAesonActionStatus :: DecodeAeson ActionStatus where
  decodeAeson s = do
    obj <- decodeAeson s
    tag <- getField obj "tag"
    contents <- getField obj "contents"
    case tag of
      "AskForSignature" -> decodeAskForSig contents
      "ToBeProcessed" -> ToBeProcessed <$> decodeAeson contents
      "ToBeSubmited" -> ToBeSubmited <$> decodeAeson contents
      "Processing" -> Right Processing
      "RejectedAtChainBuilder" -> RejectedAtChainBuilder <$> decodeAeson
        contents
      "RequireNewSignature" -> Right RequireNewSignature
      "SubmitError" -> SubmitError <$> decodeAeson contents
      "NotFound" -> Right NotFound
      other -> Left
        (TypeMismatch $ "IncludeActionError: unexpected constructor " <> other)
    where
    decodeAskForSig c = do
      obj <- decodeAeson c
      (uid :: UID) <- getField obj "uuid"
      (cborHash :: String) <- getField obj "txCborHex"
      pure $ AskForSignature
        { uuid: unwrap uid
        , txCborHex: cborHash
        }

data GetStatusError = GSOtherError String

derive instance Generic GetStatusError _

instance showStatusResponseError :: Show GetStatusError where
  show = genericShow

instance encodeAesonStatusResponseError :: EncodeAeson GetStatusError where
  encodeAeson = case _ of
    GSOtherError err -> encodeTagged' "GSOtherError" err

instance decodeAesonStatusResponseError :: DecodeAeson GetStatusError where
  decodeAeson s = do
    obj <- decodeAeson s
    tag <- getField obj "tag"
    contents <- getField obj "contents"
    case tag of
      "GSOtherError" -> GSOtherError <$> decodeAeson contents
      other -> Left
        (TypeMismatch $ "IncludeActionError: unexpected constructor " <> other)

newtype SendSignedTransaction = SendSignedTransaction
  { uuid :: UUID
  , txCborHex :: String
  }

derive instance Newtype SendSignedTransaction _
derive newtype instance Show SendSignedTransaction

type LeaderStateInner a =
  { pendingActionsRequest :: Ref $ OrderedMap UUID (UserAction a)
  -- For actions that were part of a previous built chain 
  -- and were removed since a previous action failed.
  , prioritaryPendingActions :: Ref $ OrderedMap UUID (UserAction a)
  , processing :: Ref $ OrderedMap UUID (UserAction a)
  , waitingForSignature :: Ref $ OrderedMap UUID Transaction
  , waitingForSubmission :: Ref $ OrderedMap UUID Transaction
  , errorAtSubmission :: Ref $ OrderedMap UUID Transaction
  , stage :: LeaderServerStage
  }

newtype LeaderState a = LeaderState (LeaderStateInner a)

getFromRefAtLeaderState
  :: forall a b. LeaderNode a -> (LeaderStateInner a -> Ref b) -> Aff b
getFromRefAtLeaderState ln f =
  withRefFromState ln f Ref.read

setToRefAtLeaderState
  :: forall a b
   . LeaderNode a
  -> b
  -> (LeaderStateInner a -> Ref b)
  -> Aff Unit
setToRefAtLeaderState ln v f = do
  withRefFromState ln f (Ref.modify_ (const v))

withRefFromState
  :: forall a b c
   . LeaderNode a
  -> (LeaderStateInner a -> Ref b)
  -> (Ref b -> Effect c)
  -> Aff c
withRefFromState (LeaderNode node) acessor f =
  liftEffect $ f $ acessor (unwrap node.state)

takeFromPending
  :: forall a. Int -> LeaderNode a -> Aff (OrderedMap UUID (UserAction a))
takeFromPending n ln@(LeaderNode node) = do
  pending <- getFromRefAtLeaderState ln _.pendingActionsRequest
  liftEffect $ Ref.modify_ (const (OrderedMap.drop n pending))
    (unwrap node.state).pendingActionsRequest
  pure $ OrderedMap.take n pending

addAction :: forall a. UserAction a -> LeaderState a -> Aff UUID
addAction action st = liftEffect do
  actionUUID <- genUUID
  pushRefMap_ actionUUID action
    (unwrap st).pendingActionsRequest
  pure actionUUID

numberOfPending :: forall a. LeaderNode a -> Aff Int
numberOfPending (LeaderNode node) = OrderedMap.length <$>
  (liftEffect $ Ref.read (unwrap node.state).pendingActionsRequest)

derive instance Newtype (LeaderState a) _

newtype LeaderConfiguration :: forall k. k -> Type
newtype LeaderConfiguration a = LeaderConfiguration
  { maxWaitingTimeForSignature :: MilliSeconds
  , maxQueueSize :: Int
  , numberOfActionToTriggerChainBuilder :: Int
  , maxWaitingTimeBeforeBuildChain :: Int
  }

maxPendingCapacity :: forall a. LeaderConfiguration a -> Int
maxPendingCapacity conf = (unwrap conf).maxQueueSize

signTimeout :: forall a. LeaderConfiguration a -> Int
signTimeout conf = (unwrap conf).maxWaitingTimeForSignature

chaintriggerTreshold :: forall a. LeaderNode a -> Int
chaintriggerTreshold (LeaderNode node) =
  (unwrap node.configuration).numberOfActionToTriggerChainBuilder

derive instance Newtype (LeaderConfiguration a) _

newtype LeaderNode a = LeaderNode
  { state :: LeaderState a
  , configuration :: LeaderConfiguration a
  , buildChain ::
      Array (UserAction a)
      -> Aff (Array (FinalizedTransaction /\ UserAction a))
  }

derive instance Newtype (LeaderNode a) _

newtype UserState a = UserState
  {
    -- | `actionsSent` pourpose is to store the actions already
    -- | send the to the `leader`that are confirmed to be accepted
    -- | but are still waiting to reach the requirement of signature.
    actionsSent :: Ref (OrderedMap UUID a)
  -- | For actions whose transaction is already signed and sent 
  -- | to the server.
  , transactionsSent :: Ref (OrderedMap UUID a)
  -- | Those signed transactions are confirmed to be in the chain 
  -- | ready for submission.
  -- | Once a transaction is confirmed to be done, we can safely 
  -- | remove it from this.
  -- | Is advisable to clean it from time to time, Seath WON'T clean it.
  -- Well maybe we can put a extra call back in the interface that
  -- is automatically called wen the transaction is confirmed in the 
  -- blockchain and then we autoremove this transactions.
  , submitedTransactions :: Ref (OrderedMap UUID a)
  }

derive instance Newtype (UserState a) _

type UserHandlers a =
  { submitToLeader :: UserAction a -> Aff $ Either IncludeActionError UUID
  , sendSignedToLeader ::
      SendSignedTransaction
      -> Aff $ Either AcceptSignedTransactionError Unit
  , refuseToSign :: UUID -> Aff Unit
  , getActionStatus :: UUID -> Aff (Either GetStatusError ActionStatus)
  }

newtype UserConfiguration a = UserConfiguration
  { maxQueueSize :: Int
  , clientHandlers :: UserHandlers a
  }

derive instance Newtype (UserConfiguration a) _

newtype UserNode a = UserNode
  { state :: UserState a
  , configuration :: UserConfiguration a
  , makeAction :: a -> Aff (UserAction a)
  }

derive instance Newtype (UserNode a) _

addToSentActions :: forall a. UserNode a -> (UUID /\ a) -> Aff Unit
addToSentActions (UserNode node) (uuid /\ action) = do
  liftEffect $ pushRefMap_ uuid action (unwrap node.state).actionsSent

readSentActions :: forall a. UserNode a -> Aff (Array (UUID /\ a))
readSentActions (UserNode node) = liftEffect $
  OrderedMap.orderedElems <$> Ref.read (unwrap node.state).actionsSent

getUserHandlers :: forall a. UserNode a -> UserHandlers a
getUserHandlers (UserNode node) = (unwrap node.configuration).clientHandlers

pushRefMap_
  :: forall k v. Ord k => k -> v -> Ref (OrderedMap k v) -> Effect Unit
pushRefMap_ k v mutMap =
  Ref.modify_
    (OrderedMap.push k v)
    mutMap
