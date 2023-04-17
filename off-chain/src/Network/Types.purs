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
import Contract.Monad (Contract)
import Contract.Transaction
  ( BalancedSignedTransaction
  , FinalizedTransaction
  , Transaction
  )
import Ctl.Internal.Helpers (encodeTagged')
import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Queue as Queue
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
  | ToBeSubmitted Int
  | Processing
  | PrioritaryToBeProcessed Int
  | NotFound

derive instance Generic ActionStatus _

instance showActionStatus :: Show ActionStatus where
  show = genericShow

instance encodeAesonActionStatus :: EncodeAeson ActionStatus where
  encodeAeson = case _ of
    AskForSignature askForSig -> encodeTagged' "AskForSignature"
      (encodeAskForSig askForSig)
    ToBeProcessed i -> encodeTagged' "ToBeProcessed" i
    ToBeSubmitted i -> encodeTagged' "ToBeSubmitted" i
    Processing -> encodeTagged' "Processing" ""
    PrioritaryToBeProcessed i -> encodeTagged' "PrioritaryToBeProcessed" i
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
      "ToBeSubmitted" -> ToBeSubmitted <$> decodeAeson contents
      "Processing" -> Right Processing
      "PrioritaryToBeProcessed" -> PrioritaryToBeProcessed <$> decodeAeson
        contents
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

newtype SendSignedTransaction = SendSignedTransaction
  { uuid :: UUID
  , txCborHex :: String
  }

derive instance Newtype SendSignedTransaction _
derive newtype instance Show SendSignedTransaction

-- if you update this, update `Seath.Network.Leader.actionStatus` please
-- and also `Seath.Network.Leader.restLeaderState`
type LeaderStateInner a =
  { receivedActionsRequests ::
      Queue.Queue (read :: Queue.READ, write :: Queue.WRITE)
        -- TODO : write types to represent this
        -- left is for leader to tell how much pending actions are taken
        -- right is for payload handler.
        (Either Int (UserAction a /\ Ref (Either Unit (Maybe UUID))))
  , pendingActionsRequest ::
      Queue.Queue (read :: Queue.READ, write :: Queue.WRITE)
        (UUID /\ UserAction a)
  -- For actions that were part of a previous built chain 
  -- and were removed since a previous action failed.
  , prioritaryPendingActions :: Ref $ OrderedMap UUID (UserAction a)
  , processing :: Ref $ OrderedMap UUID (UserAction a)
  , waitingForSignature :: Ref $ OrderedMap UUID Transaction
  , signatureRequests ::
      Queue.Queue (read :: Queue.READ, write :: Queue.WRITE)
        (UUID /\ Either Unit Transaction)
  , waitingForSubmission :: Ref $ OrderedMap UUID Transaction
  , stage :: Ref LeaderServerStage
  }

newtype LeaderState a = LeaderState (LeaderStateInner a)

getFromRefAtLeaderState
  :: forall a b. LeaderNode a -> (LeaderStateInner a -> Ref b) -> Aff b
getFromRefAtLeaderState ln f =
  withRefFromState ln f Ref.read

getFromLeaderState :: forall a b. LeaderNode a -> (LeaderStateInner a -> b) -> b
getFromLeaderState ln accessor = accessor (unwrap (unwrap ln).state)

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
  let pendingQueue = getFromLeaderState ln _.pendingActionsRequest
  pendingValues <- liftEffect $ Queue.takeMany pendingQueue n
  let requestsQueue = getFromLeaderState ln _.receivedActionsRequests
  let taken = Array.length pendingValues
  liftEffect $ Queue.put requestsQueue (Left taken)
  pure $ OrderedMap.fromFoldable pendingValues

getFromLeaderConfiguration
  :: forall a b. LeaderNode a -> (LeaderConfigurationInner a -> b) -> b
getFromLeaderConfiguration (LeaderNode node) accessor = accessor
  (unwrap node.configuration)

getNumberOfPending :: forall a. LeaderNode a -> Aff Int
getNumberOfPending ln = do
  numberOfPrioritary <- OrderedMap.length <$> getFromRefAtLeaderState ln
    _.prioritaryPendingActions
  numberOfPending <- liftEffect $ Queue.length $ getFromLeaderState ln
    _.pendingActionsRequest
  pure $ numberOfPrioritary + numberOfPending

derive instance Newtype (LeaderState a) _

-- Needed to avoid purescript to reject the newtype instance of 
-- `LeaderConfiguration`.
newtype FunctionToPerformContract = FunctionToPerformContract
  (forall b. Contract b -> Aff b)

type LeaderConfigurationInner a =
  { maxWaitingTimeForSignature :: MilliSeconds
  , maxQueueSize :: Int
  , numberOfActionToTriggerChainBuilder :: Int
  , maxWaitingTimeBeforeBuildChain :: Int
  , fromContract :: FunctionToPerformContract
  }

newtype LeaderConfiguration :: forall k. k -> Type
newtype LeaderConfiguration a = LeaderConfiguration (LeaderConfigurationInner a)

maxPendingCapacity :: forall a. LeaderConfiguration a -> Int
maxPendingCapacity conf = (unwrap conf).maxQueueSize

signTimeout :: forall a. LeaderConfiguration a -> Int
signTimeout conf = (unwrap conf).maxWaitingTimeForSignature

getChaintriggerTreshold :: forall a. LeaderNode a -> Int
getChaintriggerTreshold (LeaderNode node) =
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
  , getActionStatus :: UUID -> Aff ActionStatus
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
  , signTx :: FinalizedTransaction -> Aff BalancedSignedTransaction
  }

derive instance Newtype (UserNode a) _

addToSentActions :: forall a. UserNode a -> (UUID /\ a) -> Aff Unit
addToSentActions (UserNode node) (uuid /\ action) = do
  liftEffect $ pushRefMap_ uuid action (unwrap node.state).actionsSent

addToTransactionsSent :: forall a. UserNode a -> (UUID /\ a) -> Aff Unit
addToTransactionsSent (UserNode node) (uuid /\ action) = do
  liftEffect $ pushRefMap_ uuid action (unwrap node.state).transactionsSent

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
