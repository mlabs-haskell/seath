module Seath.Network.Types
  ( AcceptSignedTransactionError(AcceptSignedTransactionError)
  , ActionResult
  , ActionStatus
      ( AskForSignature
      , ToBeProcessed
      , ToBeSubmitted
      , Processing
      , WaitingOtherChainSignatures
      , PrioritaryToBeProcessed
      , Submitted
      , SubmissionFailed
      , NotFound
      )
  , RunContract(RunContract)
  , GetActionStatus(GetActionStatus)
  , IncludeActionError(RejectedServerBussy)
  , LeaderConfiguration(LeaderConfiguration)
  , LeaderConfigurationInner
  , LeaderNode(LeaderNode)
  , LeaderServerInfo(LeaderServerInfo)
  , LeaderServerStage
      ( WaitingForActions
      , BuildingChain
      , WaitingForChainSignatures
      , SubmittingChain
      )
  , LeaderState(LeaderState)
  , LeaderStateInner
  , MilliSeconds
  , SendSignedTransaction(SendSignedTransaction)
  , SignedTransaction(SignedTransaction)
  , UserConfiguration(UserConfiguration)
  , NetworkHandlers
  , UserNode(UserNode)
  , UserState(UserState)
  , UserStateInner
  ) where

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
import Contract.Transaction (FinalizedTransaction, Transaction, TransactionHash)
import Ctl.Internal.Helpers (encodeTagged')
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, Fiber)
import Effect.Ref (Ref)
import Queue as Queue
import Seath.Common.Types (UID(UID))
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Type.Function (type ($))

type MilliSeconds = Int

newtype IncludeActionError = RejectedServerBussy LeaderServerInfo

derive instance Newtype IncludeActionError _
derive newtype instance Show IncludeActionError
derive newtype instance EncodeAeson IncludeActionError
derive newtype instance DecodeAeson IncludeActionError

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

newtype LeaderServerInfo = LeaderServerInfo
  { numberOfActionsToProcess :: Int
  , maxNumberOfPendingActions :: Int
  , maxTimeOutForSignature :: MilliSeconds
  , serverStage :: LeaderServerStage
  }

derive newtype instance EncodeAeson LeaderServerInfo
derive newtype instance DecodeAeson LeaderServerInfo

derive instance Generic LeaderServerInfo _
instance showLeaderServerStateInfo :: Show LeaderServerInfo where
  show = genericShow

newtype GetActionStatus = GetActionStatus
  { uuid :: UUID
  }

derive instance Newtype GetActionStatus _

data ActionStatus
  = AskForSignature
      { uuid :: UUID -- Remove it? we don't need anymore with current types.
      , txCborHex :: String -- TODO: maybe separate type?
      }
  | ToBeProcessed Int
  | ToBeSubmitted Int
  | Processing
  | WaitingOtherChainSignatures (Maybe TransactionHash)
  | PrioritaryToBeProcessed Int
  | Submitted TransactionHash
  | SubmissionFailed String
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
    WaitingOtherChainSignatures txH -> encodeTagged'
      "WaitingOtherChainSignatures"
      (encodeAeson txH)
    PrioritaryToBeProcessed i -> encodeTagged' "PrioritaryToBeProcessed" i
    Submitted txH -> encodeTagged' "Submitted" (encodeAeson txH)
    SubmissionFailed err -> encodeTagged' "SubmissionFailed" (encodeAeson err)
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
      "WaitingOtherChainSignatures" -> WaitingOtherChainSignatures <$>
        decodeAeson contents
      "PrioritaryToBeProcessed" -> PrioritaryToBeProcessed <$> decodeAeson
        contents
      "Submitted" -> Submitted <$> decodeAeson contents
      "SubmissionFailed" -> SubmissionFailed <$> decodeAeson contents
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
  , signatureResponses ::
      Queue.Queue (read :: Queue.READ, write :: Queue.WRITE)
        (UUID /\ Maybe Transaction)
  , waitingForSubmission :: Ref $ OrderedMap UUID Transaction
  , submitted :: Ref $ OrderedMap UUID TransactionHash
  , submissionFailed :: Ref $ OrderedMap UUID String
  , stage :: Ref LeaderServerStage
  }

newtype LeaderState a = LeaderState (LeaderStateInner a)

derive instance Newtype (LeaderState a) _

-- Needed to avoid purescript to reject the newtype instance of 
-- `LeaderConfiguration`.
newtype RunContract = RunContract
  (forall b. Contract b -> Aff b)

type LeaderConfigurationInner a =
  { maxWaitingTimeForSignature :: MilliSeconds
  , numberOfActionToTriggerChainBuilder :: Int
  , maxWaitingTimeBeforeBuildChain :: MilliSeconds
  , runContract :: RunContract
  , buildChain ::
      Array (UserAction a)
      -> Aff (Array (FinalizedTransaction /\ UserAction a))
  }

newtype LeaderConfiguration a = LeaderConfiguration (LeaderConfigurationInner a)

derive instance Newtype (LeaderConfiguration a) _

newtype LeaderNode a = LeaderNode
  { state :: LeaderState a
  , configuration :: LeaderConfiguration a
  , _leaderFibers :: Ref (Array (Fiber Unit))
  }

derive instance Newtype (LeaderNode a) _

type ActionResult a =
  { uuid :: UUID
  , action :: UserAction a
  -- TODO: Introduce proper type for error instead of String
  , status :: Either String TransactionHash
  }

type UserStateInner a =
  { actionsSentQueue ::
      Queue.Queue (read :: Queue.READ, write :: Queue.WRITE)
        { uuid :: UUID
        , action :: UserAction a
        , status :: ActionStatus
        , previousStatus :: ActionStatus
        }
  -- | `actionsSent` pourpose is to store the actions already
  -- | send the to the `leader`that are confirmed to be accepted
  -- | but are still waiting to reach the requirement of signature.
  , actionsSent :: Ref (OrderedMap UUID (UserAction a /\ ActionStatus))
  , resultsQueue ::
      Queue.Queue (read :: Queue.READ, write :: Queue.WRITE)
        -- TODO: make this a newtype 
        (ActionResult a)
  }

newtype UserState a = UserState (UserStateInner a)

derive instance Newtype (UserState a) _

type NetworkHandlers a =
  { submitToLeader :: UserAction a -> Aff $ Either IncludeActionError UUID
  , sendSignedToLeader ::
      SendSignedTransaction
      -> Aff $ Either AcceptSignedTransactionError Unit
  , refuseToSign :: UUID -> Aff Unit
  , getActionStatus :: UUID -> Aff ActionStatus
  }

newtype UserConfiguration a = UserConfiguration
  { networkHandlers :: NetworkHandlers a
  , runContract :: RunContract
  , checkChainedTx :: Transaction -> Aff (Either String Transaction)
  }

derive instance Newtype (UserConfiguration a) _

newtype UserNode a = UserNode
  { state :: UserState a
  , configuration :: UserConfiguration a
  , _userFibers :: Ref (Array (Fiber Unit))
  }

derive instance Newtype (UserNode a) _
