module Seath.Network.Utils
  ( getPublicKeyHash
  , getFromRefAtLeaderState
  , getFromLeaderState
  , setToRefAtLeaderState
  , withRefFromState
  , takeFromPending
  , getFromLeaderConfiguration
  , getNumberOfPending
  , maxPendingCapacity
  , signTimeout
  , getChaintriggerTreshold
  , addToSentActions
  , addToTransactionsSent
  , readSentActions
  , getUserHandlers
  , pushRefMap_
  ) where

import Contract.Address (PubKeyHash, getWalletAddresses, toPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Control.Applicative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Array as Array
import Data.Either (Either(Left))
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.Newtype (unwrap)
import Data.Ord (class Ord)
import Data.Semiring ((+))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prelude (discard)
import Queue as Queue
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.Types
  ( LeaderConfiguration
  , LeaderConfigurationInner
  , LeaderNode(LeaderNode)
  , LeaderStateInner
  , UserHandlers
  , UserNode(UserNode)
  )

getPublicKeyHash :: Contract PubKeyHash
getPublicKeyHash = do
  address <- liftedM "can't get the address of KeyWallet" $ head <$>
    getWalletAddresses
  liftMaybe (error "can't get pubKeyHash of KeyWallet") $ toPubKeyHash address

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
takeFromPending n ln = do
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

maxPendingCapacity :: forall a. LeaderConfiguration a -> Int
maxPendingCapacity conf = (unwrap conf).maxQueueSize

signTimeout :: forall a. LeaderConfiguration a -> Int
signTimeout conf = (unwrap conf).maxWaitingTimeForSignature

getChaintriggerTreshold :: forall a. LeaderNode a -> Int
getChaintriggerTreshold (LeaderNode node) =
  (unwrap node.configuration).numberOfActionToTriggerChainBuilder

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
