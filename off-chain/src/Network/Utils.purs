module Seath.Network.Utils
  ( actionToTriggerChainBuild
  , getChaintriggerTreshold
  , getFromLeaderConfiguration
  , getFromLeaderState
  , getFromRefAtLeaderState
  , getNetworkHandlers
  , getNumberOfPending
  , getPublicKeyHash
  , isAnotherActionInProcess
  , lookupActionsSent
  , modifyActionsSent
  , pushRefMap_
  , putToResults
  , readResults
  , readSentActions
  , setToRefAtLeaderState
  , signTimeout
  , takeFromPending
  , userRunContract
  , userWithRefFromState
  , withRefFromState
  ) where

import Contract.Prelude

import Contract.Address (PubKeyHash, getWalletAddresses, toPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Array as Array
import Data.Either (Either(Left))
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Ord (class Ord)
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Queue as Queue
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.Types
  ( ActionResult
  , ActionStatus
  , LeaderConfiguration
  , LeaderConfigurationInner
  , LeaderNode(LeaderNode)
  , LeaderStateInner
  , NetworkHandlers
  , RunContract
  , UserNode(UserNode)
  , UserStateInner
  )

getPublicKeyHash :: Contract PubKeyHash
getPublicKeyHash = do
  address <- liftedM "can't get the address of KeyWallet" $ head <$>
    getWalletAddresses
  liftMaybe (error "can't get pubKeyHash of KeyWallet") $ toPubKeyHash address

-- Leader ----------------------------------------------------------------------
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

-- TODO: Move User to their own folder, add a reader monad for it and 
-- move this functions there (same for Leader).
-- USER ----------------------------------------------------------------------
userWithRefFromState
  :: forall a b c
   . UserNode a
  -> (UserStateInner a -> Ref b)
  -> (Ref b -> Effect c)
  -> Aff c
userWithRefFromState (UserNode node) acessor f = liftEffect $ f $ acessor
  (unwrap node.state)

lookupActionsSent
  :: forall a
   . UserNode a
  -> UUID
  -> Aff (Maybe (UserAction a /\ ActionStatus))
lookupActionsSent userNode uuid = do
  _map <- userWithRefFromState userNode _.actionsSent Ref.read
  pure $ OrderedMap.lookup uuid _map

modifyActionsSent
  :: forall a
   . UserNode a
  -> ( OrderedMap UUID (UserAction a /\ ActionStatus)
       -> OrderedMap UUID (UserAction a /\ ActionStatus)
     )
  -> Aff Unit
modifyActionsSent userNode transform =
  userWithRefFromState userNode _.actionsSent (Ref.modify_ transform)

putToResults
  :: forall a
   . UserNode a
  -> ActionResult a
  -> Aff Unit
putToResults userNode result =
  let
    resultsQueue = (unwrap (unwrap userNode).state).resultsQueue
  in
    liftEffect $ Queue.put resultsQueue result

readResults
  :: forall a
   . UserNode a
  -> Aff (Array (ActionResult a))
readResults userNode =
  let
    resultsQueue = (unwrap (unwrap userNode).state).resultsQueue
  in
    liftEffect $ Queue.read resultsQueue

userRunContract
  :: forall a. UserNode a -> RunContract
userRunContract (UserNode node) =
  (unwrap node.configuration).runContract

getNumberOfPending :: forall a. LeaderNode a -> Aff Int
getNumberOfPending ln = do
  numberOfPrioritary <- OrderedMap.length <$> getFromRefAtLeaderState ln
    _.prioritaryPendingActions
  numberOfPending <- liftEffect $ Queue.length $ getFromLeaderState ln
    _.pendingActionsRequest
  pure $ numberOfPrioritary + numberOfPending

signTimeout :: forall a. LeaderConfiguration a -> Int
signTimeout conf = (unwrap conf).maxWaitingTimeForSignature

actionToTriggerChainBuild :: forall a. LeaderConfiguration a -> Int
actionToTriggerChainBuild conf =
  (unwrap conf).numberOfActionToTriggerChainBuilder

getChaintriggerTreshold :: forall a. LeaderNode a -> Int
getChaintriggerTreshold (LeaderNode node) =
  (unwrap node.configuration).numberOfActionToTriggerChainBuilder

readSentActions
  :: forall a
   . UserNode a
  -> Aff (Array (UUID /\ (UserAction a /\ ActionStatus)))
readSentActions (UserNode node) = liftEffect $
  OrderedMap.orderedElems <$> Ref.read (unwrap node.state).actionsSent

getNetworkHandlers :: forall a. UserNode a -> NetworkHandlers a
getNetworkHandlers (UserNode node) = (unwrap node.configuration).networkHandlers

pushRefMap_
  :: forall k v. Ord k => k -> v -> Ref (OrderedMap k v) -> Effect Unit
pushRefMap_ k v mutMap =
  Ref.modify_
    (OrderedMap.push k v)
    mutMap

isAnotherActionInProcess
  :: forall a. LeaderNode a -> UserAction a -> Aff Boolean
isAnotherActionInProcess ln ua = do
  inPending <- checkPending
  inPrioritaryPending <- checkPrioritaryPending
  inProcessing <- checkProcessing
  inReceived <- checkReceived
  pure $ inPending || inPrioritaryPending || inProcessing || inReceived
  where
  checkReceived = do
    received <- liftEffect $ Queue.read $ getFromLeaderState ln
      _.receivedActionsRequests
    pure $ (flip Array.any) received $
      \a -> case a of
        Right (action /\ _)
        -> (unwrap action).publicKey == (unwrap ua).publicKey
        _ -> false

  checkPending = do
    pending <- liftEffect $ Queue.read $ getFromLeaderState ln
      _.pendingActionsRequest
    pure $ (flip Array.any) pending check

  checkPrioritaryPending = do
    prioritaryPending <- liftEffect $ Ref.read $ getFromLeaderState ln
      _.prioritaryPendingActions
    pure $ (flip Array.any) (OrderedMap.toArray prioritaryPending) check

  checkProcessing = do
    processing <- liftEffect $ Ref.read $ getFromLeaderState ln _.processing
    pure $ (flip Array.any) (OrderedMap.toArray processing) check

  check (_uuid /\ action) = (unwrap action).publicKey == (unwrap ua).publicKey
