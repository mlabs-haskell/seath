module Seath.Network.Users
  ( getActionStatus
  , getSeathCoreConfiguration
  , newUserState
  , performAction
  , sendActionToLeader
  , sendRejectionToLeader
  , sendSignedTransactionToLeader
  , startUserNode
  ) where

import Contract.Prelude

import Contract.Transaction (FinalizedTransaction(FinalizedTransaction))
import Control.Monad (bind)
import Data.Either (Either)
import Data.Function (($))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, Fiber, delay, forkAff, try)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Seath.Core.Types (CoreConfiguration, UserAction)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.TxHex as TxHex
import Seath.Network.Types
  ( ActionStatus
  , GetStatusError
  , IncludeActionError
  , SignedTransaction
  , UserConfiguration
  , UserNode(UserNode)
  , UserState(UserState)
  , addToSentActions
  , getUserHandlers
  , readSentActions
  )
import Type.Function (type ($))
import Undefined (undefined)

getSeathCoreConfiguration
  :: forall actionType userStateType validatorType datumType redeemerType
   . UserNode actionType
  -> CoreConfiguration actionType userStateType validatorType datumType
       redeemerType
getSeathCoreConfiguration = undefined

-- | This function won't raise a exception if we can't reach the network.
sendActionToLeader
  :: forall a
   . UserNode a
  -> UserAction a
  -> Aff $ Either IncludeActionError UUID
sendActionToLeader userNode action =
  (getUserHandlers userNode).submitToLeader action

-- Query server for action status
getActionStatus
  :: forall a. UserNode a -> UUID -> Aff (Either GetStatusError ActionStatus)
getActionStatus userNode =
  (getUserHandlers userNode).getActionStatus

sendSignedTransactionToLeader
  :: forall a
   . UserNode a
  -> UUID
  -> SignedTransaction
  -- the first Either is to catch the network errors
  -> Aff Unit
sendSignedTransactionToLeader userNode uuid signedTx = do
  let
    (FinalizedTransaction tx) = unwrap signedTx

  cbor <- try $ TxHex.toCborHex tx
  case cbor of
    Left e -> log $ "User: could not serialize signed Tx to CBOR: " <> show e
    Right (cbor' :: String) -> do
      res <- try $ (getUserHandlers userNode).sendSignedToLeader
        (wrap { uuid: uuid, txCborHex: cbor' })
      case res of
        Left e -> log $ "User: failed to send signed Tx to leader: " <> show e
        Right (Left e) -> log $ "User: failed to send signed Tx to leader: " <>
          show e
        Right (Right _) -> log "USer: signet Tx sent seuccessfully"

-- | We refuse to sign the given transaction and inform the server
-- | explicitly.
sendRejectionToLeader
  :: forall a
   . UserNode a
  -> UUID
  -> Aff Unit
sendRejectionToLeader userNode uuid = do
  res <- try $ (getUserHandlers userNode).refuseToSign uuid
  log $ "User: refusing to sing " <> show uuid <> ", result: " <> show res

-- | Return a new mutable `userState`
newUserState :: forall a. Aff $ UserState a
newUserState = undefined

performAction :: forall a. UserNode a -> a -> Aff Unit
performAction userNode action = do
  userAction <- (unwrap userNode).makeAction action
  result <- userNode `sendActionToLeader` userAction
  case result of
    Left err -> log $ "TODO: React to error " <> show err
    Right uid -> do
      userNode `addToSentActions` (uid /\ action)

startUserNode
  :: forall a
   . Show a
  => (a -> Aff (UserAction a))
  -> UserConfiguration a
  -> Aff (Fiber Unit /\ UserNode a)
startUserNode makeAction conf = do
  actionsSent <- liftEffect $ Ref.new OrderedMap.empty
  transactionsSent <- liftEffect $ Ref.new OrderedMap.empty
  submitedTransactions <- liftEffect $ Ref.new OrderedMap.empty
  let
    node = UserNode
      { state: UserState
          { actionsSent
          , transactionsSent
          , submitedTransactions
          }
      , configuration: conf
      , makeAction: makeAction

      }
  fiber <- startActionStatusCheck node
  pure $ fiber /\ node

startActionStatusCheck :: forall a. Show a => UserNode a -> Aff (Fiber Unit)
startActionStatusCheck userNode = do
  log $ "Start checking actions"
  forkAff check
  where
  check = do
    sent <- readSentActions userNode
    for_ sent $ \(uid /\ _) -> do
      -- TODO: process response
      res <- getActionStatus userNode uid
      log $ "User: status of action " <> show uid <> ": " <> show res
      delay $ Milliseconds 500.0
    delay $ Milliseconds 2000.0
    check
