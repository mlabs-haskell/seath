module Seath.Network.Users
  ( getActionStatus
  , newUserState
  , performAction
  , sendActionToLeader
  , sendRejectionToLeader
  , sendSignedTransactionToLeader
  , startUserNode
  ) where

import Contract.Prelude

import Contract.Transaction
  ( FinalizedTransaction(FinalizedTransaction)
  , Transaction
  , signTransaction
  )
import Control.Monad (bind)
import Data.Either (Either)
import Data.Function (($))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, Fiber, delay, error, forkAff, throwError, try)
import Effect.Class (liftEffect)
import Queue as Queue
import Effect.Ref as Ref
import Seath.Core.Types (UserAction)
import Seath.Core.Utils as Core.Utils
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.TxHex as TxHex
import Seath.Network.Types
  ( ActionStatus(AskForSignature)
  , FunctionToPerformContract(FunctionToPerformContract)
  , IncludeActionError
  , SendSignedTransaction(SendSignedTransaction)
  , SignedTransaction
  , UserConfiguration
  , UserNode
  , UserState
  )
import Seath.Network.Utils
  ( addToSentActions
  , addToTransactionsSent
  , getUserHandlers
  , readSentActions
  , userRunContract
  )
import Type.Function (type ($))

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
  :: forall a. UserNode a -> UUID -> Aff ActionStatus
getActionStatus userNode =
  (getUserHandlers userNode).getActionStatus

signTx
  :: forall a. UserNode a -> Transaction -> Aff Transaction
signTx userNode tx = do
  let (FunctionToPerformContract run) = userRunContract userNode
  signedTx <- run $ signTransaction (FinalizedTransaction tx)
  pure $ unwrap signedTx

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
        Right (Right _) -> log "User: signet Tx sent seuccessfully"

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

performAction :: forall a. UserNode a -> a -> Aff Unit
performAction userNode action = do
  let (FunctionToPerformContract run) = userRunContract userNode
  userAction <- run $ Core.Utils.makeActionContract action
  result <- try $ userNode `sendActionToLeader` userAction
  case result of
    Right (Right uid) -> do
      userNode `addToSentActions` (uid /\ action)
    Right (Left refused) ->
      log $ "User: leader refused to include action: " <> show refused
    Left err -> log $ "User: unexpected: failed to submit action to Leader " <>
      show err

startUserNode
  :: forall a
   . Show a
  => UserConfiguration a
  -> Aff (Fiber Unit /\ UserNode a)
startUserNode conf = do
  node <- newUserNode conf
  fiber <- startActionStatusCheck node
  pure $ fiber /\ node

newUserState :: forall a. Aff (UserState a)
newUserState = do
  actionsSentQueue <- liftEffect $ Queue.new
  Queue.on actionsSentQueue actionsSentHandler 
  actionsSent <- liftEffect $ Ref.new OrderedMap.empty
  transactionsSentQueue <- liftEffect $ Queue.new
  Queue.on transactionsSentHandler transactionsSentHandler
  transactionsSent <- liftEffect $ Ref.new OrderedMap.empty
  pure $ wrap
    { 
    actionsSentQueue
    ,actionsSent
    ,transactionsSentQueue
    , transactionsSent
    }

actionsSentHandler :: forall a . {uuid::UUID, action::UserAction a, status::ActionStatus} -> Effect Unit
actionsSentHandler record =
  case record.status of 
    _ -> undefined

transactionsSentHandler :: forall a . {uuid::UUID, action::UserAction a, status::ActionStatus} ->Effect Unit
transactionsSentHandler = undefined

newUserNode :: forall a. UserConfiguration a -> Aff (UserNode a)
newUserNode conf = do
  state <- newUserState
  pure $ wrap
    { state: state
    , configuration: conf
    }

startActionStatusCheck :: forall a. Show a => UserNode a -> Aff (Fiber Unit)
startActionStatusCheck userNode = do
  log $ "Start checking actions"
  forkAff check
  where
  check = do
    sent <- readSentActions userNode
    for_ sent $ \entry -> do
      -- TODO: process response
      res <- try $ checkStatusAndProcess entry
      case res of
        Right _ -> pure unit
        {- TODO: Many possible errors can be caught here:
         - error during status request to the leader over HTTP (e.g. leader is offline)
         - can't parse UUID returned by the leader
         - JSON decode errors
         - error during encoding and deconding transaction CBOR
         - error while running signing Contract
         - error while sending signed Tx to the leader over HTTP
         - AcceptSignedTransactionError

         Not sure atm how granular error handler need to be
        -}
        Left e -> log $ "User: failed to check status for " <> show (fst entry)
          <> ": "
          <> show e
    delay $ Milliseconds 500.0
    check
  checkStatusAndProcess (uid /\ action) = do
    status <- getActionStatus userNode uid
    case status of
      AskForSignature afs -> do
        tx <- TxHex.fromCborHex afs.txCborHex
        signedTx <- signTx userNode tx
        signedCbor <- TxHex.toCborHex signedTx

        let
          singedRequest = SendSignedTransaction
            { uuid: uid, txCborHex: signedCbor }
        result <- (getUserHandlers userNode).sendSignedToLeader singedRequest
        case result of
          Right _ -> do
            addToTransactionsSent userNode (uid /\ action)
            log $ "User: Successfully signed and sent Tx " <> show uid
              <> " to the Leader"
          Left e -> throwError (error $ show e)

      other -> log $ "User: status for action " <> show uid <> ": " <> show
        other
