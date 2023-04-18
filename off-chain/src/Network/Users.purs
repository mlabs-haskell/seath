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

import Contract.Transaction
  ( BalancedSignedTransaction
  , FinalizedTransaction(FinalizedTransaction)
  , Transaction
  )
import Control.Monad (bind)
import Data.Either (Either)
import Data.Function (($))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff (Aff, Fiber, delay, error, forkAff, throwError, try)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Seath.Core.Types (CoreConfiguration, UserAction)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.TxHex as TxHex
import Seath.Network.Types
  ( ActionStatus(AskForSignature)
  , IncludeActionError
  , SendSignedTransaction(SendSignedTransaction)
  , SignedTransaction
  , UserConfiguration
  , UserNode(UserNode)
  , UserState(UserState)
  )
import Seath.Network.Utils
  ( addToSentActions
  , addToTransactionsSent
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
  :: forall a. UserNode a -> UUID -> Aff ActionStatus
getActionStatus userNode =
  (getUserHandlers userNode).getActionStatus

signTx
  :: forall a. UserNode a -> Transaction -> Aff Transaction
signTx userNode tx = unwrap <$> (unwrap userNode).signTx (wrap tx)

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

-- | Return a new mutable `userState`
newUserState :: forall a. Aff $ UserState a
newUserState = undefined

performAction :: forall a. UserNode a -> a -> Aff Unit
performAction userNode action = do
  userAction <- (unwrap userNode).makeAction action
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
  => (a -> Aff (UserAction a))
  -> (FinalizedTransaction -> Aff BalancedSignedTransaction)
  -> UserConfiguration a
  -> Aff (Fiber Unit /\ UserNode a)
startUserNode makeActionH signTxH conf = do
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
      , makeAction: makeActionH
      , signTx: signTxH
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
