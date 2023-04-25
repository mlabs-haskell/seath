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
  , TransactionHash
  , signTransaction
  )
import Control.Monad (bind)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either)
import Data.Function (($))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect.Aff
  ( Aff
  , Fiber
  , delay
  , error
  , forkAff
  , launchAff_
  , throwError
  , try
  )
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effect.Ref as Ref
import Queue as Queue
import Seath.Core.Types (UserAction)
import Seath.Core.Utils as Core.Utils
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.TxHex as TxHex
import Seath.Network.Types
  ( ActionStatus
      ( AskForSignature
      , ToBeProcessed
      , ToBeSubmitted
      , Processing
      , WaitingOtherChainSignatures
      , PrioritaryToBeProcessed
      , Submitted
      , NotFound
      )
  , FunctionToPerformContract(FunctionToPerformContract)
  , IncludeActionError
  , SignedTransaction
  , UserConfiguration
  , UserNode
  , UserState
  )
import Seath.Network.Utils
  ( getNetworkHandlers
  , modifyActionsSent
  , putToResults
  , readSentActions
  , userRunContract
  )
import Type.Function (type ($))

-- TODO: is this afirmation true? (the comment was there before the 
-- implementation).
-- | This function won't raise a exception if we can't reach the network.
sendActionToLeader
  :: forall a
   . UserNode a
  -> UserAction a
  -> Aff $ Either IncludeActionError UUID
sendActionToLeader userNode action =
  (getNetworkHandlers userNode).submitToLeader action

checkChainedTx
  :: forall a. UserNode a -> Transaction -> Aff (Either String Transaction)
checkChainedTx userNode =
  (unwrap (unwrap userNode).configuration).checkChainedTx

-- Query server for action status
getActionStatus
  :: forall a. UserNode a -> UUID -> Aff ActionStatus
getActionStatus userNode =
  (getNetworkHandlers userNode).getActionStatus

signTx
  :: forall a. UserNode a -> Transaction -> Aff (Either String Transaction)
signTx userNode tx = do
  let (FunctionToPerformContract run) = userRunContract userNode
  bimap show (unwrap) <$>
    (try $ run $ signTransaction (FinalizedTransaction tx))

sendSignedTransactionToLeader
  :: forall a
   . UserNode a
  -> UUID
  -> SignedTransaction
  -- the first Either is to catch the network errors
  -> Aff (Maybe String)
sendSignedTransactionToLeader userNode uuid signedTx = do
  let
    (FinalizedTransaction tx) = unwrap signedTx

  cbor <- try $ TxHex.toCborHex tx
  case cbor of
    Left e -> do
      let msg = "User: could not serialize signed Tx to CBOR: " <> show e
      log msg
      pure $ pure msg
    Right (cbor' :: String) -> do
      res <- try $ (getNetworkHandlers userNode).sendSignedToLeader
        (wrap { uuid: uuid, txCborHex: cbor' })
      case res of
        Left e -> do
          let msg = "User: failed to send signed Tx to leader: " <> show e
          log $ msg
          pure $ pure msg
        Right (Left e) -> do
          let msg = "User: failed to send signed Tx to leader: " <> show e
          log $ msg
          pure $ pure msg
        Right (Right _) -> do
          log "User: signet Tx sent successfully"
          pure Nothing

-- | We refuse to sign the given transaction and inform the server
-- | explicitly.
sendRejectionToLeader
  :: forall a
   . UserNode a
  -> UUID
  -> Aff Unit
sendRejectionToLeader userNode uuid = do
  res <- try $ (getNetworkHandlers userNode).refuseToSign uuid
  log $ "User: refusing to sing " <> show uuid <> ", result: " <> show res

-- | The right function to begin the process of a new action (don't use `sendActionToLeader`)
performAction :: forall a. UserNode a -> a -> Aff Unit
performAction userNode action = do
  let (FunctionToPerformContract run) = userRunContract userNode
  userAction <- run $ Core.Utils.makeActionContract action
  result <- try $ userNode `sendActionToLeader` userAction
  case result of
    Right (Right uuid) ->
      let
        actionsSentQueue = (unwrap (unwrap userNode).state).actionsSentQueue
      in
        liftEffect $ Queue.put actionsSentQueue
          { uuid
          , action: userAction
          , status: ToBeProcessed (-1)
          , previousStatus: ToBeProcessed (-1)
          }
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
  actionsSent <- liftEffect $ Ref.new OrderedMap.empty
  resultsQueue <- liftEffect $ Queue.new
  pure $ wrap
    { actionsSentQueue
    , actionsSent
    , resultsQueue
    }

singTransactionAndSend
  :: forall a
   . Show a
  => UserNode a
  -> { uuid :: UUID
     , txCborHex :: String
     , action :: UserAction a
     , previousStatus :: ActionStatus
     }
  -> Aff (Maybe String)
singTransactionAndSend userNode afs = do
  result <- try $ do
    signedTx <-
      TxHex.fromCborHex afs.txCborHex
        >>= checkChained
        >>= signChecked
    sendSignedTransactionToLeader userNode afs.uuid (wrap $ wrap signedTx)
  case result of
    Left err -> pure $ pure (message err)
    Right (Just msg) -> pure $ pure msg
    Right Nothing -> pure Nothing
  where
  checkChained tx = do
    res <- checkChainedTx userNode tx
    case res of
      Left refused -> do
        sendRejectionToLeader userNode afs.uuid
        modifyActionsSent userNode (OrderedMap.delete afs.uuid)
        throwError (error refused)
      Right checkedTx -> pure checkedTx

  signChecked tx = signTx userNode tx >>= either (error >>> throwError) pure

{- TODO: Many possible errors can be caught here:
 - error during status request to the leader over HTTP (e.g. leader is offline)
 - can't parse UUID returned by the leader
 - JSON decode errors
 - error during encoding and deconding transaction CBOR
 - error while running signing Contract
 - error while sending signed Tx to the leader over HTTP
 - AcceptSignedTransactionError

 Not sure atm how granular error handler need to be.
 For now, we put a string with the error.
-}
-- | This is intended to be the only function that manipulates `actionsSent` in the 
-- | `UserConfiguration`
makeActionsSentHandler
  :: forall a
   . Show a
  => UserNode a
  -> { uuid :: UUID
     , action :: UserAction a
     , status :: ActionStatus
     , previousStatus :: ActionStatus
     }
  -> Effect Unit
makeActionsSentHandler userNode record = launchAff_ $
  case record.status of
    -- This must block the thread, otherwise if we allow `actionHandler` to spawn
    -- threads, we would face race conditions again.
    AskForSignature afs -> do
      result <- singTransactionAndSend userNode
        { uuid: afs.uuid
        , txCborHex: afs.txCborHex
        , action: record.action
        , previousStatus: record.previousStatus
        }
      case result of
        -- We don't have a state to represent `sent but we haven't ask the server to update the state`.
        Nothing -> modifyActionsSent userNode
          (OrderedMap.push record.uuid (record.action /\ record.status))
        Just msg -> putToResults userNode $ makeResult (Left msg)
    Submitted txH -> do
      putToResults userNode $ makeResult (Right txH)
      modifyActionsSent userNode (OrderedMap.delete record.uuid)
    NotFound ->
      case record.previousStatus of
        AskForSignature _ ->
          putFailure "Send signature but can't find it"
        ToBeProcessed _ ->
          putFailure "Was in processing but can't find it"
        ToBeSubmitted _ ->
          putFailure "Was in submission but can't find it"
        Processing ->
          putFailure "Was in a batch to process but can't find it"
        WaitingOtherChainSignatures _ ->
          putFailure "Was waiting to end of signature cycle but can't find it"
        PrioritaryToBeProcessed _ ->
          putFailure "Was in prioritary queue but can't find it"
        Submitted txH -> do
          putToResults userNode $ makeResult (Right txH)
          modifyActionsSent userNode (OrderedMap.delete record.uuid)
        NotFound -> do
          putToResults userNode $ makeResult (Left "NotFound twice")
          modifyActionsSent userNode (OrderedMap.delete record.uuid)
    other -> do
      log $ "User: status for action " <> show record.uuid <> ": " <> show other
      modifyActionsSent userNode
        (OrderedMap.push record.uuid (record.action /\ record.status))
  where
  makeResult
    :: Either String TransactionHash
    -> { uuid :: UUID
       , action :: UserAction a
       , status :: Either String TransactionHash
       }
  makeResult result =
    { uuid: record.uuid, action: record.action, status: result }

  putFailure :: String -> Aff Unit
  putFailure msg = do
    putToResults userNode $ makeResult (Left msg)
    modifyActionsSent userNode (OrderedMap.delete record.uuid)

newUserNode :: forall a. Show a => UserConfiguration a -> Aff (UserNode a)
newUserNode conf = do
  state <- newUserState
  let
    userNode = wrap
      { state: state
      , configuration: conf
      }
  liftEffect $ Queue.on (unwrap state).actionsSentQueue $ makeActionsSentHandler
    userNode
  pure $ userNode

startActionStatusCheck :: forall a. Show a => UserNode a -> Aff (Fiber Unit)
startActionStatusCheck userNode = do
  log $ "Start checking actions"
  forkAff check
  where
  check = do
    sent <- readSentActions userNode
    for_ sent $ \(uuid /\ action /\ previousStatus) -> do
      -- TODO: push to Queue
      res <- try $ getActionStatus userNode uuid
      case res of
        Right status ->
          let
            actionsSentQueue = (unwrap (unwrap userNode).state).actionsSentQueue
          in
            liftEffect $ Queue.put actionsSentQueue
              { uuid, action, status, previousStatus }
        Left e -> log $ "User: failed to check status for " <> show uuid
          <> ": "
          <> show e
    delay $ Milliseconds 500.0
    check
