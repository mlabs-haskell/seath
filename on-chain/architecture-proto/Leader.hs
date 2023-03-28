module Leader where

import Control.Concurrent.STM (STM, TQueue, TVar, atomically, flushTQueue, modifyTVar', newTBQueueIO, newTQueueIO, newTVarIO, readTVarIO, swapTVar, writeTQueue, writeTVar)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Types
import Prelude

data LeaderError
  = InboxLimitReached Int
  | CantAcceptDoingProcessing
  deriving stock (Show)

data State = Accepting | Processing

type RequestMap = Seq (UserId, SingRequest) -- some ordered map will be better, probably

data LeaderHandlers = LeaderHandlers
  { sendToUserToSign :: SingRequest -> IO (Either String ())
  }

data Leader = Leader
  { inbox :: TQueue ActionRequest,
    inboxLimit :: Int,
    msgCount :: TVar Int,
    operationState :: TVar State,
    reqMap :: TVar RequestMap,
    handlers :: LeaderHandlers
  }

newLeader :: LeaderHandlers -> IO Leader
newLeader hs = do
  inboxQueue <- newTQueueIO
  count <- newTVarIO 0
  state <- newTVarIO Accepting
  reqs <- newTVarIO Seq.empty
  pure $ Leader inboxQueue 3 count state reqs hs

receiveAction :: Leader -> ActionRequest -> IO (Either LeaderError ())
receiveAction leader@(Leader inb lim count opState reqs _) ar = do
  st <- readTVarIO opState
  case st of
    Processing -> do
      putStrLn "Leader: Accept refused - processing"
      pure $ Left CantAcceptDoingProcessing
    Accepting -> acceptAction
  where
    acceptAction = do
      currentCount <- readTVarIO count
      let !nextCount = succ currentCount
      if nextCount > lim
        then do
          putStrLn "Leader: Inbox limit reached"
          pure $ Left (InboxLimitReached lim)
        else do
          putStrLn "Leader: Adding Action to queue"
          atomically $ writeTQueue inb ar
          _ <- atomically $ do
            swapTVar count nextCount
          -- appendRequest reqs ar

          when (nextCount == lim) $ do
            _ <- atomically $ swapTVar opState Processing
            processInbox leader
          pure $ Right ()

processInbox leader@(Leader inb lim count opState reqs hs) = do
  putStrLn "Leader: processing actions"
  actions <- atomically $ flushTQueue inb
  forM_ actions $ \ar -> do
    putStrLn $ "Leader: processing action " <> show ar
    let signReq = SingRequest $ mkTx $ action ar
    sendRes <- sendToUserToSign hs signReq
    case sendRes of
      Right _ -> do 
        putStrLn $ "Leader: sending for signing - OK, user " <> userId ar
        atomically $ rememberRequest reqs (userId ar) signReq
      Left err ->
        putStrLn $
          "Failed to send sign request to user " <> userId ar
            <> ". Error: "
            <> err

-- sendToSign

rememberRequest :: TVar RequestMap -> UserId -> SingRequest -> STM ()
rememberRequest tv uId sr = modifyTVar' tv (\s -> s |> (uId, sr))

mkTx :: UserAction -> Tx
mkTx ua = "Tx-from-" <> ua