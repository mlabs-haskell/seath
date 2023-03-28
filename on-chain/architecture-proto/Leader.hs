

module Leader where

import Prelude
import Control.Concurrent.STM (TQueue, newTBQueueIO, newTQueueIO, TVar, newTVarIO, atomically, readTVarIO, writeTQueue, writeTVar, swapTVar)
import Types
import Control.Monad.IO.Class
import Control.Monad (when)

data LeaderError 
  = InboxLimitReached Int
  | CantAcceptDoingProcessing
  deriving stock Show

data State = Accepting | Processing

data Leader = Leader
  { inbox :: TQueue ActionRequest
  , inboxLimit :: Int
  , msgCount :: TVar Int
  , operationState :: TVar State
  }

newLeader :: IO Leader
newLeader = do
  inboxQueue <- newTQueueIO
  count <- newTVarIO 0
  state <- newTVarIO Accepting
  pure $ Leader inboxQueue 3 count state


receiveAction :: Leader -> ActionRequest -> IO (Either LeaderError ())
receiveAction (Leader inb lim count opState) ar = do
  st <- readTVarIO opState
  case st of
    Processing -> do
      putStrLn "Leader: Processing - accept refused"
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
          putStrLn "Adding Action to queue"
          atomically $ writeTQueue inb ar
          _ <- atomically $ swapTVar count nextCount

          when (nextCount == lim) $ do
            _ <- atomically $ swapTVar opState Processing
            processActions
          pure $ Right ()

processActions = putStrLn "Leader: processing actions"