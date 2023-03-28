{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Leader where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TQueue, TVar, atomically, flushTQueue, modifyTVar', newTQueueIO, newTVarIO, readTVarIO, swapTVar, writeTQueue)
import Control.Monad (forM_, void, when)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Types
  ( ActionRequest (..),
    SignedTx,
    SingRequest (..),
    Tx,
    UserAction,
    UserId,
  )
import Prelude

data LeaderError
  = InboxLimitReached Int
  | CantAcceptDuringProcessing
  deriving stock (Show)

data State = Accepting | Processing

type RequestMap = Seq (UserId, SingRequest) -- some ordered map will be better, probably

data LeaderHandlers = LeaderHandlers
  { sendToUserToSign :: SingRequest -> IO (Either String SignedTx)
  }

data Leader = Leader
  { inbox :: TQueue ActionRequest,
    inboxLimit :: Int,
    msgCount :: TVar Int,
    operationState :: TVar State,
    reqMap :: TVar RequestMap,
    lHandlers :: LeaderHandlers
    
  }

newLeader :: LeaderHandlers -> IO Leader
newLeader hs = do
  inboxQueue <- newTQueueIO
  count <- newTVarIO 0
  state <- newTVarIO Accepting
  reqs <- newTVarIO Seq.empty
  pure $ Leader inboxQueue 3 count state reqs hs

receiveAction :: Leader -> ActionRequest -> IO (Either LeaderError ())
receiveAction leader@(Leader inb lim count opState _ hs) ar = do
  st <- readTVarIO opState
  case st of
    Processing -> do
      putStrLn $ "Leader: PROCESSING: Accept refused for " <> userId ar
      pure $ Left CantAcceptDuringProcessing
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
          putStrLn $ "Leader: Adding request to queue: " <> show ar
          atomically $ writeTQueue inb ar
          _ <- atomically $ do
            swapTVar count nextCount
          -- appendRequest reqs ar

          when (nextCount == lim) $ do
            _ <- atomically $ swapTVar opState Processing
            void $ forkIO $ processInbox leader

          pure $ Right ()

processInbox :: Leader -> IO ()
processInbox (Leader inb _ _ _ reqs hs) = do
  putStrLn "Leader: processing actions"
  actions <- atomically $ flushTQueue inb
  forM_ actions $ \ar -> do
    putStrLn $ "Leader: processing action " <> show ar
    let signReq = SingRequest (userId ar) (mkTx $ action ar)
    sendRes <- sendToUserToSign hs signReq
    case sendRes of
      Right _ -> do
        putStrLn $ "Leader: sending for signing - OK, user " <> userId ar
        atomically $ rememberRequest reqs (userId ar) signReq
      Left err ->
        putStrLn $
          "Leader: Failed to get signature from user " <> userId ar
            <> ". Error: "
            <> err
  printSuccessfullySigned reqs

-- sendToSign

rememberRequest :: TVar RequestMap -> UserId -> SingRequest -> STM ()
rememberRequest tv uId sr = modifyTVar' tv (\s -> s |> (uId, sr))

mkTx :: UserAction -> Tx
mkTx ua = "Tx-from-" <> ua

printSuccessfullySigned :: TVar RequestMap -> IO ()
printSuccessfullySigned reqs = do
  putStrLn "Leader got signatures for following requests:"
  reqs' <- readTVarIO reqs
  forM_ reqs' print