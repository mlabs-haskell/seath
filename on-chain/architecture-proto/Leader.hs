{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Leader where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TQueue, TVar, atomically, flushTQueue, modifyTVar', newTQueueIO, newTVarIO, readTVarIO, swapTVar, writeTQueue)
import Control.Monad (forM_, void, when)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import SeathCore qualified as Seath
import Types
  ( ActionRequest (action, userId),
    SignedTx,
    SingRequest (SingRequest),
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
receiveAction leader@(Leader inb lim count opState _ _) ar = do
  st <- readTVarIO opState
  case st of
    Processing -> do
      putStrLn $ "Leader: PROCESSING IN PROGRESS: refused to accept action from " <> userId ar
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
          putStrLn $ "Leader: adding request to queue: " <> show ar
          atomically $ writeTQueue inb ar
          _ <- atomically $ do
            swapTVar count nextCount

          when (nextCount == lim) $ do
            switchToProcessing opState
            void $ forkIO $ processInbox leader

          pure $ Right ()

processInbox :: Leader -> IO ()
processInbox (Leader inb _ _ _ reqs hs) = do
  putStrLn "Leader: queue filled - processing actions"
  actionReqs <- atomically $ flushTQueue inb

  -- arch: run contract with `runContract` from CTL to do chaining
  chainedTxs <- Seath.runContract (Seath.actionsToTxChain (action <$> actionReqs))

  let toProcess = zipWith (\ar tx -> (userId ar, tx)) actionReqs chainedTxs

  forM_ toProcess $ \(uid, tx) -> do
    putStrLn $ "Leader: processing chained tx " <> show tx
    let signReq = SingRequest uid tx
    sendRes <- sendToUserToSign hs signReq
    case sendRes of
      Right _ -> do
        putStrLn $ "Leader: sending for signing - OK, user " <> uid
        atomically $ rememberRequest reqs uid signReq
      Left err ->
        putStrLn $
          "Leader: Failed to get signature from user " <> uid
            <> ". Error: "
            <> err
  printSuccessfullySigned reqs

switchToProcessing :: TVar State -> IO ()
switchToProcessing state = void $ atomically $ swapTVar state Processing

-- collect requests for which leader got signed transaction from user
rememberRequest :: TVar RequestMap -> UserId -> SingRequest -> STM ()
rememberRequest tv uId sr = modifyTVar' tv (\s -> s |> (uId, sr))

printSuccessfullySigned :: TVar RequestMap -> IO ()
printSuccessfullySigned reqs = do
  putStrLn "Leader got signatures for following requests:"
  reqs' <- readTVarIO reqs
  forM_ reqs' print
