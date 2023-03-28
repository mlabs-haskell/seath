{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
module Main (main) where

import Control.Arrow (ArrowChoice (left))
import Control.Concurrent
  ( forkIO,
    newChan,
    putMVar,
    readChan,
    threadDelay,
  )
import Control.Monad (forM_, forever, void)
import Data.Map qualified as Map
import Handlers qualified
import Leader
  ( LeaderHandlers (LeaderHandlers),
    newLeader,
    receiveAction,
    sendToUserToSign,
  )
import Types
  ( NetMessage (AcceptActionReq, AcceptActionResp, SignTxReq, SignTxResp),
    Network (Network),
    UserId,
    toLeader,
  )
import User
  ( User (uId),
    UserHandlers (UserHandlers),
    emitAction,
    newUser,
    sendActionToLeader,
    signChainedTx,
    uId,
  )
import Prelude

main :: IO ()
main = do
  putStrLn "Start test"
  let userIds = ["user-" <> show x | x <- [1 .. 4]]
  -- make "network simulation"
  net <- mkNetwork userIds

  -- arch: make handlers for dependency injection for leader
  let leaderHandlers =
        LeaderHandlers
          -- arch: this will be done with some http-library for requests
          { sendToUserToSign = Handlers._sendToUserToSign net
          }
  -- arch: build wrapper for bunch of mutable stuff for the leader
  leader <- newLeader leaderHandlers

  -- arch: make handlers for dependency injection for users
  let userHandlers =
        UserHandlers
          -- arch: this will be done with some http-library for requests
          { sendActionToLeader = Handlers._sendActionToLeader net
          }
  -- arch: build wrappers for users (they don't have bunch of mutable stuff now, but probably will)
  users <- traverse (newUser userHandlers) userIds

  -- starting fake servers and emitting actions from users
  -- actions will be sent via handler to the leader under the user's hood
  -- thorough the "network"
  startLeaderThread net leader

  forM_ users (startUserThread net)

  forM_ users (\u -> emitAction u $ "Test action by " <> uId u)

  -- let main thread run for some seconds while leader and users exchanging actions
  letItRunFor 5
  putStrLn "Test end"
  where
    startLeaderThread net leader = do
      void $
        forkIO $
          forever $ do
            req <- readChan (toLeader net)
            -- arch: this will be API endpoints for leader web-server
            case req of
              AcceptActionReq forResponse ar -> do
                res <- left show <$> receiveAction leader ar
                putMVar forResponse (AcceptActionResp res)
              other -> error $ "leader got unexpected request: " <> show other

    startUserThread :: Network -> User -> IO ()
    startUserThread (Network _ userMap) u = do
      userChan <-
        maybe
          (error "User not found in network")
          pure
          (Map.lookup (uId u) userMap)
      void . forkIO . forever $ do
        req <- readChan userChan
        -- arch: this will be API endpoints for user web-server
        case req of
          SignTxReq forResponse sr ->
            signChainedTx u sr
              >>= putMVar forResponse . SignTxResp
          other -> error $ "leader got unexpected request: " <> show other
      pure ()

letItRunFor :: Int -> IO ()
letItRunFor seconds = threadDelay (seconds * 1000000)

mkNetwork :: [UserId] -> IO Network
mkNetwork uids = do
  leaderChan <- newChan
  uss <- traverse mkUserConn uids
  pure $ Network leaderChan (Map.fromList uss)
  where
    mkUserConn uid = (uid,) <$> newChan
