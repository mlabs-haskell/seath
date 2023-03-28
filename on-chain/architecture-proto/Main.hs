{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
module Main (main) where

import Control.Arrow (ArrowChoice (left))
import Control.Concurrent
  ( Chan,
    MVar,
    forkIO,
    newChan,
    newEmptyMVar,
    putMVar,
    readChan,
    takeMVar,
    threadDelay,
    writeChan,
  )
import Control.Monad (forM_, forever, void)
import Data.Map (Map)
import Data.Map qualified as Map
import Leader
  ( LeaderHandlers (LeaderHandlers),
    newLeader,
    receiveAction,
    sendToUserToSign,
  )
import Types (ActionRequest, SignedTx, SingRequest (..), UserId)
import User
  ( User,
    UserHandlers (UserHandlers),
    acceptSignReq,
    newUser,
    sendAction,
    sendActionToLeader,
    uHandlers,
    uId,
  )
import Prelude

main :: IO ()
main = do
  putStrLn "Start test"
  let userIds = ["user-" <> show x | x <- [1 .. 4]]
  net <- mkNetwork userIds

  let leaderHandlers =
        LeaderHandlers
          { sendToUserToSign = _sendSignToUser net
          }
  leader <- newLeader leaderHandlers

  let userHandlers =
        UserHandlers
          { sendActionToLeader = _sendActionToLeader net
          }

  users <- traverse (newUser userHandlers) userIds

  startLeaderThread net leader

  forM_ users (startUserThread net)
  forM_ users (`sendAction` "Test action")

  letItRunFor 5

  putStrLn "Test end"
  where
    startLeaderThread net leader = do
      void $
        forkIO $
          forever $ do
            req <- readChan (toLeader net)
            case req of
              AcceptActionReq sender ar -> do
                res <- left show <$> receiveAction leader ar
                putMVar sender (AcceptActionResp res)
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
        case req of
          SignTxReq forResponse sr ->
            acceptSignReq (uHandlers u) u sr
              >>= putMVar forResponse . SignTxResp
          other -> error $ "leader got unexpected request: " <> show other
      pure ()

letItRunFor :: Int -> IO ()
letItRunFor n = threadDelay (n * 1000000)

_sendActionToLeader :: Network -> ActionRequest -> IO (Either String ())
_sendActionToLeader (Network leadChan _) ar = do
  response <- newEmptyMVar
  writeChan leadChan (AcceptActionReq response ar)
  resp <- takeMVar response
  case resp of
    AcceptActionResp r -> pure r
    other -> error $ "Unexpected response while sending action to leader: " <> show other

_sendSignToUser :: Network -> SingRequest -> IO (Either String SignedTx)
_sendSignToUser (Network _ userMap) sr = do
  let netUserId = let (SingRequest uid _) = sr in uid
  userChan <-
    maybe
      (error "User not found in network")
      pure
      (Map.lookup netUserId userMap)
  forResponse <- newEmptyMVar
  writeChan userChan (SignTxReq forResponse sr)
  resp <- takeMVar forResponse
  case resp of
    SignTxResp signedTx -> pure signedTx
    other -> error $ "Unexpected response while asking user to sign: " <> show other

instance Show (MVar NetMessage) where
  show _ = "<sender>"

-- requests has MVar for response to be put there
data NetMessage
  = AcceptActionReq (MVar NetMessage) ActionRequest
  | AcceptActionResp (Either String ())
  | SignTxReq (MVar NetMessage) SingRequest
  | SignTxResp (Either String SignedTx)
  deriving stock (Show)

data Network = Network
  { toLeader :: Chan NetMessage,
    toUsers :: Map UserId (Chan NetMessage)
  }

mkNetwork :: [UserId] -> IO Network
mkNetwork uids = do
  leaderChan <- newChan
  uss <- traverse mkUserConn uids
  pure $ Network leaderChan (Map.fromList uss)
  where
    mkUserConn uid = (uid,) <$> newChan
