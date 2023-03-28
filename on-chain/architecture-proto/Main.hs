module Main (main) where

import Prelude
import Leader (newLeader, receiveAction, LeaderHandlers (LeaderHandlers), sendToUserToSign)
import Types

main :: IO ()
main = do
  putStrLn "Start test"

  let leaderHandlers = LeaderHandlers {
    sendToUserToSign = \a -> pure (Right ())
  }

  leader <- newLeader leaderHandlers
  leader `receiveAction` (newAction "localhost:8090" "")
  leader `receiveAction` (newAction "localhost:8090" "")
  leader `receiveAction` (newAction "localhost:8090" "")
  leader `receiveAction` (newAction "localhost:8090" "")

  putStrLn "Test end"
