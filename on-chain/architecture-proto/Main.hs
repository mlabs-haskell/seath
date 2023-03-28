module Main (main) where

import Prelude
import Leader (newLeader, receiveAction)
import Types

main :: IO ()
main = do
  putStrLn "Start test"
  leader <- newLeader
  leader `receiveAction` (newAction "localhost:8090" "")
  leader `receiveAction` (newAction "localhost:8090" "")
  leader `receiveAction` (newAction "localhost:8090" "")
  leader `receiveAction` (newAction "localhost:8090" "")

  putStrLn "Test end"
