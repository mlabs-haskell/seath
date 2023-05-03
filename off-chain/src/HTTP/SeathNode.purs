module Seath.HTTP.SeathNode (SeathNode, start, stop) where

import Contract.Prelude

import Aeson (class DecodeAeson)
import Effect.Aff (error, throwError)
import Payload.Server as Payload
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.Network.Leader (startLeaderNode, stopLeaderNode)
import Seath.Network.Types
  ( LeaderConfiguration
  , LeaderNode
  , UserConfiguration
  , UserNode
  )
import Seath.Network.Users (startUserNode, stopUserNode)

newtype SeathNode a = SeathNode
  { leaderNode :: LeaderNode a
  , userNode :: UserNode a
  , server :: Payload.Server
  }

start
  :: forall a
   . Show a
  => DecodeAeson a
  => SeathServerConfig
  -> LeaderConfiguration a
  -> UserConfiguration a
  -> Aff (SeathNode a)
start serverConf leaderConf userConf = do
  leaderNode <- startLeaderNode leaderConf
  userNode <- startUserNode userConf
  startServer <- Server.runServer serverConf leaderNode

  case startServer of
    Right server -> do
      log "Seath node started"
      pure $ SeathNode
        { leaderNode
        , userNode
        , server
        }
    Left errMsg -> do
      log "Failed to start Seath node sever - cleaning up fibers"
      stopLeaderNode leaderNode
      stopUserNode userNode
      log "Cleaning up complete"
      throwError (error $ "Failed to start Seath node: " <> errMsg)

stop :: forall a. SeathNode a -> Aff Unit
stop (SeathNode node) = do
  Payload.close node.server
  stopLeaderNode node.leaderNode
  stopUserNode node.userNode
