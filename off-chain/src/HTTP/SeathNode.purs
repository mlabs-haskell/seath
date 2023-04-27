module Seath.HTTP.SeathNode (SeathNode, start, stop) where

import Contract.Prelude

import Aeson (class DecodeAeson)
import Effect.Aff (Fiber, error, killFiber, throwError)
import Payload.Server as Payload
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.Network.Leader (startLeaderNode)
import Seath.Network.Types
  ( LeaderConfiguration
  , LeaderNode
  , UserConfiguration
  , UserNode
  )
import Seath.Network.Users (startUserNode)

newtype SeathNode a = SeathNode
  { leaderNode :: LeaderNode a
  , userNode :: UserNode a
  , fibers :: Array (Fiber Unit)
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
  (leaderFiber /\ leaderNode) <- do
    (leaderFiber /\ leaderNode) <- startLeaderNode leaderConf
    pure $ (leaderFiber /\ leaderNode)

  (userFiber /\ userNode) <- startUserNode userConf

  startServer <- Server.runServer serverConf leaderNode

  let fibers = [ leaderFiber, userFiber ]
  case startServer of
    Right server -> do
      log "Seath node started"
      pure $ SeathNode
        { leaderNode
        , userNode
        , fibers
        , server
        }
    Left errMsg -> do
      log "Failed to start Seath node sever - cleaning up fibers"
      for_ fibers $ killFiber
        (error "Could not kill Seath node processes correctly")
      log "Cleaning up complete"
      throwError (error $ "Failed to start Seath node: " <> errMsg)

stop :: forall a. SeathNode a -> Aff Unit
stop (SeathNode node) = do
  Payload.close node.server
  for_ node.fibers $ killFiber
    (error "Could not kill Seath node processes correctly")
