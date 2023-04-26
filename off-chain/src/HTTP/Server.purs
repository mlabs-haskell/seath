module Seath.HTTP.Server where

import Prelude

import Aeson (class DecodeAeson)
import Data.Either (Either)
import Effect.Aff (Aff)
import Payload.Server (Server)
import Payload.Server as Payload
import Seath.HTTP.ServerHandlers as Handlers
import Seath.HTTP.Spec as Spec
import Seath.Network.Types (LeaderNode)

type SeathServerConfig = {}

runServer
  :: forall a
   . DecodeAeson a
  => Show a
  => SeathServerConfig
  -> LeaderNode a
  -> Aff (Either String Server)
runServer _ leaderNode = Payload.start_
  Spec.spec
  (Handlers.mkHandlers leaderNode)
