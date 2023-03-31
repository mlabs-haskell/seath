module Seath.HTTP.Server where

import Prelude

import Aeson (class DecodeAeson)
import Effect (Effect)
import Payload.Server as Payload
import Seath.HTTP.Handlers as Handlers
import Seath.HTTP.Spec as Spec
import Seath.Network.Types (LeaderNode)

type SeathServerConfig = {}

runServer
  :: forall a
   . DecodeAeson a
  => Show a
  => SeathServerConfig
  -> LeaderNode a
  -> Effect Unit
runServer _ leaderNode = Payload.launch
  Spec.spec
  (Handlers.mkHandlers leaderNode)