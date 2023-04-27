module Seath.HTTP.Server where

import Prelude

import Aeson (class DecodeAeson)
import Data.Either (Either)
import Effect.Aff (Aff)
import Payload.Server (Server, defaultOpts)
import Payload.Server as Payload
import Seath.HTTP.ServerHandlers as Handlers
import Seath.HTTP.Spec as Spec
import Seath.Network.Types (LeaderNode)

type SeathServerConfig = { port :: Int }

runServer
  :: forall a
   . DecodeAeson a
  => Show a
  => SeathServerConfig
  -> LeaderNode a
  -> Aff (Either String Server)
runServer conf leaderNode = Payload.start
  defaultOpts { port = conf.port }
  Spec.spec
  (Handlers.mkHandlers leaderNode)
