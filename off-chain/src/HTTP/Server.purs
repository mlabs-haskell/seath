module Seath.HTTP.Server where

import Prelude
import Type.Proxy

import Effect (Effect)
import Payload.Server as Payload
import Seath.HTTP.Handlers as Handlers
import Seath.HTTP.Spec as Spec
import Seath.Test.Examples.Addition.Types (AdditionAction)

type ServerConfig = {}

runServer :: ServerConfig -> Effect Unit
runServer _ = Payload.launch
  (Spec.spec (Proxy :: Proxy AdditionAction))
  Handlers.handlers