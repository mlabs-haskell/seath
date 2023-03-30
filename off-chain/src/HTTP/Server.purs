module Seath.Network.Server where

import Prelude
import Seath.Network.Spec
import Type.Proxy

import Effect (Effect)
import Payload.Server as Payload
import Seath.Test.Examples.Addition.Types (AdditionAction)

type ServerConfig = {}

runServer :: ServerConfig -> Effect Unit
runServer _ =
  Payload.launch
    (mkSpec (Proxy :: Proxy AdditionAction))
    handlers