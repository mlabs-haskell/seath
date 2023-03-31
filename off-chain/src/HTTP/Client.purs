module Seath.HTTP.Client where

import Prelude
import Type.Proxy

import Effect (Effect)
import Payload.Client as Client
import Payload.Client.ClientApi (class ClientApi)
import Payload.Server as Payload
import Seath.HTTP.Handlers as Handlers
import Seath.HTTP.Spec as Spec
import Seath.Network.Types (UserNode)
import Seath.Test.Examples.Addition.Types (AdditionAction)

mkUserClient userNode =
  let
    opts = Client.defaultOpts { baseUrl = "http://localhost:3000" } -- FIXME: hardcoded
  in
    Client.mkClient opts (Spec.spec)