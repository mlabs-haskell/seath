module Seath.HTTP.Client where

import Prelude
import Type.Proxy

import Effect (Effect)
import Payload.Client (defaultOpts, mkClient)
import Payload.Server as Payload
import Seath.HTTP.Handlers as Handlers
import Seath.HTTP.Spec as Spec
import Seath.Test.Examples.Addition.Types (AdditionAction)

client =
  let
    opts = defaultOpts { baseUrl = "http://localhost:3000" }
  in
    mkClient opts (Spec.spec (Proxy :: Proxy AdditionAction))