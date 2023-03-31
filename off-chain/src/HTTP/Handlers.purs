module Seath.HTTP.Handlers where

import Prelude

import Contract.Prelude (Either(..), log)
import Data.Either (Either)
import Data.UUID (genUUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Seath.HTTP.Types (IncludeRequest, IncludeResponse, fromUUID)
import Undefined (undefined)

handlers = { leader: { includeAction: includeAction } }

includeAction
  :: forall a
   . Show a
  => { body :: IncludeRequest a }
  -> Aff (Either String IncludeResponse)
includeAction b = do
  uuid <- liftEffect $ do
    log $ show b
    genUUID
  pure $ Right (fromUUID uuid)

-- pure $ Left "Include action error"

acceptSignedTransaction :: forall anything. anything
acceptSignedTransaction = undefined

refuseToSign :: forall anything. anything
refuseToSign = undefined

getActionStatus :: forall anything. anything
getActionStatus = undefined
