module Seath.HTTP.Handlers where

import Prelude

import Contract.Prelude (Either(..), log)
import Data.Either (Either)
import Data.UUID (genUUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Seath.HTTP.Types (IncludeRequest, IncludeResponse, fromUUID)
import Seath.Network.Types (LeaderNode)
import Undefined (undefined)

mkHandlers leaderNode = { leader: { includeAction: includeAction leaderNode } }

includeAction
  :: forall a
   . Show a
  => LeaderNode a
  -> { body :: IncludeRequest a }
  -> Aff (Either String IncludeResponse)
includeAction leaderNode b = do
  uuid <- liftEffect $ do
    log $ "Leader: Server: includeAction body: " <> show b
    genUUID -- TODO: use leader node
  pure $ Right (fromUUID uuid)

acceptSignedTransaction :: forall anything. anything
acceptSignedTransaction = undefined

refuseToSign :: forall anything. anything
refuseToSign = undefined

getActionStatus :: forall anything. anything
getActionStatus = undefined
