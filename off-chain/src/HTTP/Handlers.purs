module Seath.HTTP.Handlers where

import Prelude

import Aeson (encodeAeson, toString)
import Contract.Prelude (Either(..), log)
import Data.Either (Either)
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Seath.HTTP.Types (IncludeRequest, JSend(..), UID(..), toJsend)
import Seath.Network.Types (LeaderNode)
import Undefined (undefined)

mkHandlers leaderNode = { leader: { includeAction: includeAction leaderNode } }

includeAction
  :: forall a
   . Show a
  => LeaderNode a
  -> { body :: IncludeRequest a }
  -- -> Aff (Either String UUID)
  -> Aff (JSend String UID)
includeAction leaderNode b = do
  uuid <- liftEffect $ do
    log $ "Leader: Server: includeAction body: " <> show b
    genUUID -- TODO: use leader node
  pure $ toJsend $ (Right (UID uuid) :: Either String UID)
  -- pure $ toJsend $ (Left "ErrLOL" :: Either String UID)

acceptSignedTransaction :: forall anything. anything
acceptSignedTransaction = undefined

refuseToSign :: forall anything. anything
refuseToSign = undefined

getActionStatus :: forall anything. anything
getActionStatus = undefined
