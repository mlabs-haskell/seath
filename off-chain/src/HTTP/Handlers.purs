module Seath.HTTP.Handlers where

import Prelude

import Aeson (encodeAeson, toString)
import Contract.Prelude (Either(..), log, unwrap)
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Seath.HTTP.Types (IncludeRequest, JSend(..), UID(..), toJsend)
import Seath.Network.Leader as Leader
import Seath.Network.Types (IncludeActionError, LeaderNode)
import Undefined (undefined)

mkHandlers leaderNode = { leader: { includeAction: includeAction leaderNode } }

includeAction
  :: forall a
   . Show a
  => LeaderNode a
  -> { body :: IncludeRequest a }
  -- -> Aff (Either String UUID)
  -> Aff (JSend String UID)
includeAction leaderNode req = do
  log "Leader HTTP-server: requets to include action"
  result <- leaderNode `Leader.includetAction` (unwrap req.body)
  -- TODO: correct (de)serialization for `IncludeActionError`
  pure $ toJsend $ (bimap show UID result)

acceptSignedTransaction :: forall anything. anything
acceptSignedTransaction = undefined

refuseToSign :: forall anything. anything
refuseToSign = undefined

getActionStatus :: forall anything. anything
getActionStatus = undefined
