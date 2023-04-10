module Seath.HTTP.Handlers where

import Prelude

import Contract.Prelude (Either(..), log, unwrap)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Effect.Aff (Aff)
import Seath.HTTP.Types (IncludeRequest, JSend, UID(UID), toJsend)
import Seath.Network.Leader as Leader
import Seath.Network.Types
  ( ActionStatus
  , GetStatusError
  , IncludeActionError
  , LeaderNode
  )
import Undefined (undefined)

mkHandlers leaderNode =
  { leader:
      { includeAction: includeAction leaderNode
      , actionStatus: actionStatus leaderNode
      }
  }

includeAction
  :: forall a
   . Show a
  => LeaderNode a
  -> { body :: IncludeRequest a }
  -> Aff (JSend IncludeActionError UID)
includeAction leaderNode req = do
  -- undefined
  log $ "Leader HTTP-server: include action request: " <> show req
  result <- leaderNode `Leader.includeAction` (unwrap req.body)
  let response = toJsend (rmap UID result)
  log $ "Leader HTTP-server: include action response: " <> show response
  pure response

acceptSignedTransaction :: forall anything. anything
acceptSignedTransaction = undefined

refuseToSign :: forall anything. anything
refuseToSign = undefined

actionStatus
  :: forall a
   . Show a
  => LeaderNode a
  -> { params :: { uid :: UID } }
  -> Aff (JSend GetStatusError ActionStatus)
actionStatus leaderNode request = do
  log $ "Leader HTTP-server: action status request: " <> show request
  (result :: ActionStatus) <- leaderNode `Leader.actionStatus`
    (unwrap request.params.uid)
  let response = toJsend $ (Right result :: Either GetStatusError ActionStatus)
  log $ "Leader HTTP-server: action status response: " <> show response
  pure response
