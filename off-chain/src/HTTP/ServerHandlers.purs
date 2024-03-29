module Seath.HTTP.ServerHandlers where

import Contract.Prelude

import Data.Bifunctor (lmap, rmap)
import Effect.Aff (Aff, try)
import Seath.Common.Types (UID(UID))
import Seath.HTTP.Types (IncludeRequest, JSend, SendSignedRequest, toJsend)
import Seath.Network.Leader as Leader
import Seath.Network.Types (ActionStatus, IncludeActionError, LeaderNode)

mkHandlers
  :: forall actionType
   . Show actionType
  => LeaderNode actionType
  -> { leader ::
         { acceptSignedTransaction ::
             { body :: SendSignedRequest
             }
             -> Aff
                  { data :: String
                  , status :: String
                  }
         , actionStatus ::
             { params ::
                 { uid :: UID
                 }
             }
             -> Aff
                  { data :: String
                  , status :: String
                  }
         , includeAction ::
             { body :: IncludeRequest actionType
             }
             -> Aff
                  { data :: String
                  , status :: String
                  }
         , refuseToSign ::
             { params ::
                 { uid :: UID
                 }
             }
             -> Aff
                  { data :: String
                  , status :: String
                  }
         }
     }
mkHandlers leaderNode =
  { leader:
      { includeAction: includeAction leaderNode
      , actionStatus: actionStatus leaderNode
      , acceptSignedTransaction: acceptSignedTransaction leaderNode
      , refuseToSign: refuseToSign leaderNode
      }
  }

includeAction
  :: forall a
   . Show a
  => LeaderNode a
  -> { body :: IncludeRequest a }
  -> Aff (JSend IncludeActionError UID)
includeAction leaderNode req = do
  log $ "Leader HTTP-server: include action request: " <> show req
  result <- leaderNode `Leader.includeAction` (unwrap req.body)
  let response = toJsend (rmap UID result)
  log $ "Leader HTTP-server: include action response: " <> show response
  pure response

acceptSignedTransaction
  :: forall a
   . LeaderNode a
  -> { body :: SendSignedRequest }
  -> Aff (JSend String String)
acceptSignedTransaction leaderNode req = do
  log $ "Leader HTTP-server: accept signed Tx request: " <> show
    (unwrap (unwrap req.body)).uuid
  _result <- leaderNode `Leader.acceptSignedTransaction` (unwrap req.body)
  let
    response = case _result of
      Left msg -> toJsend $ (Left msg :: Either String String)
      Right _ -> toJsend (Right "" :: Either String String)
  log $ "Leader HTTP-server: accept signed Tx response: " <> show response
  pure response

refuseToSign
  :: forall a
   . LeaderNode a
  -> { params :: { uid :: UID } }
  -> Aff (JSend String String)
refuseToSign leaderNode request = do
  let uuid = unwrap request.params.uid
  log $ "Leader HTTP-server: refuse to sign request: "
    <> show uuid
  _result <- leaderNode `Leader.acceptRefuseToSign` uuid
  log $ "Leader HTTP-server: refuse to sign request acknowledged" <> show
    _result
  case _result of
    Right _ -> pure <<< toJsend $ (Right "" :: Either String String)
    Left msg -> pure <<< toJsend $ (Left msg :: Either String String)

actionStatus
  :: forall a
   . Show a
  => LeaderNode a
  -> { params :: { uid :: UID } }
  -> Aff (JSend String ActionStatus)
actionStatus leaderNode request = do
  log $ "Leader HTTP-server: action status request: " <> show request
  result <- try $ leaderNode `Leader.actionStatus`
    (unwrap request.params.uid)
  let response = toJsend (lmap show result)
  log $ "Leader HTTP-server: action status response: " <> show response
  pure response
