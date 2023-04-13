module Seath.HTTP.Spec where

import Payload.Spec (GET, POST, Routes, Spec(Spec))
import Seath.Common.Types (UID)
import Seath.HTTP.Types (IncludeRequest, JSend, SendSignedRequest)
import Seath.Network.Types (ActionStatus, GetStatusError, IncludeActionError)

type LeaderRoutes a = Routes "/leader"
  { includeAction ::
      POST "/include-action"
        { body :: IncludeRequest a
        , response :: JSend IncludeActionError UID
        }
  , actionStatus ::
      GET "/action-status/<uid>"
        { params :: { uid :: UID }
        , response :: JSend GetStatusError ActionStatus
        }
  , acceptSignedTransaction ::
      POST "/accept-signed-tx"
        { body :: SendSignedRequest
        , response :: JSend String String
        }
  , refuseToSign ::
      GET "/refuse-to-sign/<uid>"
        { params :: { uid :: UID }
        , response :: (JSend String String)
        }

  }

spec
  :: forall a
   . Spec
       { leader :: LeaderRoutes a
       }
spec = Spec

-- TODO:
-- type LeaderServerSpec a = Spec
--   , acceptSignedTransaction ::
--       POST "/leader/acceptSignedTransaction"
--         { body :: SendSignedTransaction
--         , response :: Empty
--         }
--   }

