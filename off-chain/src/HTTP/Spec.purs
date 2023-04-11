module Seath.HTTP.Spec where

import Seath.HTTP.Types

import Payload.Spec (GET, POST, Routes, Spec(Spec))
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
--   , rejectToSign ::
--       GET "/leader/refuseToSign/<controlNumber>"
--         { params :: { controlNumber :: UUID }
--         , response :: Empty
--         }
--   }

