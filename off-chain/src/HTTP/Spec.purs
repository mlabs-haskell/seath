module Seath.HTTP.Spec where

import Contract.Prelude (Either)
import Data.UUID (UUID)
import Payload.Spec (POST, Routes, Spec(Spec))
import Seath.HTTP.Types (IncludeRequest, JSend, UID(..))
import Type.Proxy (Proxy)

type LeaderRoutes a = Routes "/leader"
  { includeAction ::
      POST "/include-action"
        { body :: IncludeRequest a
        -- , response :: Either String UUID 
        , response :: JSend String UID 
        }
  }

spec
  :: forall a
   . Spec
       { leader :: LeaderRoutes a
       }
spec = Spec

proxySpec
  :: forall a
   . Proxy a
  -> Spec
       { leader :: LeaderRoutes a
       }
proxySpec _ = spec

-- TODO:
-- type LeaderServerSpec a = Spec
--   { includeAction ::
--       POST "/leader/includeAction"
--         { body :: UserAction a
--         , response :: Either IncludeActionError UUID
--         }
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
--   , getActionStatus ::
--       GET "/leader/getActionStatus/<controlNumber>"
--         { params :: { controlNumber :: UUID }
--         , response :: StatusResponse
--         }
--   }

