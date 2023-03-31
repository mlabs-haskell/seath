module Seath.Network.Spec where

import Data.Either (Either)
import Data.UUID (UUID)
import Payload.ResponseTypes (Empty)
import Payload.Spec (GET, POST, Spec)
import Seath.Core.Types (UserAction)
import Seath.Network.Types
  ( IncludeActionError
  , SendSignedTransaction
  , StatusResponse
  )

type LeaderServerSpec a = Spec
  { includeAction ::
      POST "/leader/includeAction"
        { body :: UserAction a
        , response :: Either IncludeActionError UUID
        }
  , acceptSignedTransaction ::
      POST "/leader/acceptSignedTransaction"
        { body :: SendSignedTransaction
        , response :: Empty
        }
  , rejectToSign ::
      GET "/leader/refuseToSign/<controlNumber>"
        { params :: { controlNumber :: UUID }
        , response :: Empty
        }
  , getActionStatus ::
      GET "/leader/getActionStatus/<controlNumber>"
        { params :: { controlNumber :: UUID }
        , response :: StatusResponse
        }
  }
