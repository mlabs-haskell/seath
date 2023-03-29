module Seath.Network.Spec where

import Payload.ResponseTypes (Empty)
import Payload.Spec (POST, Spec)
import Seath.Network.Types
  ( AskForSignature
  , IncludeActionRequest
  , IncludeActionResponse
  , ReportErrorAtProcessing
  , SendSignatureRejection
  , SendSignedTransaction
  )

type UserServerSpec = Spec
  { transactionSignature ::
      POST "/user/transactionSignature"
        { body :: AskForSignature
        -- | Http always needs a response and we can't wait for user to sign a 
        -- | transaction to respond, as consequence the corresponding response 
        -- | is empty and the user need to send a new request
        -- | with the signed transaction later.

        , response :: Empty
        }
  , reportError ::
      POST "/user/reportError"
        { body :: ReportErrorAtProcessing
        , response :: Empty
        }
  }

type LeaderServerSpec a = Spec
  { includeAction ::
      POST "/leader/includeAction"
        { body :: IncludeActionRequest a
        , response :: IncludeActionResponse
        }
  , acceptSignedTransaction ::
      POST "/leader/acceptSignedTransaction"
        { body :: SendSignedTransaction
        , response :: Empty
        }
  , rejectToSign ::
      POST "/leader/acceptSignedTransaction"
        { body :: SendSignatureRejection
        , response :: Empty
        }
  }
