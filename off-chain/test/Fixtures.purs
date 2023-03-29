module Seath.Test.Fixtures where

import Data.Either (Either)
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Seath.Network.Types
  ( AskForSignature
  , IncludeActionRequest
  , IncludeActionResponse
  , ReportErrorAtProcessing
  , SendSignatureRejection
  , SendSignedTransaction
  )
import Seath.Test.Examples.Addition.Types (AdditionAction)
import Type.Function (type ($))
import Undefined (undefined)

fixedTimeOut :: Int
fixedTimeOut = 100

fixedPort :: Int
fixedPort = 22

-- Those must be IO handlers (local instead of network)
fixedLeaderClientHandlers
  :: { transactionSignature :: AskForSignature -> Aff $ Either String Unit
     , reportError :: ReportErrorAtProcessing -> Aff $ Either String Unit
     }
fixedLeaderClientHandlers = undefined

-- Those must be IO handlers (local instead of network)
fixedUserClientHandlers
  :: { includeAction ::
         IncludeActionRequest AdditionAction
         -> Aff $ Either String IncludeActionResponse
     , acceptSignedTransaction ::
         SendSignedTransaction -> Aff $ Either String Unit
     , rejectToSign :: SendSignatureRejection -> Aff $ Either String Unit
     }
fixedUserClientHandlers = undefined
