module Seath.Network.Spec where

import Aeson
import Payload.Server.DecodeBody
import Prelude
import Type.Proxy
import Undefined

import Contract.Prelude (Effect, Either(..), genericShow, unwrap, wrap)
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Debug (trace)
import Effect.Aff (Aff)
import Payload.ResponseTypes (Empty)
import Payload.Server as Payload
import Payload.Spec
  ( type (:)
  , DELETE
  , GET
  , Guards
  , Nil
  , POST
  , Routes(Routes)
  , Spec(Spec)
  )
import Seath.Network.Types
  ( AskForSignature
  , IncludeActionRequest(IncludeActionRequest)
  , IncludeActionResponse
  , ReportErrorAtProcessing
  , SendSignatureRejection
  , SendSignedTransaction
  )

-- TODO: ⬇️
-- type UserServerSpec = Spec
--   { transactionSignature ::
--       POST "/user/transactionSignature"
--         { body :: AskForSignature
--         -- | Http always needs a response and we can't wait for user to sign a 
--         -- | transaction to respond, as consequence the corresponding response 
--         -- | is empty and the user need to send a new request
--         -- | with the signed transaction later.

--         , response :: Empty
--         }
--   , reportError ::
--       POST "/user/reportError"
--         { body :: ReportErrorAtProcessing
--         , response :: Empty
--         }
--   }

-- type LeaderServerSpec a = Spec
--   { includeAction ::
--       POST "/leader/includeAction"
--         { body :: IncludeActionRequest a
--         , response :: IncludeActionResponse
--         }
--   , acceptSignedTransaction ::
--       POST "/leader/acceptSignedTransaction"
--         { body :: SendSignedTransaction
--         , response :: Empty
--         }
--   , rejectToSign ::
--       POST "/leader/acceptSignedTransaction"
--         { body :: SendSignatureRejection
--         , response :: Empty
--         }
--   }

-- TODO: ⬆️

-- type LeaderRoutes a = Routes "/leader"
--            { getMessages ::
--                POST "/include_action"
--                  { body :: IncludeActionRequest a
--                  , response :: String
--                  }
--            }

type LeaderRoutes a = Routes "/leader"
  { includeAction ::
      POST "/include_action"
        { body :: IncludeActionRequest a
        , response :: String
        }
  }

spec
  :: forall a
   . DecodeAeson a
  => Spec
       { leader :: LeaderRoutes a
       }
spec = Spec

mkSpec
  :: forall a
   . Proxy a
  -> Spec
       { leader :: LeaderRoutes a
       }
mkSpec _ = Spec

includeAction
  :: forall a
   . Show a
  -- => DecodeAeson a
  => { body :: IncludeActionRequest a }
  -> Aff String
includeAction b = do
  -- let (IncludeActionRequest req) = b.body
  pure $ "Test: " <> show b

handlers = { leader: { includeAction: includeAction } }

-- newtype TestInclude a = TestInclude { id :: Int, smth :: a }

-- derive instance Generic (TestInclude a) _
-- instance Show a => Show (TestInclude a) where
--   show x = genericShow x

-- instance DecodeAeson a => DecodeAeson (TestInclude a) where
--   decodeAeson s = do
--     obj <- decodeAeson s
--     id <- getField obj "id"
--     smth <- getField obj "smth"
--     pure $ TestInclude { id, smth }

-- instance decodeBodyRecord ::
--   ( DecodeAeson a
--   ) =>
--   DecodeBody (TestInclude a) where
--   -- decodeBody s = undefined
--   decodeBody s = lmap show $ decodeJsonString s
