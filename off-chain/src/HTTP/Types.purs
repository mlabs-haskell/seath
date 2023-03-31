module Seath.HTTP.Types where

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeJsonString
  , encodeAeson
  )
import Contract.Prelude (class Show, show, ($))
import Data.Bifunctor (bimap)
import Data.UUID (UUID)
import Payload.Client.EncodeBody (class EncodeBody)
import Payload.ContentType (class HasContentType, json)
import Payload.Server.DecodeBody (class DecodeBody)
import Seath.Core.Types (UserAction)

-- Include action
newtype IncludeRequest a = IncludeRequest (UserAction a)

derive newtype instance Show a => Show (IncludeRequest a)

instance decIncludeReq ::
  ( DecodeAeson a
  ) =>
  DecodeBody (IncludeRequest a) where
  decodeBody s = bimap show IncludeRequest $ decodeJsonString s

instance encIncludeReq ::
  ( EncodeAeson a
  ) =>
  EncodeBody (IncludeRequest a) where
  encodeBody (IncludeRequest action) = show $ encodeAeson action

instance includeContentType :: HasContentType (IncludeRequest a) where
  getContentType _ = json

type IncludeResponse =
  { requestId :: String }

fromUUID :: UUID -> IncludeResponse
fromUUID uuid = { requestId: show uuid }

-- newtype IncludeResponse = IncludeResponse UUID

-- instance encIncludeResp :: EncodeResponse IncludeResponse where
--   encodeResponse (IncludeResponse uuid) = undefined
