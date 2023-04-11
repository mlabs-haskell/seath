module Seath.HTTP.Types where

import Contract.Prelude
import Payload.Client.EncodeParam
import Payload.Server.Params
import Undefined

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeJsonString
  , encodeAeson
  , fromString
  , isString
  , stringifyAeson
  , toString
  )
import Ctl.Internal.Test.UtxoDistribution (encodeDistribution)
import Data.Bifunctor (bimap, lmap)
import Data.String
  ( Pattern(Pattern)
  , Replacement(Replacement)
  , replace
  , replaceAll
  )
import Data.UUID (UUID, parseUUID)
import Payload.Client.DecodeResponse (class DecodeResponse)
import Payload.Client.EncodeBody (class EncodeBody)
import Payload.ContentType (class HasContentType, json)
import Payload.ResponseTypes (Json(..), Response(..), ResponseBody(..))
import Payload.Server.DecodeBody (class DecodeBody)
import Payload.Server.Response (class EncodeResponse, encodeResponse, ok)
import Seath.Core.Types (UserAction)

-- Include action
-- ! acrh: newtype wrapper made to avoid having DecodeBody/EncodeBody
-- ! instances in `Network` layer, as it should be decoupled from HTTP layer
newtype IncludeRequest a = IncludeRequest (UserAction a)

derive instance Newtype (IncludeRequest a) _
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

instance EncodeParam (IncludeRequest a) where
  encodeParam = undefined

newtype UID = UID UUID

derive instance Newtype UID _

instance showUID :: Show UID where
  show (UID uuid) =
    replace (Pattern "(UUID ") (Replacement "")
      $ replace (Pattern ")") (Replacement "")
      $ show uuid

instance eaUID :: EncodeAeson UID where
  encodeAeson = show >>> fromString

instance epUID :: EncodeParam UID where
  encodeParam = show

instance dpUID :: DecodeParam UID where
  decodeParam uid =
    note ("Could not parse UUID param from " <> uid) (UID <$> parseUUID uid)

-- Phantom types to make Spec and Handlers more readable
-- Went with Record coz couldn't make `DecodeResponse` instance for custom `data` type
type JSend :: forall err a. err -> a -> Type
type JSend err a = { status :: String, data :: String }

toJsend
  :: forall err a
   . EncodeAeson err
  => EncodeAeson a
  => Either err a
  -> JSend err a
toJsend r = case r of
  Right a -> { status: "success", data: showAeson a }
  Left err -> { status: "failure", data: showAeson err }
  where
  showAeson :: forall v. EncodeAeson v => v -> String
  showAeson v =
    let
      encoded = encodeAeson v
    in -- FIXME: how to avoid extra quotes? 
      fromMaybe (show encoded) (toString encoded)
