module Seath.HTTP.Types where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeJsonString
  , encodeAeson
  , fromString
  , toString
  )
import Contract.Transaction (FinalizedTransaction(FinalizedTransaction))
import Data.Bifunctor (bimap)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace)
import Data.UUID (UUID, parseUUID)
import Payload.Client.EncodeBody (class EncodeBody)
import Payload.Client.EncodeParam (class EncodeParam)
import Payload.ContentType (class HasContentType, json)
import Payload.Server.DecodeBody (class DecodeBody)
import Payload.Server.Params (class DecodeParam)
import Seath.Core.Types (UserAction)
import Seath.Network.Types (SendSignedTransaction(..))
import Undefined (undefined)

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

newtype SendSignedRequest = SendSignedRequest SendSignedTransaction

derive instance Newtype SendSignedRequest _
derive newtype instance Show SendSignedRequest
instance decSendSignedReq ::
  DecodeBody SendSignedRequest where
  decodeBody s = undefined

instance encSendSignedReq ::
  EncodeBody SendSignedRequest where
  encodeBody sendSigReq =
    let
      (SendSignedTransaction s) = unwrap sendSigReq
      (FinalizedTransaction tx) = unwrap s.transaction
    in
      show $ encodeAeson
        { "controlNumber": encodeAeson (UID s.controlNumber)
        , "transaction": encodeAeson tx
        }

instance sendSignedReqContentType :: HasContentType SendSignedRequest where
  getContentType _ = json