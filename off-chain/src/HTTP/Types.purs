module Seath.HTTP.Types where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeJsonString
  , encodeAeson
  , toString
  )
import Contract.Transaction (FinalizedTransaction(FinalizedTransaction))
import Data.Bifunctor (bimap)
import Payload.Client.EncodeBody (class EncodeBody)
import Payload.Client.EncodeParam (class EncodeParam)
import Payload.ContentType (class HasContentType, json)
import Payload.Server.DecodeBody (class DecodeBody)
import Seath.Common.Types (UID(UID))
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
        { "uuid": encodeAeson (UID s.uuid)
        , "transaction": encodeAeson tx
        }

instance sendSignedReqContentType :: HasContentType SendSignedRequest where
  getContentType _ = json
