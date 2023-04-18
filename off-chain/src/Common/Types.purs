module Seath.Common.Types
  ( UID(UID)
  ) where

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , fromString
  , toString
  )
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Either (note)
import Data.Function (($), (>>>))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (class Newtype, wrap)
import Data.Show (class Show, show)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace)
import Data.UUID (UUID)
import Data.UUID as UUID
import Payload.Client.EncodeParam (class EncodeParam)
import Payload.Server.Params (class DecodeParam)

newtype UID = UID UUID

derive instance Newtype UID _

instance showUID :: Show UID where
  show (UID uuid) = UUID.toString uuid

instance eaUID :: EncodeAeson UID where
  encodeAeson = show >>> fromString

instance daUID :: DecodeAeson UID where
  decodeAeson a = do
    str <- note (TypeMismatch "Expected string") (toString a)
    uuid <- note (TypeMismatch "Can't parseUUID") (UUID.parseUUID str)
    pure $ wrap uuid

instance epUID :: EncodeParam UID where
  encodeParam = show

instance dpUID :: DecodeParam UID where
  decodeParam uid =
    note ("Could not parse UUID param from " <> uid)
      (wrap <$> UUID.parseUUID uid)
