module Seath.Common.Types
  ( UID(..)
  ) where

import Contract.Prelude

import Aeson (class EncodeAeson, fromString)
import Data.String (Pattern(Pattern), Replacement(Replacement), replace)
import Data.UUID (UUID, parseUUID)
import Payload.Client.EncodeParam (class EncodeParam)
import Payload.Server.Params (class DecodeParam)

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