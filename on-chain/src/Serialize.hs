module Serialize (toStringEnvelope) where

import Cardano.Api (PlutusScriptV2, serialiseToJSON, serialiseToTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Maybe (Maybe (Nothing))
import Data.String (String)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Plutus.V2.Ledger.Api (Script)
import Prelude ((.))

toStringEnvelope :: Script -> String
toStringEnvelope =
  Text.unpack . Text.decodeUtf8 . serialiseToJSON
    . serialiseToTextEnvelope Nothing
    . PlutusScriptSerialised @PlutusScriptV2
    . toShort
    . toStrict
    . serialise
