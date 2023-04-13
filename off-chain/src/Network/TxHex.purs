module Seath.Network.TxHex where

import Contract.Prelude
import Ctl.Internal.Types.CborBytes
import Undefined

import Contract.Transaction (Transaction(..))
import Control.Monad.Error.Class (liftMaybe, throwError)
import Ctl.Internal.Deserialization.Transaction as D
import Ctl.Internal.Serialization as S
import Ctl.Internal.Serialization.Types as S
import Effect.Aff (error)

toCborHex :: Transaction -> Aff String
toCborHex tx = do
  cslTx <- liftEffect (S.convertTransaction tx :: Effect S.Transaction)
  let
    cb :: CborBytes
    cb = S.toBytes cslTx
  pure $ cborBytesToHex cb

fromCborHex :: String -> Aff Transaction
fromCborHex hex = do
  bytes <- liftMaybe (error "Failed to read CBOR") (hexToCborBytes hex)
  let
    cslTx = D.deserializeTransaction bytes
  case cslTx of
    Left _ -> throwError (error "Cant deserialise CBOR to Transaction")
    Right tx -> pure tx
