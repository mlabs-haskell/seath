module Seath.Network.TxHex where

import Contract.Prelude
  ( Aff
  , Effect
  , Either(Left, Right)
  , bind
  , liftEffect
  , pure
  , ($)
  )
import Contract.Transaction (Transaction)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Ctl.Internal.Deserialization.Transaction as D
import Ctl.Internal.Serialization (convertTransaction, toBytes) as S
import Ctl.Internal.Serialization.Types (Transaction) as S
import Ctl.Internal.Types.CborBytes (CborBytes, cborBytesToHex, hexToCborBytes)
import Effect.Aff (error)

-- todo: to ~ from test
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
    Left _ -> throwError (error "Can't deserialise CBOR to Transaction")
    Right tx -> pure tx
