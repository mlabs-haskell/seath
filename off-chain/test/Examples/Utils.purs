module Seath.Test.Examples.Utils
  ( genPlutipWalletConfig
  , getScriptInput
  , getScriptInputAndUtxos
  , getScriptUtxos
  , getTypedDatum
  ) where

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData
  ( class FromData
  , fromData
  , toData
  )
import Contract.Scripts (ValidatorHash)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , _input
  , lookupTxHash
  , outputDatumDatum
  )
import Contract.Utxos (UtxoMap, utxosAt)
import Control.Applicative (pure)
import Control.Monad (bind)
import Ctl.Internal.Plutus.Types.Transaction (_datum, _output)
import Data.Array (head, replicate)
import Data.BigInt as BigInt
import Data.Either (Either, note)
import Data.Functor ((<$>))
import Data.Lens (view, (^.))
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (($))

getScriptUtxos
  :: ValidatorHash
  -> Contract UtxoMap
getScriptUtxos hash = do
  netId <- getNetworkId
  validatorAddress <- liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId hash)
  utxosAt validatorAddress

getScriptInput :: TransactionHash -> UtxoMap -> Maybe TransactionInput
getScriptInput txId utxos = view _input <$> head (lookupTxHash txId utxos)

getScriptInputAndUtxos
  :: ValidatorHash
  -> TransactionHash
  -> Contract (Maybe TransactionInput /\ UtxoMap)
getScriptInputAndUtxos valHash txId = do
  utxos <- getScriptUtxos valHash
  pure $ getScriptInput txId utxos /\ utxos

genPlutipWalletConfig :: Int -> Array (Array BigInt.BigInt)
genPlutipWalletConfig value = replicate value [ BigInt.fromInt 1_000_000_000 ]

getTypedDatum
  :: forall a. FromData a => TransactionOutputWithRefScript -> Either String a
getTypedDatum out = do
  datum <- note "can't get datum" $ outputDatumDatum (out ^. _output ^. _datum)
  note "can't decode datum" $ fromData $ toData datum

