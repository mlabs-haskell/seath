module Seath.Test.Examples.Utils
  ( submitTxFromConstraintsWithLog
  , getScriptInputAndUtxos
  , getScriptUtxos
  , genPlutipWalletConfig
  ) where

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.PlutusData (class IsData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (class ValidatorTypes, ValidatorHash)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , _input
  , balanceTx
  , lookupTxHash
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (UtxoMap, utxosAt)
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Array (head, replicate)
import Data.BigInt as BigInt
import Data.Functor ((<$>))
import Data.Lens (view)
import Data.Maybe (Maybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (discard, ($))

submitTxFromConstraintsWithLog
  :: forall (validator :: Type) (datum :: Type)
       (redeemer :: Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => ScriptLookups.ScriptLookups validator
  -> TxConstraints redeemer datum
  -> Contract TransactionHash
submitTxFromConstraintsWithLog lookups constraints = do
  unbalancedTx <- liftedE $ ScriptLookups.mkUnbalancedTx lookups constraints
  logInfo' $ "unbalancedTx: " <> show unbalancedTx
  balancedTx <- liftedE $ balanceTx unbalancedTx
  logInfo' $ "balancedTx: " <> show balancedTx
  balancedSignedTx <- signTransaction balancedTx
  logInfo' $ "balancedSignedTx: " <> show balancedSignedTx
  txHash <- submit balancedSignedTx
  logInfo' $ "submitedTxId: " <> show txHash
  pure txHash

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
