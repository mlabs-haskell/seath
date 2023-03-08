module Seath.Test.Examples.Utils
  ( submitTxFromConstraintsWithLog
  , getScriptInput
  , getScriptInputAndUtxos
  , getScriptUtxos
  , genPlutipWalletConfig
  , genScriptUTXoFromTransaction
  , getFinalizedTransactionHash
  ) where

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Hashing as Ctl.Internal.Hashing
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.PlutusData (class IsData)
import Contract.Prelude (liftEffect, liftM, sequence)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (class ValidatorTypes, ValidatorHash)
import Contract.Transaction
  ( FinalizedTransaction(FinalizedTransaction)
  , TransactionHash
  , TransactionInput
  , _body
  , _input
  , _inputs
  , _outputs
  , balanceTx
  , lookupTxHash
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (UtxoMap, utxosAt)
import Control.Applicative (pure)
import Control.Monad (bind)
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Ctl.Internal.Serialization as Ctl.Internal.Serialization
import Data.Array (filter, head, replicate)
import Data.BigInt as BigInt
import Data.Eq ((==))
import Data.Functor ((<$>))
import Data.Lens (view)
import Data.Map as Map
import Data.Maybe (Maybe, isJust)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Set (toUnfoldable)
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (error)
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

getFinalizedTransactionHash :: FinalizedTransaction -> Contract TransactionHash
getFinalizedTransactionHash fTx = do
  liftEffect $ Ctl.Internal.Hashing.transactionHash <$>
    Ctl.Internal.Serialization.convertTransaction (unwrap fTx)

genScriptUTXoFromTransaction
  :: FinalizedTransaction -> ValidatorHash -> Contract UtxoMap
genScriptUTXoFromTransaction fTx@(FinalizedTransaction tx) valHash = do
  txId <- getFinalizedTransactionHash fTx
  netId <- getNetworkId
  validatorAddress <- liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId valHash)
  let
    txInputs = view _inputs (view _body tx)
    txOutputsCardano = view _outputs (view _body tx)
  txInput <- liftM (error "Can't find Input in transaction") $ head
    $ filter (\tx' -> (unwrap tx').transactionId == txId)
    $ toUnfoldable txInputs
  txOutputsPlutus <-
    liftM (error "Can't find Outputs in transaction") $
      sequence
        ( filter
            isJust
            $ toPlutusTxOutputWithRefScript <$> txOutputsCardano
        )
  txOutputPlutus <- liftM (error "Can't find output in transaction") $ head $
    filter (\tx' -> (unwrap (unwrap tx').output).address == validatorAddress)
      txOutputsPlutus
  pure $ Map.singleton txInput txOutputPlutus
