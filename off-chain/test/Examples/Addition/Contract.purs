module Seath.Test.Examples.Addition.Contract (simpleTest) where

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Log (logInfo')
import Contract.Monad (Aff, Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.Prelude (liftEffect)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts
  ( class DatumType
  , class RedeemerType
  , PlutusScript
  , Validator
  , ValidatorHash
  , applyArgs
  , validatorHash
  )
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (runPlutipContract, PlutipConfig)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , mustIncludeDatum
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Wallet (KeyWallet)
import Control.Applicative (pure)
import Control.Monad (bind, (>>=))
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (filter, head)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map (Map)
import Data.Map as Map
import Data.Monoid ((<>), mempty)
import Data.Newtype (unwrap, wrap)
import Data.Show (show)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unit (Unit, unit)
import Effect.Aff (error)
import Effect.Exception (throw)
import Prelude (($), discard, (<<<), (==), (+))
import Seath.Test.Examples.Addition.Types
  ( AdditionDatum(AdditionDatum)
  , AdditionParams
  , AdditionRedeemer(AdditionRedeemer)
  )
import Seath.Test.Exaxmles.Addition.Validator (validatorScript)

data AdditionValidator

instance DatumType AdditionValidator AdditionDatum
instance RedeemerType AdditionValidator AdditionRedeemer

importValidator :: AdditionParams -> Contract PlutusScript
importValidator params = do
  validator <- liftedM "can't decode validator" $ pure
    ( decodeTextEnvelope validatorScript
        >>= plutusScriptV2FromEnvelope
    )
  liftedE $ pure (applyArgs validator [ toData params ])

getValidatorAndHash :: AdditionParams -> Contract (Validator /\ ValidatorHash)
getValidatorAndHash params = do
  script <- importValidator params
  let validator = wrap script
  pure $ validator /\ validatorHash validator

simpleTest :: PlutipConfig -> Aff Unit
simpleTest config = runPlutipContract config distribution onchainActions
  where
  distribution :: Array (Array BigInt)
  distribution = [ [ BigInt.fromInt 1_000_000_000 ] ]

  onchainActions :: Array KeyWallet -> Contract Unit
  onchainActions [ onlyWallet ] = do
    (txId1 /\ datum1) <-
      withKeyWallet onlyWallet initialContract
    _ <- withKeyWallet onlyWallet $ advanceStateContract txId1 datum1 200
    pure unit
  onchainActions _ = liftEffect $ throw "Can't consume plutip configuration"

initialContract :: Contract (TransactionHash /\ AdditionDatum)
initialContract = do
  validator /\ hash <- getValidatorAndHash unit
  let
    lookups :: ScriptLookups.ScriptLookups AdditionValidator
    lookups = ScriptLookups.validator validator
    datum = AdditionDatum $ { lockedAmount: BigInt.fromInt 100 }
    constraints = mustPayToScript hash
      ( wrap
          <<< toData
          <<< AdditionDatum $ { lockedAmount: BigInt.fromInt 100 }
      )
      DatumInline
      mempty
  transactionId <- submitTxFromConstraints lookups constraints
  logInfo' $ "Tx ID: " <> show transactionId
  awaitTxConfirmed transactionId
  logInfo' "Confirmed tx"
  pure $ transactionId /\ datum

advanceStateContract
  :: TransactionHash
  -> AdditionDatum
  -> Int
  -> Contract (TransactionHash /\ AdditionDatum)
advanceStateContract txId datum increase = do
  (validator /\ hash) <- getValidatorAndHash unit
  (txIn /\ scriptUtxos) <- getScriptOutputs hash txId
  let
    redeemer = AdditionRedeemer { increaseAmount: BigInt.fromInt increase }
    newDatum = AdditionDatum
      { lockedAmount: (unwrap datum).lockedAmount + BigInt.fromInt increase
      }

    lookups :: ScriptLookups.ScriptLookups AdditionValidator
    lookups = ScriptLookups.validator validator
      <> ScriptLookups.unspentOutputs scriptUtxos
    constraints =
      mustPayToScript hash (wrap <<< toData $ newDatum)
        DatumInline
        mempty
        <> mustSpendScriptOutput txIn (wrap $ toData redeemer)
        <> mustIncludeDatum (wrap $ toData datum)
  transactionId <- submitTxFromConstraints lookups constraints
  logInfo' $ "Tx ID: " <> show transactionId
  awaitTxConfirmed transactionId
  logInfo' "Confirmed tx"
  pure $ transactionId /\ newDatum

getScriptOutputs
  :: ValidatorHash
  -> TransactionHash
  -> Contract
       ( TransactionInput /\
           Map TransactionInput TransactionOutputWithRefScript
       )
getScriptOutputs hash txId = do
  netId <- getNetworkId
  validatorAddress <- liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId hash)
  scriptUtxos <- utxosAt validatorAddress
  (txIn /\ _) <- liftMaybe (error $ "cannot find transaction: " <> show txId)
    $ head
    $ filter (\(input /\ _) -> (unwrap input).transactionId == txId)
    $ Map.toUnfoldable scriptUtxos
  pure (txIn /\ scriptUtxos)
