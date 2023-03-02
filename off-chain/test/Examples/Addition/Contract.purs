module Seath.Test.Examples.Addition.Contract (mainTest) where

import Contract.Log (logInfo')
import Contract.Monad (Aff, Contract, liftedE, liftedM)
import Contract.PlutusData (Datum, toData)
import Contract.Prelude (liftEffect)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (class DatumType, class RedeemerType, PlutusScript, Validator, ValidatorHash, applyArgs, validatorHash)
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (runPlutipContract, PlutipConfig)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (TransactionHash, awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline), mustPayToScript, mustSpendScriptOutput)
import Contract.Wallet (KeyWallet)
import Control.Applicative (pure)
import Control.Monad (bind, (>>=))
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, tail)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show (show)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unit (Unit, unit)
import Effect.Aff (error)
import Effect.Exception (throw)
import Prelude (($), discard, (<<<), (+))
import Seath.Test.Examples.Addition.Types (AdditionDatum(AdditionDatum), AdditionParams, AdditionRedeemer(AdditionRedeemer))
import Seath.Test.Examples.Addition.Validator (validatorScript)
import Seath.Test.Examples.Utils (genPlutipWalletConfig, getScriptInputAndUtxos, getScriptUtxos)

data AdditionValidator

instance DatumType AdditionValidator AdditionDatum
instance RedeemerType AdditionValidator AdditionRedeemer

newtype ActionNumber = ActionNumber Int

derive instance Newtype ActionNumber _

mainTest :: PlutipConfig -> Aff Unit
mainTest config = runPlutipContract config distribution $ plutipAction increase
  where
  walletNumber :: Int
  walletNumber = 10

  distribution :: Array (Array BigInt)
  distribution = genPlutipWalletConfig walletNumber

  increase :: Int
  increase = 200

plutipAction
  :: Int
  -> Array KeyWallet
  -> Contract Unit
plutipAction increase wallets = do
  _ /\ valHash <- getValidatorAndHash unit
  case head wallets of
    Just firstWallet -> do
      txId /\ datum <- withKeyWallet firstWallet initialContract
      logState valHash
      _ <- loop valHash (tail wallets) txId datum
      pure unit
    Nothing -> liftEffect $ throw "Can't consume plutip configuration"
  where
  -- TODO: Maybe we can change this for a `traverse` with a environment? 
  loop
    :: ValidatorHash
    -> Maybe (Array KeyWallet)
    -> TransactionHash
    -> AdditionDatum
    -> Contract (TransactionHash /\ AdditionDatum)
  loop valHash' (Just wallets') txId datum =
    case head wallets' of
      Just wallet -> do
        (txId2 /\ datum2) <- withKeyWallet wallet
          $ advanceStateContract txId datum increase
        logState valHash'
        loop valHash' (tail wallets') txId2 datum2
      Nothing -> pure $ txId /\ datum
  loop _ Nothing txId datum = pure $ txId /\ datum

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

logState :: ValidatorHash -> Contract Unit
logState hash = do
  state <- getScriptUtxos hash
  logInfo' $ "State: " <> show state

initialContract :: Contract (TransactionHash /\ AdditionDatum)
initialContract = do
  validator /\ hash <- getValidatorAndHash unit
  let
    lookups :: ScriptLookups.ScriptLookups AdditionValidator
    lookups = ScriptLookups.validator validator
    datum = AdditionDatum $ { lockedAmount: BigInt.fromInt 100 }
    constraints = mustPayToScript hash
      (wrap $ toData datum)
      DatumInline
      mempty
  logInfo' $ "datum: " <> (show :: Datum -> String) (wrap $ toData datum)
  transactionId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed transactionId
  pure $ transactionId /\ datum

advanceStateContract
  :: TransactionHash
  -> AdditionDatum
  -> Int
  -> Contract (TransactionHash /\ AdditionDatum)
advanceStateContract txId datum increase = do
  (validator /\ hash) <- getValidatorAndHash unit
  (maybeTxIn /\ scriptUtxos) <- getScriptInputAndUtxos hash txId
  txIn <- liftMaybe
    (error $ "Can't find UTxO " <> show txId <> " locked by the script.")
    maybeTxIn
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
  logInfo' $ "datum: " <> (show :: Datum -> String) (wrap $ toData newDatum)
  logInfo' $ "redeemer: " <> show (toData redeemer)
  transactionId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed transactionId
  pure $ transactionId /\ newDatum
