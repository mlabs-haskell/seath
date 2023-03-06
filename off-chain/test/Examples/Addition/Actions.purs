module Seath.Test.Examples.Addition.Actions
  ( action2ConstraintsAndLookup
  , initialState
  ) where

import Prelude

import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (Validator, ValidatorHash, applyArgs, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (FinalizedTransaction(..), TransactionHash)
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Control.Monad.Error.Class (liftMaybe)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Effect.Aff (error)
import Seath.Data (UserAction)
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum(AdditionDatum)
  , AdditionRedeemer(AdditionRedeemer)
  , AdditionValidator
  )
import Seath.Test.Examples.Addition.Validator (validatorScript)
import Seath.Test.Examples.Utils (getScriptInputAndUtxos)
import Seath.Types (StateReturn) as Seath.Types

type AdditionState = BigInt

initialState :: AdditionState
initialState = BigInt.fromInt 100

action2ConstraintsAndLookup
  :: UserAction AdditionAction
  -> AdditionState
  -> FinalizedTransaction
  -> Contract
       ( Seath.Types.StateReturn AdditionValidator AdditionRedeemer
           AdditionDatum
           AdditionState
       )
action2ConstraintsAndLookup userAction lockedValue oldTransaction =
  case (unwrap userAction).action of
    AddAmount increase -> do
      val <- fixedValidator
      valHash <- fixedValidatorHash
      (maybeTxIn /\ scriptUtxos) <- getScriptInputAndUtxos valHash
        txId
      txIn <- liftMaybe
        (error $ "Can't find UTxO " <> show txId <> " locked by the script.")
        maybeTxIn
      let
        redeemer = AdditionRedeemer { increaseAmount: increase }
        newDatum = AdditionDatum
          { lockedAmount: lockedValue + increase
          }

        lookups :: ScriptLookups.ScriptLookups AdditionValidator
        lookups = ScriptLookups.validator val
          <> ScriptLookups.unspentOutputs scriptUtxos
        constraints =
          mustPayToScript valHash (wrap <<< toData $ newDatum)
            DatumInline
            mempty
            <> mustSpendScriptOutput txIn (wrap $ toData redeemer)
      pure $ wrap
        { constraints, lookups, userState: (unwrap newDatum).lockedAmount }

fixedValidator :: Contract Validator
fixedValidator = do
  validator <- liftedM "can't decode validator" $ pure
    ( decodeTextEnvelope validatorScript
        >>= plutusScriptV2FromEnvelope
    )
  script <- liftedE $ pure (applyArgs validator [ toData unit ])
  pure $ wrap script

fixedValidatorHash :: Contract ValidatorHash
fixedValidatorHash = validatorHash <$> fixedValidator
