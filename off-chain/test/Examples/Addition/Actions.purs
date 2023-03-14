module Seath.Test.Examples.Addition.Actions
  ( fixedValidator
  , fixedValidatorHash
  , handleAction
  , getScriptUtxosFromChain
  ) where

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM, throwContractError)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (Validator, ValidatorHash, applyArgs, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.TxConstraints
  ( DatumPresence(..)
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (UtxoMap)
import Control.Applicative (pure)
import Control.Monad (bind, (>>=))
import Data.Functor ((<$>))
import Data.Map (toUnfoldable)
import Data.Monoid (mempty, (<>))
import Data.Newtype (unwrap, wrap)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Data.Unit (unit)
import Prelude (($), (+), (<<<))
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum(AdditionDatum)
  , AdditionRedeemer(AdditionRedeemer)
  , AdditionState
  , AdditionValidator
  )
import Seath.Test.Examples.Addition.Validator (validatorScript)
import Seath.Test.Examples.Utils (getScriptUtxos)
import Seath.Types (StateReturn) as Seath.Types
import Seath.Types (UserAction)

-- todo: maybe check here what we returning
getScriptUtxosFromChain ∷ Contract UtxoMap
getScriptUtxosFromChain = do
  valHash <- fixedValidatorHash
  getScriptUtxos valHash

handleAction
  :: UserAction AdditionAction
  -> AdditionState
  -> Contract UtxoMap
  -> Contract
       ( Seath.Types.StateReturn AdditionValidator AdditionDatum
           AdditionRedeemer
           AdditionState
       )
handleAction userAction lockedValue getScriptUtxo =
  case (unwrap userAction).action of
    AddAmount increase -> do
      val <- fixedValidator
      valHash <- fixedValidatorHash
      scriptUtxos <- getScriptUtxo
      case toUnfoldable scriptUtxos of
        [ inp /\ _outp ] -> do
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
                <> mustSpendScriptOutput inp (wrap $ toData redeemer)
          pure $ wrap
            { constraints, lookups, userState: (unwrap newDatum).lockedAmount }
        _ -> throwContractError $
          "Unexpected set of UTXOs at script addres. Should be only one UTXO, but got:\n"
            <> show scriptUtxos

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
