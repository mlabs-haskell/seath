module Seath.Test.Examples.Addition.Actions
  ( handleActionFromFinalizedTransaction
  , initialState
  , handleActionFromBlockChain
  , fixedValidator
  , fixedValidatorHash
  ) where

import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (OutputDatum(..), fromData, toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (Validator, ValidatorHash, applyArgs, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( FinalizedTransaction
  , TransactionHash
  , createAdditionalUtxos
  , lookupTxHash
  )
import Contract.TxConstraints
  ( DatumPresence(..)
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Control.Applicative (pure)
import Control.Monad (bind, (>>=))
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Plutus.Types.Transaction (_datum, _output)
import Data.Array (head)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Functor ((<$>))
import Data.Lens (view)
import Data.Monoid (mempty, (<>))
import Data.Newtype (unwrap, wrap)
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Data.Unit (unit)
import Effect.Aff (error)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Prelude (($), (+), (<<<))
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum(AdditionDatum)
  , AdditionRedeemer(AdditionRedeemer)
  , AdditionValidator
  )
import Seath.Test.Examples.Addition.Validator (validatorScript)
import Seath.Test.Examples.Utils
  ( getFinalizedTransactionHash
  , getScriptInput
  , getScriptInputAndUtxos
  )
import Seath.Types (StateReturn) as Seath.Types
import Seath.Types (UserAction)

type AdditionState = BigInt

initialState :: AdditionState
initialState = BigInt.fromInt 100

handleActionFromFinalizedTransaction
  :: UserAction AdditionAction
  -> AdditionState
  -> FinalizedTransaction
  -> Contract
       ( Seath.Types.StateReturn AdditionValidator AdditionDatum
           AdditionRedeemer
           AdditionState
       )
handleActionFromFinalizedTransaction userAction lockedValue oldTransaction =
  case (unwrap userAction).action of
    AddAmount increase -> do
      val <- fixedValidator
      valHash <- fixedValidatorHash
      txId <- getFinalizedTransactionHash oldTransaction
      scriptUtxos <- createAdditionalUtxos oldTransaction
      txIn <-
        liftMaybe
          (error $ "Can't find UTxO " <> show txId <> " locked by the script.")
          $ getScriptInput txId scriptUtxos
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

handleActionFromBlockChain
  :: UserAction AdditionAction
  -> TransactionHash
  -> Contract
       ( Seath.Types.StateReturn AdditionValidator AdditionDatum
           AdditionRedeemer
           AdditionState
       )
handleActionFromBlockChain action txId =
  case (unwrap action).action of
    AddAmount increase ->
      do
        val <- fixedValidator
        valHash <- fixedValidatorHash
        (maybeTxIn /\ scriptUtxos) <- getScriptInputAndUtxos valHash txId
        txIn <- liftMaybe
          (error $ "Can't find UTxO " <> show txId <> " locked by the script.")
          maybeTxIn
        output <-
          liftMaybe
            ( error $ "Can't find old datum " <> show txId <>
                " locked by the script."
            )
            $ head
                (lookupTxHash txId scriptUtxos)
        (oldDatum :: AdditionDatum) <-
          case view _datum $ view _output (unwrap output).output of
            OutputDatum d -> liftMaybe (error "can't decode datum") $ fromData
              (unwrap d)
            _ -> liftEffect $ throw "can't find old datum"
        let
          redeemer = AdditionRedeemer
            { increaseAmount: increase }
          newDatum = AdditionDatum
            { lockedAmount: (unwrap oldDatum).lockedAmount + increase
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
