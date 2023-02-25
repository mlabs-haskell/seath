module Seath.Test.Exaxmles.Addition.Contract (simpleTest) where

import Contract.Log (logInfo')
import Contract.Monad (Aff, Contract, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.Prelude (liftEffect)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (class DatumType, class RedeemerType, PlutusScript, applyArgs, validatorHash)
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (runPlutipContract, PlutipConfig)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline), mustPayToScript)
import Contract.Wallet (KeyWallet)
import Control.Applicative (pure)
import Control.Monad (bind, (>>=))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Functor ((<$>))
import Data.Monoid ((<>), mempty)
import Data.Newtype (wrap)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Effect.Exception (throw)
import Prelude (($), discard, (<<<))
import Seath.Test.Examples.Addition.Types (AdditionDatum(AdditionDatum), AdditionParams, AdditionRedeemer)
import Seath.Test.Exaxmles.Addition.Validator (validatorScript)

simpleTest :: PlutipConfig -> Aff Unit
simpleTest config = runPlutipContract config distribution onchainActions
  where
  distribution :: Array (Array BigInt)
  distribution = [ [ BigInt.fromInt 1_000_000_000 ] ]

  onchainActions :: Array KeyWallet -> Contract Unit
  onchainActions [ onlyWallet ] = withKeyWallet onlyWallet do
    validator <- wrap <$> importValidator unit
    let
      hash = validatorHash validator

      lookups :: ScriptLookups.ScriptLookups AdditionValidator
      lookups = ScriptLookups.validator validator
      constraints = mustPayToScript hash (wrap <<< toData <<< AdditionDatum $ { lockedAmount: BigInt.fromInt 100 }) DatumInline mempty
    transactionId <- submitTxFromConstraints lookups constraints
    logInfo' $ "Tx ID: " <> show transactionId
    awaitTxConfirmed transactionId
    logInfo' "Confirmed tx"
  onchainActions _ = liftEffect $ throw "Can't consume plutip configuration"

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
