{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module AdditionValidator (
  additionScript,
  mkAdditionValidator,
  getAdditionValidator,
) where

import Data.Eq.Deriving (deriveEq)
import GHC.Generics (Generic)
import Ledger (Datum, scriptHashAddress)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Validators
import Plutus.V2.Ledger.Api (
  Address,
  Datum (Datum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (ScriptContext),
  TxInfo (TxInfo, txInfoOutputs),
  TxOut (TxOut),
  ValidatorHash,
  fromCompiledCode,
  toBuiltinData,
 )
import PlutusTx qualified
import PlutusTx.Integer (Integer)
import PlutusTx.Prelude (Bool (False), Maybe (Just, Nothing), (+), (.), (==))
import PlutusTx.Trace qualified as Trace
import Prelude qualified as Hask

type AdditionParams = ()

data AdditionDatum = AdditionDatum {lockedAmount :: Integer}
  deriving stock (Generic, Hask.Show)

PlutusTx.makeLift ''AdditionDatum
PlutusTx.makeIsDataIndexed
  ''AdditionDatum
  [('AdditionDatum, 0)]

deriveEq ''AdditionDatum

data AdditionRedeemer = AdditionRedeemer {increaseAmount :: Integer}

PlutusTx.makeLift ''AdditionRedeemer
PlutusTx.makeIsDataIndexed
  ''AdditionRedeemer
  [('AdditionRedeemer, 0)]

deriveEq ''AdditionRedeemer

{-# INLINEABLE mkAdditionValidator #-}
mkAdditionValidator ::
  AdditionParams ->
  AdditionDatum ->
  AdditionRedeemer ->
  ScriptContext ->
  Bool
mkAdditionValidator _ datum redeemer context =
  case getOutputs context of
    [getDatumHash -> Just datHash] ->
      -- TODO : Check that the redeemer only contains positive values
      Trace.traceIfFalse
        "Datum is updated incorrectly"
        (datumIsUpdated datum redeemer datHash)
    _ -> Trace.trace "Only one Datum was expected" False

{-# INLINEABLE getOutputs #-}
getOutputs :: ScriptContext -> [TxOut]
getOutputs (ScriptContext TxInfo {txInfoOutputs} _) = txInfoOutputs

{-# INLINEABLE datumIsUpdated #-}
datumIsUpdated :: AdditionDatum -> AdditionRedeemer -> Datum -> Bool
datumIsUpdated (AdditionDatum locked) (AdditionRedeemer toAdd) newDatum =
  let expectedLockedValue = locked + toAdd
      expectedDatum :: Datum
      expectedDatum = (Datum . toBuiltinData . AdditionDatum) expectedLockedValue
   in expectedDatum == newDatum

{-# INLINEABLE getDatumHash #-}
-- we can use `import Plutus.V2.Ledger.Tx (outDatum)` instead
getDatumHash :: TxOut -> Maybe Datum
getDatumHash (TxOut _ _ (OutputDatum datum) _) = Just datum
getDatumHash _ = Nothing

data Addition

instance Validators.ValidatorTypes Addition where
  type RedeemerType Addition = AdditionRedeemer
  type DatumType Addition = AdditionDatum

typedAdditionValidator ::
  AdditionParams ->
  Validators.TypedValidator Addition
typedAdditionValidator params =
  Validators.mkTypedValidator @Addition
    ( $$(PlutusTx.compile [||mkAdditionValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap =
      Validators.mkUntypedValidator

getAdditionValidator :: AdditionParams -> Scripts.Validator
getAdditionValidator = Scripts.validatorScript . typedAdditionValidator

getAdditionValidatorHash :: AdditionParams -> ValidatorHash
getAdditionValidatorHash = Scripts.validatorHash . typedAdditionValidator

getAdditionValidatorAddress :: AdditionParams -> Address
getAdditionValidatorAddress = scriptHashAddress . getAdditionValidatorHash

additionScript :: Script
additionScript = fromCompiledCode $$(PlutusTx.compile [||mkAdditionValidator||])
