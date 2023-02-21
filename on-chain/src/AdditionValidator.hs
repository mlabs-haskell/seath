{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module AdditionValidator
  ( additionScript,
    mkAdditionValidator,
    getAdditionValidator,
  )
where

import Data.Eq.Deriving (deriveEq)
import GHC.Generics (Generic)
import Ledger (scriptHashAddress)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Validators
import Plutus.V2.Ledger.Api
  ( Address,
    DatumHash,
    Script,
    ScriptContext (ScriptContext),
    TxInfo (TxInfo, txInfoOutputs),
    ValidatorHash,
    fromCompiledCode,
  )
import Plutus.V2.Ledger.Tx (TxOut)
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.Trace qualified as PlutusTx
import Prelude qualified as Hask

type AdditionParams = ()

-- FIXME: no instance for Natural
data AdditionDatum = AdditionDatum {lockedAmount :: Hask.Integer}
  deriving stock (Generic, Hask.Show)

PlutusTx.makeLift ''AdditionDatum
PlutusTx.makeIsDataIndexed
  ''AdditionDatum
  [('AdditionDatum, 0)]

deriveEq ''AdditionDatum

-- FIXME: no instance for Natural
data AdditionRedeemer = AdditionRedeemer {increaseAmount :: Hask.Integer}

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
      PlutusTx.traceIfFalse
        "Datum is updated incorrectly"
        (datumIsUpdated datum redeemer datHash)
    _ -> trace "Only one Datum was expected" False

{-# INLINEABLE getOutputs #-}
getOutputs :: ScriptContext -> [TxOut]
getOutputs (ScriptContext TxInfo {txInfoOutputs} _) = txInfoOutputs

{-# INLINEABLE datumIsUpdated #-}
datumIsUpdated :: AdditionDatum -> AdditionRedeemer -> DatumHash -> Bool

{-# INLINEABLE getDatumHash #-}
getDatumHash :: TxOut -> Maybe DatumHash
getDatumHash = Hask.undefined -- TODO

-- | TODO : check that the datum corresponding to the hash has updated the value according to the needed value
-- bassically we want :  DatumLockValue + RedeeemrValue = DatumValueOfOutputTx
datumIsUpdated = Hask.undefined

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

getAdditionVAlidatorAddress :: AdditionParams -> Address
getAdditionVAlidatorAddress = scriptHashAddress . getAdditionValidatorHash

additionScript :: Script
additionScript = fromCompiledCode $$(PlutusTx.compile [||mkAdditionValidator||])