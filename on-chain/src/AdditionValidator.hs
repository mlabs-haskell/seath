{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module AdditionValidator (mkAdditonValidator) where

import PlutusTx.Natural (Natural)
import PlutusTx qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger (ScriptContext)

type AdditionParams = ()

data AdditionDatum = AdditionDatum {lockedAmount :: Natural}
  deriving stock (Generic, Show, Eq)

PlutusTx.makeLift ''AdditionDatum
PlutusTx.makeIsDataIndexed
  ''AdditionDatum
  [('AdditionDatum, 0)]

deriveEq ''AdditionDatum

data AdditionRedemer = AdditionRedeemer {increaseAmount :: Natural}

PlutusTx.makeLift ''AdditionRedeemer
PlutusTx.makeIsDataIndexed
  ''AdditionRedeemer
  [('AdditionRedeemer, 0)]

deriveEq ''AdditionRedeemer


{-# INLINEABLE mkAdditionValidator #-}
mkAdditionValidator :: AdditionParams -> AdditionDatum -> AdditionRedeemer 
                      -> ScriptContext -> Bool
mkAdditionValidator _ datum reedemer  context = 
  case getOutputs context of 
    [(TxOut _ _ outputHash)] -> 
      traceIfFalse "Datum is updated incorrectly" (datumIsUpdated datum redeemer outputHash)
    _ -> trace "Only one Datum was expected"  False

{-# INLINEABLE getOutputs #-}
getOutputs :: ScriptContext -> [TxOut]
getOutputs (ScriptContext (TxInfo{txInfoOutputs}) _) = txInfoOutputs

{-# INLINEABLE datumIsUpdated #-}
datumIsUpdated :: AdditionDatum -> AdditionRedeemer -> DatumHash -> Bool
-- | TODO : check that the datum corresponding to the hash has updated the value according to the needed value
-- bassically we want :  DatumLockValue + RedeeemrValue = DatumValueOfOutputTx
datumIsUpdated = undefined 

typedAdditionValidator ::
  AdditionParams ->
  Scripts.TypedValidator AdditionTypedValidator
typedIDOValidator params =
  Scripts.mkTypedValidator @AdditionTypedValidator
    ( $$(PlutusTx.compile [||mkAdditionValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap =
      Scripts.wrapValidator
        @AdditionDatum
        @AdditionRedemer

getAdditionValidator :: AdditionParams -> Scripts.Validator
getAdditionValidator = Scripts.validatorScript . typedAdditionValidator

getAdditionValidatorHash :: AdditioParams -> Ledger.ValidatorHash
getAdditionValidatorHash = Scripts.validatorHash . typedAdditionValidator

getAdditionVAlidatorAddress :: AdditionParams -> Ledger.Address
getAdditionVAlidatorAddress = Ledger.scriptAddress . getAdditionValidator 
