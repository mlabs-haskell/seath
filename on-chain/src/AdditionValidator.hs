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
  OutputDatum (NoOutputDatum, OutputDatum),
  Script,
  ScriptContext (ScriptContext),
  TxInfo (TxInfo, txInfoOutputs),
  TxOut (TxOut),
  ValidatorHash,
  fromBuiltinData,
  fromCompiledCode,
  toBuiltinData,
 )
import PlutusTx qualified
import PlutusTx.Bool (Bool (False, True))
import PlutusTx.Integer (Integer)
import PlutusTx.Maybe (Maybe (Just, Nothing), maybe)
import PlutusTx.Prelude (
  Applicative,
  BuiltinData,
  BuiltinString,
  Functor,
  check,
  filter,
  fmap,
  pure,
  ($),
  (&&),
  (+),
  (.),
  (<),
  (<$>),
  (<*>),
  (<>),
  (==),
 )
import PlutusTx.Trace qualified as Trace

type AdditionParams = ()

data AdditionDatum = AdditionDatum {lockedAmount :: Integer}
  deriving stock (Generic)

PlutusTx.makeLift ''AdditionDatum
PlutusTx.makeIsDataIndexed
  ''AdditionDatum
  [('AdditionDatum, 0)]

deriveEq ''AdditionDatum

data AdditionRedeemer = AdditionRedeemer {increaseAmount :: Integer}
  deriving stock (Generic)

PlutusTx.makeLift ''AdditionRedeemer
PlutusTx.makeIsDataIndexed
  ''AdditionRedeemer
  [('AdditionRedeemer, 0)]

deriveEq ''AdditionRedeemer

data Result a
  = Ok a
  | Err BuiltinString

instance Functor Result where
  {-# INLINEABLE fmap #-}
  fmap f (Ok x) = Ok (f x)
  fmap _ (Err e) = Err e

instance Applicative Result where
  {-# INLINEABLE pure #-}
  pure = Ok
  {-# INLINEABLE (<*>) #-}
  Ok f <*> Ok x = Ok (f x)
  Err e <*> Ok _ = Err e
  Ok _ <*> Err e = Err e
  Err e1 <*> Err e2 = Err (e1 <> "; " <> e2)

{-# INLINEABLE err #-}
err :: BuiltinString -> Result a
err = Err

{-# INLINEABLE foldResult #-}
foldResult :: (BuiltinString -> b) -> (a -> b) -> Result a -> b
foldResult _ onOk (Ok x) = onOk x
foldResult onErr _ (Err e) = onErr e

{-# INLINEABLE mkAdditionValidator #-}
mkAdditionValidator ::
  AdditionParams ->
  AdditionDatum ->
  AdditionRedeemer ->
  ScriptContext ->
  Bool
mkAdditionValidator _ datum redeemer context =
  case filter hasOutputDatum $ getOutputs context of
    [getDatumHash -> Just datHash] ->
      Trace.trace "Checking redeemer and Datum" $
        Trace.traceIfFalse "Redeemer isn't updated" (0 < increaseAmount redeemer)
          && Trace.traceIfFalse
            "Datum is updated incorrectly"
            (datumIsUpdated datum redeemer datHash)
    _ -> Trace.trace "Only one UTxO with attached Datum was expected" False

mkAdditionValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAdditionValidator' params datum redeemer context =
  foldResult Trace.traceError check $
    mkAdditionValidator
      <$> maybe (err "Failed to parse params") pure (fromBuiltinData params)
      <*> maybe (err "Failed to parse datum") pure (fromBuiltinData datum)
      <*> maybe (err "Failed to parse redeemer") pure (fromBuiltinData redeemer)
      <*> maybe (err "Failed to parse context") pure (fromBuiltinData context)

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

hasOutputDatum :: TxOut -> Bool
hasOutputDatum (TxOut _ _ NoOutputDatum _) = False
hasOutputDatum _ = True

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
additionScript = fromCompiledCode $$(PlutusTx.compile [||mkAdditionValidator'||])
