module Seath.Test.Examples.Addition.ContractUtils
  ( buildAdditionCoreConfig
  , initialSeathContract
  ) where

import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts
  ( PlutusScript
  , Validator
  , ValidatorHash
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline), mustPayToScript)
import Control.Applicative (pure)
import Control.Monad (bind, (>>=))
import Data.BigInt (BigInt)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (unit)
import Prelude (discard, ($))
import Seath.Core.Types (CoreConfiguration(..))
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.Types
  ( AdditionAction
  , AdditionDatum(AdditionDatum)
  , AdditionParams
  , AdditionRedeemer(..)
  , AdditionState
  , AdditionValidator
  , initialState
  )
import Seath.Test.Examples.Addition.Validator (validatorScript)

newtype ActionNumber = ActionNumber Int

derive instance Newtype ActionNumber _

initialSeathContract :: Contract AdditionState
initialSeathContract = do
  validator /\ hash <- getValidatorAndHash unit
  let
    lookups :: ScriptLookups.ScriptLookups AdditionValidator
    lookups = ScriptLookups.validator validator
    datum = AdditionDatum $ { lockedAmount: initialState }
    constraints = mustPayToScript hash
      (wrap $ toData datum)
      DatumInline
      mempty
  -- logInfo' $ "datum: " <> (show :: Datum -> String) (wrap $ toData datum)
  transactionId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed transactionId
  pure initialState

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

buildAdditionCoreConfig
  ∷ Contract
      ( CoreConfiguration
          AdditionAction
          BigInt
          AdditionValidator
          AdditionDatum
          AdditionRedeemer
      )
buildAdditionCoreConfig = do
  vaildatorHash <- Addition.fixedValidatorHash
  leaderPkh <- getPublicKeyHash
  pure $ CoreConfiguration
    { leader: leaderPkh
    , stateVaildatorHash: vaildatorHash
    , actionHandler: Addition.handleAction
    , queryBlockchainState: Addition.queryBlockchainState
    }
