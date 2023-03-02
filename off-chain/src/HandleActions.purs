module Seath.HandleActions where

import Contract.BalanceTxConstraints (mustUseAdditionalUtxos)
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class FromData, class ToData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (class DatumType, class RedeemerType)
import Contract.Transaction
  ( FinalizedTransaction
  , balanceTx
  , balanceTxWithConstraints
  , createAdditionalUtxos
  )
import Contract.TxConstraints (TxConstraints)
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Array (snoc, uncons)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (($))
import Seath.Data (UserAction)

-- TODO: put the handler in a Env together with the leader pubkeyhas and 
-- use it to add the constraint that transaction must be signed by both 
-- leader and user

actions2UTxOChain
  :: forall (a :: Type) (validatorType :: Type) (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> Contract
            ( TxConstraints redeemerType datumType /\ ScriptLookups
                validatorType
            )
     )
  -> Array (UserAction a)
  -> Contract (Array FinalizedTransaction)
actions2UTxOChain actionHandler actions =
  case uncons actions of
    Just { head, tail } -> do
      finalizedTx <- action2UTxO actionHandler head
      actions2UTxOChainFromFinalizedTx actionHandler finalizedTx []
        tail
    Nothing -> pure []

actions2UTxOChainFromFinalizedTx
  :: forall (a :: Type) (validatorType :: Type) (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> Contract
            ( TxConstraints redeemerType datumType /\ ScriptLookups
                validatorType
            )
     )
  -> FinalizedTransaction
  -> Array FinalizedTransaction
  -> Array (UserAction a)
  -> Contract (Array FinalizedTransaction)
actions2UTxOChainFromFinalizedTx actionHandler oldUtxo processed unprocessed =
  case uncons unprocessed of
    Just { head: action, tail: newUnprocessed } ->
      do
        tobeSigned <- action2UTxOFromFinalizedTx actionHandler oldUtxo action
        actions2UTxOChainFromFinalizedTx actionHandler tobeSigned
          (snoc processed tobeSigned)
          newUnprocessed
    Nothing -> pure $ processed

action2UTxOFromFinalizedTx
  :: forall (a :: Type) (validatorType :: Type) (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> Contract
            ( TxConstraints redeemerType datumType /\ ScriptLookups
                validatorType
            )
     )
  -> FinalizedTransaction
  -> UserAction a
  -> Contract FinalizedTransaction
action2UTxOFromFinalizedTx actionHandler oldUTxO userAction = do
  (constraints /\ lookups) <- actionHandler userAction
  additionalUtxos <- createAdditionalUtxos oldUTxO
  let balanceConstraints = mustUseAdditionalUtxos additionalUtxos
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  liftedE $ balanceTxWithConstraints unbalancedTx balanceConstraints

action2UTxO
  :: forall (a :: Type) (validatorType :: Type) (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> Contract
            ( TxConstraints redeemerType datumType /\ ScriptLookups
                validatorType
            )
     )
  -> UserAction a
  -> Contract FinalizedTransaction
action2UTxO actionHandler userAction = do
  (constraints /\ lookups) <- actionHandler userAction
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  liftedE $ balanceTx unbalancedTx
