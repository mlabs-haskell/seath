module Seath.HandleActions where

import Contract.BalanceTxConstraints (mustUseAdditionalUtxos)
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class FromData, class ToData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (class DatumType, class RedeemerType)
import Contract.Transaction (FinalizedTransaction, balanceTx, balanceTxWithConstraints, createAdditionalUtxos)
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Array (snoc, uncons)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (($))
import Seath.Types (StateReturn(StateReturn), UserAction)

-- TODO: put the handler in a Env together with the leader pubkeyhas and 
-- use it to add the constraint that transaction must be signed by both 
-- leader and user

actions2UTxOChain
  :: forall (a :: Type) (userType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => ToData userType
  => FromData userType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> userType
       -> Contract
            (StateReturn validatorType redeemerType datumType userType)
     )
  -> Array (UserAction a)
  -> userType
  -> Contract (Array FinalizedTransaction /\ userType)
actions2UTxOChain actionHandler actions state =
  case uncons actions of
    Just { head, tail } -> do
      finalizedTx /\ newState <- action2UTxO actionHandler head state
      actions2UTxOChainFromFinalizedTx actionHandler finalizedTx []
        tail
        newState
    Nothing -> pure $ [] /\ state

actions2UTxOChainFromFinalizedTx
  :: forall (a :: Type) (userType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => ToData userType
  => FromData userType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> userType
       -> Contract
            (StateReturn validatorType redeemerType datumType userType)
     )
  -> FinalizedTransaction
  -> Array FinalizedTransaction
  -> Array (UserAction a)
  -> userType
  -> Contract (Array FinalizedTransaction /\ userType)
actions2UTxOChainFromFinalizedTx
  actionHandler
  oldUtxo
  processed
  unprocessed
  state =
  case uncons unprocessed of
    Just { head: action, tail: newUnprocessed } ->
      do
        tobeSigned /\ newState <- action2UTxOFromFinalizedTx actionHandler
          oldUtxo
          action
          state
        actions2UTxOChainFromFinalizedTx actionHandler tobeSigned
          (snoc processed tobeSigned)
          newUnprocessed
          newState
    Nothing -> pure $ processed /\ state

action2UTxOFromFinalizedTx
  :: forall (a :: Type) (userType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => ToData userType
  => FromData userType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> userType
       -> Contract
            (StateReturn validatorType redeemerType datumType userType)
     )
  -> FinalizedTransaction
  -> UserAction a
  -> userType
  -> Contract (FinalizedTransaction /\ userType)
action2UTxOFromFinalizedTx actionHandler oldUTxO userAction state = do
  (StateReturn handlerResult) <- actionHandler userAction state
  additionalUtxos <- createAdditionalUtxos oldUTxO
  let balanceConstraints = mustUseAdditionalUtxos additionalUtxos
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx handlerResult.lookups
    handlerResult.constraints
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx
    balanceConstraints
  pure $ balancedTx /\ handlerResult.userState

action2UTxO
  :: forall (a :: Type) (userType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => ToData userType
  => FromData userType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> userType
       -> Contract
            (StateReturn validatorType redeemerType datumType userType)
     )
  -> UserAction a
  -> userType
  -> Contract (FinalizedTransaction /\ userType)
action2UTxO actionHandler userAction state= do
  (StateReturn handlerResult) <- actionHandler userAction state
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx handlerResult.lookups handlerResult.constraints
  balancedTx <- liftedE $ balanceTx unbalancedTx
  pure $ balancedTx /\ handlerResult.userState
