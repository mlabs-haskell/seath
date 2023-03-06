module Seath.HandleActions where

import Contract.BalanceTxConstraints (mustUseAdditionalUtxos)
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class FromData, class ToData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (class DatumType, class RedeemerType)
import Contract.Transaction (FinalizedTransaction, TransactionHash, balanceTx, balanceTxWithConstraints, createAdditionalUtxos)
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Array (snoc, uncons)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (($))
import Seath.Types (StateReturn(StateReturn), UserAction)

-- TODO : Add a function that can get the first state from the blockchain
-- and then begin to process actions

-- TODO: put the handler in a Env together with the leader pubkeyhas and 
-- use it to add the constraint that transaction must be signed by both 
-- leader and user

actions2UTxOChain
  :: forall (a :: Type) (userStateType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> userStateType
       -> TransactionHash
       -> Contract
            (StateReturn validatorType redeemerType datumType userStateType)
     )
  -> Array (UserAction a)
  -> userStateType
  -> Contract (Array FinalizedTransaction /\ userStateType)
actions2UTxOChain actionHandler actions state =
  case uncons actions of
    Just { head, tail } -> do
      finalizedTx /\ newState <- action2UTxO actionHandler head state
      actions2UTxOChainFromFinalizedTx actionHandler finalizedTx []
        tail
        newState
    Nothing -> pure $ [] /\ state

actions2UTxOChainFromFinalizedTx
  :: forall (a :: Type) (userStateType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> userStateType
       -> TransactionHash
       -> Contract
            (StateReturn validatorType redeemerType datumType userStateType)
     )
  -> FinalizedTransaction
  -> Array FinalizedTransaction
  -> Array (UserAction a)
  -> userStateType
  -> Contract (Array FinalizedTransaction /\ userStateType)
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
  :: forall (a :: Type) (userStateType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> userStateType
       -> TransactionHash
       -> Contract
            (StateReturn validatorType redeemerType datumType userStateType)
     )
  -> FinalizedTransaction
  -> UserAction a
  -> userStateType
  -> Contract (FinalizedTransaction /\ userStateType)
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
  :: forall (a :: Type) (userStateType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => ( UserAction a
       -> userStateType
       -> TransactionHash
       -> Contract
            (StateReturn validatorType redeemerType datumType userStateType)
     )
  -> UserAction a
  -> userStateType
  -> Contract (FinalizedTransaction /\ userStateType)
action2UTxO actionHandler userAction state = do
  (StateReturn handlerResult) <- actionHandler userAction state
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx handlerResult.lookups
    handlerResult.constraints
  balancedTx <- liftedE $ balanceTx unbalancedTx
  pure $ balancedTx /\ handlerResult.userState
