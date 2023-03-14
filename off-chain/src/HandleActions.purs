module Seath.HandleActions where

import Contract.BalanceTxConstraints
  ( mustSendChangeToAddress
  , mustUseAdditionalUtxos
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class FromData, class ToData)
import Contract.Prelude (fst, liftEffect)
import Contract.ScriptLookups (unspentOutputs)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (class DatumType, class RedeemerType)
import Contract.Transaction
  ( FinalizedTransaction
  , TransactionHash
  , balanceTxWithConstraints
  , createAdditionalUtxos
  )
import Contract.TxConstraints (mustBeSignedBy, mustSpendPubKeyOutput)
import Contract.Utxos (UtxoMap)
import Control.Applicative (pure)
import Control.Monad (bind)
import Ctl.Internal.Hashing as Ctl.Internal.Hashing
import Ctl.Internal.Serialization as Ctl.Internal.Serialization
import Data.Array (fold, snoc, uncons)
import Data.Either (Either(Left, Right))
import Data.Functor ((<$>))
import Data.Map (empty, toUnfoldable)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid ((<>))
import Data.Newtype (unwrap, wrap)
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (discard, ($), (<<<))
import Seath.Types
  ( ChainBuilderState(ChainBuilderState)
  , SeathConfig(SeathConfig)
  , StateReturn(StateReturn)
  , UserAction
  )

actions2TransactionsChain
  :: forall (actionType :: Type) (userStateType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => SeathConfig actionType userStateType validatorType datumType redeemerType
  -> ChainBuilderState actionType userStateType
  -> Contract
       ( Array (FinalizedTransaction /\ UserAction actionType) /\ Either
           TransactionHash
           (FinalizedTransaction /\ userStateType)
       )
actions2TransactionsChain (SeathConfig config) (ChainBuilderState builderState) =
  case uncons $ builderState.pendingActions of
    Just { head: userAction, tail: pendingActions } -> do
      (finalizedTransaction /\ newUserState) <- case builderState.lastResult of
        Right (finalizedTransaction /\ userState) -> do
          state <- config.finalizedTxHandler userAction userState
            finalizedTransaction
          additionalUtxos <- createAdditionalUtxos finalizedTransaction
          action2Transaction (SeathConfig config) state additionalUtxos
            userAction
        Left txId -> do
          state <- config.onchainHandler userAction txId
          action2Transaction (SeathConfig config) state empty userAction
      let
        ( finalizedTransactions
            :: Array (FinalizedTransaction /\ UserAction actionType)
        ) = snoc
          builderState.finalizedTransactions
          (finalizedTransaction /\ userAction)
        (newBuilderState :: ChainBuilderState actionType userStateType) =
          ChainBuilderState
            { lastResult: Right (finalizedTransaction /\ newUserState)
            , finalizedTransactions: finalizedTransactions
            , pendingActions: pendingActions
            }
      actions2TransactionsChain (SeathConfig config) newBuilderState
    Nothing -> pure $ builderState.finalizedTransactions /\
      builderState.lastResult

action2Transaction
  :: forall (actionType :: Type) (userStateType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => SeathConfig actionType userStateType validatorType datumType redeemerType
  -> StateReturn validatorType datumType redeemerType userStateType
  -> UtxoMap
  -> UserAction actionType
  -> Contract (FinalizedTransaction /\ userStateType)
action2Transaction
  (SeathConfig config)
  stateReturn
  additionalUtxos
  userAction =
  do
    let
      (StateReturn handlerResult) = stateReturn
      balanceConstraints = mustUseAdditionalUtxos additionalUtxos <>
        mustSendChangeToAddress (unwrap userAction).changeAddress -- <> mustUseAdditionalUtxos (unwrap userAction).userUTxo
      constraints = handlerResult.constraints
        -- TODO : does we really need the signature of the leader?
        -- It can be useful to have a track onchain of the leader 
        -- actions.
        <> fold
          ( mustSpendPubKeyOutput <<< fst <$> toUnfoldable
              (unwrap userAction).userUTxo
          )
        <> mustBeSignedBy (wrap config.leader)
        <> mustBeSignedBy (wrap (unwrap userAction).publicKey)
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx
      (handlerResult.lookups <> unspentOutputs (unwrap userAction).userUTxo)
      constraints
    balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx
      balanceConstraints
    txId <- getFinalizedTransactionHash balancedTx
    logInfo' $ "TxHash: " <> show txId
    pure $ balancedTx /\ handlerResult.userState

getFinalizedTransactionHash :: FinalizedTransaction -> Contract TransactionHash
getFinalizedTransactionHash fTx = do
  liftEffect $ Ctl.Internal.Hashing.transactionHash <$>
    Ctl.Internal.Serialization.convertTransaction (unwrap fTx)
