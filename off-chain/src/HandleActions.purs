module Seath.HandleActions (buildChain) where

import Contract.Address (Address, getNetworkId, validatorHashEnterpriseAddress)
import Contract.BalanceTxConstraints
  ( mustSendChangeToAddress
  , mustUseAdditionalUtxos
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.PlutusData (class FromData, class ToData)
import Contract.Prelude (fst, liftEffect)
import Contract.ScriptLookups (unspentOutputs)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (class DatumType, class RedeemerType, ValidatorHash)
import Contract.Transaction
  ( FinalizedTransaction
  , TransactionHash
  , TransactionOutputWithRefScript
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
import Data.Eq ((==))
import Data.Functor ((<$>))
import Data.Lens ((^.))
import Data.Map (toUnfoldable)
import Data.Map as Map
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
import Seath.Utils (findOwnOutputs, getFinalizedTransactionHash)

-- | Use this function to run Seath chain generation
buildChain
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
  -> Array (UserAction actionType)
  -- | Set it to nothing if this is the first time you run the builder or 
  -- | if you want to get the actual state in the blockchain
  -- | otherwise pass the previously generated output.
  -> Maybe (UtxoMap /\ userStateType)
  -> Contract
       ( Array (FinalizedTransaction /\ UserAction actionType) /\ UtxoMap /\
           userStateType
       )
buildChain configW@(SeathConfig config) actions mState = do
  lastResult <- case mState of
    Just state -> pure state
    Nothing -> config.queryBlockchainState
  let
    initialBuilderState = wrap
      { pendingActions: actions
      , finalizedTransactions: []
      , lastResult
      }
  actions2TransactionsChain configW initialBuilderState

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
       ( Array (FinalizedTransaction /\ UserAction actionType) /\ UtxoMap /\
           userStateType
       )
actions2TransactionsChain (SeathConfig config) (ChainBuilderState builderState) =
  case uncons $ builderState.pendingActions of
    Just { head: userAction, tail: pendingActions } -> do
      let
        lastUtxoMap /\ lastUserState = builderState.lastResult
      (finalizedTransaction /\ newUserState) <- action2Transaction
        (SeathConfig config)
        lastUserState
        lastUtxoMap
        userAction
      newUtxoMap <- createAdditionalUtxos finalizedTransaction
      let
        ( finalizedTransactions
            :: Array (FinalizedTransaction /\ UserAction actionType)
        ) = snoc
          builderState.finalizedTransactions
          (finalizedTransaction /\ userAction)
        (newBuilderState :: ChainBuilderState actionType userStateType) =
          ChainBuilderState
            { lastResult: newUtxoMap /\ newUserState
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
  -> userStateType
  -> UtxoMap
  -> UserAction actionType
  -> Contract (FinalizedTransaction /\ userStateType)
action2Transaction
  (SeathConfig config)
  userState
  additionalUtxos
  userAction =
  do
    let
      -- script UTXOs:
      -- on next transaction in chain use outputs generated by previous transaction
      -- (I think it should be safe, as we want to build chain against only thouse outputs,
      --  that user decided to use as state for chain of transactions)
      getUtxosInScript = findOwnOutputs config.stateVaildatorHash
        additionalUtxos
    (StateReturn handlerResult) <- config.actionHandler userAction userState
      getUtxosInScript
    scriptUtxos <- getUtxosInScript
    let
      balanceConstraints =
        mustUseAdditionalUtxos (Map.difference additionalUtxos scriptUtxos) <>
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
    logInfo' $ "UnbalancedTx: " <> show unbalancedTx
    balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx
      balanceConstraints
    txId <- getFinalizedTransactionHash balancedTx
    logInfo' $ "TxHash: " <> show txId
    pure $ balancedTx /\ handlerResult.userState

