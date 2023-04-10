module Seath.Core.ChainBuilder (buildChain) where

import Contract.BalanceTxConstraints
  ( mustSendChangeToAddress
  , mustUseAdditionalUtxos
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class FromData, class ToData)
import Contract.Prelude (fst)
import Contract.ScriptLookups (unspentOutputs)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (class DatumType, class RedeemerType)
import Contract.Transaction
  ( FinalizedTransaction
  , balanceTxWithConstraints
  , createAdditionalUtxos
  )
import Contract.TxConstraints (mustBeSignedBy, mustSpendPubKeyOutput)
import Contract.Utxos (UtxoMap, getWalletUtxos)
import Control.Applicative (pure)
import Control.Monad (bind)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (fold, snoc, uncons)
import Data.Eq ((==))
import Data.Functor ((<$>))
import Data.Map (empty, toUnfoldable)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid ((<>))
import Data.Newtype (unwrap, wrap)
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (discard, ($), (<<<))
import Seath.Core.Types
  ( ChainBuilderState(ChainBuilderState)
  , CoreConfiguration(CoreConfiguration)
  , StateReturn(StateReturn)
  , UserAction
  , changeAddress'
  )
import Seath.Core.Utils (findOwnOutputs, getFinalizedTransactionHash)

-- TODO: Make the the buildChain function to skip transactions with erros
-- in the balancer and to report them at the end.

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
  => CoreConfiguration actionType userStateType validatorType datumType
       redeemerType
  -> Array (UserAction actionType)
  -- | Set it to nothing if this is the first time you run the builder or 
  -- | if you want to get the actual state in the blockchain
  -- | otherwise pass the previously generated output.
  -> Maybe (UtxoMap /\ userStateType)
  -> Contract
       ( Array (FinalizedTransaction /\ UserAction actionType) /\ UtxoMap /\
           userStateType
       )
buildChain configW@(CoreConfiguration config) actions mState = do
  utxos <- getWalletUtxos
  logInfo' $ "buildChain UTXOs: " <> show utxos
  ownPKs <- ownPubKeyHashes
  logInfo' $ "buildChain own PKHs: " <> show ownPKs
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
  => CoreConfiguration actionType userStateType validatorType datumType
       redeemerType
  -> ChainBuilderState actionType userStateType
  -> Contract
       ( Array (FinalizedTransaction /\ UserAction actionType) /\ UtxoMap /\
           userStateType
       )
actions2TransactionsChain
  (CoreConfiguration config)
  (ChainBuilderState builderState) =
  case uncons $ builderState.pendingActions of
    Just { head: userAction, tail: pendingActions } -> do
      let
        lastUtxoMap /\ lastUserState = builderState.lastResult
      (finalizedTransaction /\ newUserState) <- action2Transaction
        (CoreConfiguration config)
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
      actions2TransactionsChain (CoreConfiguration config) newBuilderState
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
  => CoreConfiguration actionType userStateType validatorType datumType
       redeemerType
  -> userStateType
  -> UtxoMap
  -> UserAction actionType
  -> Contract (FinalizedTransaction /\ userStateType)
action2Transaction
  (CoreConfiguration config)
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
      -- Using directly `additionalUtxos` makes the first transaction in the 
      -- chain to fail with "AdditionalUtxoOverlap"  pointing to the 
      -- TransactionInput used in the Script.
      -- Using the differene between `additionalUtxos` and `scriptUtxos` 
      -- makes the redeemer unspendable in the second tranaction
      realAdditionalUtxos =
        if additionalUtxos == scriptUtxos then empty else additionalUtxos
      balanceConstraints =
        mustUseAdditionalUtxos realAdditionalUtxos <>
          mustSendChangeToAddress (changeAddress' userAction)
      constraints = handlerResult.constraints
        <> fold
          ( mustSpendPubKeyOutput <<< fst <$> toUnfoldable
              (unwrap userAction).userUTxOs
          )
        <> mustBeSignedBy (wrap (unwrap userAction).publicKey)
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx
      (handlerResult.lookups <> unspentOutputs (unwrap userAction).userUTxOs)
      constraints
    balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx
      balanceConstraints
    _ <- getFinalizedTransactionHash balancedTx
    -- logInfo' $ "TxHash: " <> show txId
    pure $ balancedTx /\ handlerResult.userState

