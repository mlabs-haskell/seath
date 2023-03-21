module Seath.Test.Examples.Addition.ContractSeath (mainTest) where

import Contract.Log (logInfo')
import Contract.Monad (Contract, throwContractError)
import Contract.Prelude
  ( either
  , hush
  , isJust
  , unless
  , unwrap
  , when
  , (/=)
  , (<$>)
  , (>>=)
  )
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (withKeyWallet)
import Control.Monad (bind)
import Control.Monad.Error.Class (try)
import Data.Array (last, unzip)
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Data.Either (note)
import Data.Functor (map)
import Data.List (head)
import Data.Map (size, values)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Data.Unit (Unit)
import Prelude (discard, pure, ($))
import Seath.HandleActions (buildChain)
import Seath.Test.Examples.Addition.Actions
  ( fixedValidatorHash
  , handleAction
  , queryBlockchainState
  )
import Seath.Test.Examples.Addition.Contract (initialSeathContract)
import Seath.Test.Examples.Addition.SeathSetup
  ( BlockhainState(BlockhainState)
  , Leader(Leader)
  , Participant(Participant)
  , getBlockhainState
  , getPublicKeyHash
  , logBlockchainState
  )
import Seath.Test.Examples.Addition.SeathSetup as SeathSetup
import Seath.Test.Examples.Addition.Types
  ( AdditionDatum(AdditionDatum)
  , AdditionState
  )
import Seath.Test.Examples.Utils (getTypedDatum)
import Seath.Test.TestSetup (RunnerConfig(RunnerConfig), runnerConfInfo)
import Seath.Types (SeathConfig(SeathConfig))

mainTest :: RunnerConfig AdditionState -> Contract Unit
mainTest config = do
  -- todo: check that parties participants have enough funds by config.minAdaRequired
  logInfo' $ "Running with " <> runnerConfInfo config

  let
    leaderKeyWallet = (unwrap config).seathLeader
    leader = Leader leaderKeyWallet
  vaildatorHash <- fixedValidatorHash
  leaderPublicKeyHash <- getPublicKeyHash leaderKeyWallet
  let
    participants = NE.toArray $ map Participant
      (unwrap config).seathParticipants
    seathConfig = SeathConfig
      { leader: leaderPublicKeyHash
      , stateVaildatorHash: vaildatorHash
      , actionHandler: handleAction
      , queryBlockchainState: queryBlockchainState
      }
    getState = getBlockhainState leader participants queryBlockchainState
    logState = getState >>= logBlockchainState

  exestingState <- hush <$> try getState

  unless (isJust exestingState) $ do
    logInfo' "No initialized state found - running initialization"
    -- contract initialization by some admin
    _ <- withKeyWallet (unwrap config).admin initialSeathContract
    logInfo' "Initialization - DONE"

  -- Seath round logic
  startState <- getState
  logBlockchainState startState -- state before Seath execution

  actions <- SeathSetup.genUserActions participants
  logInfo' $ "User actions: " <> show actions

  (finalizedTxsAndActions /\ _) <- withKeyWallet leaderKeyWallet $ buildChain
    seathConfig
    actions
    Nothing
  let finalizedTxs /\ _ = unzip finalizedTxsAndActions
  -- logInfo' $ "BuildChainResult: " <> show finalizedTxs
  txIds <- SeathSetup.submitChain leader participants finalizedTxs logState

  case last txIds of
    Nothing -> throwContractError
      "No IDs vere received after chain submission. Something is wrong."
    Just txId -> do
      awaitTxConfirmed txId
      endState <- getState
      logBlockchainState endState
      checkFinalState config startState endState

  logInfo' "end"

checkFinalState
  :: RunnerConfig AdditionState
  -> BlockhainState AdditionState
  -> BlockhainState AdditionState
  -> Contract Unit
checkFinalState
  (RunnerConfig config)
  (BlockhainState startState)
  (BlockhainState endState) = do

  checlLeaderUtxos
  checkScriptState

  where
  checlLeaderUtxos = do
    leaderUtxos <- maybe
      (throwContractError "Leader should have UTXOs at the end of test run")
      pure
      endState.leaderUTXOs
    when (size leaderUtxos /= 1) $ throwContractError
      "Leader should have only 1 UTXO at the end of test run"

  checkScriptState = do
    let (endUxos /\ _) = endState.sctiptState
    when (size endUxos /= 1) $ throwContractError
      "Script should have only 1 UTXO at the end of test run"

    (AdditionDatum endDatum) <-
      either throwContractError pure $
        ( note "scriptUtxos is empty!" (head $ values endUxos) >>=
            getTypedDatum
        )

    let (startUxos /\ _) = startState.sctiptState
    (AdditionDatum startDatum) <-
      either throwContractError pure $
        ( note "scriptUtxos is empty!" (head $ values startUxos) >>=
            getTypedDatum
        )
    let
      (currentAmount :: BigInt) = endDatum.lockedAmount
      (expectedAmount :: BigInt) = config.expectedStateChange
        (startDatum.lockedAmount)
    when (currentAmount /= expectedAmount)
      $ throwContractError
      $
        "Script should have " <> show expectedAmount
          <> " at the end of test run,  but has "
          <> show currentAmount

