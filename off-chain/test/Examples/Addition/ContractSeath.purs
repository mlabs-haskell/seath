module Seath.Test.Examples.Addition.ContractSeath (mainTest) where

import Contract.Log (logInfo')
import Contract.Monad (Contract, throwContractError)
import Contract.Prelude (unless, unwrap, when, (/=), (>>=))
import Contract.Scripts (ValidatorHash)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (withKeyWallet)
import Control.Monad (bind)
import Data.Array (last, unzip)
import Data.Array.NonEmpty as NE
import Data.Either (Either, note)
import Data.Functor (map)
import Data.List (head)
import Data.Map (size, values)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Data.Unit (Unit)
import Prelude (discard, pure, ($))
import Seath.Core.HandleActions (buildChain)
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
import Seath.Test.Examples.Addition.Types (AdditionDatum)
import Seath.Test.Examples.Utils (getTypedDatum)
import Seath.Test.TestSetup (RunnerConfig(RunnerConfig), runnerConfInfo)
import Seath.Core.Types (SeathConfig(SeathConfig))

mainTest :: RunnerConfig AdditionDatum -> Contract Unit
mainTest config = do
  -- todo: check that parties participants have enough funds by config.minAdaRequired
  logInfo' $ "Running with " <> runnerConfInfo config

  let
    leaderKeyWallet = (unwrap config).seathLeader
    leader = Leader leaderKeyWallet

  unless ((unwrap config).alreadyInitialized) $ do
    -- contract initialization by some admin
    _ <- withKeyWallet (unwrap config).admin initialSeathContract
    logInfo' "----------------------- INIT DONE -------------------------"

  -- Seath round logic
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
    logState = logBlockchainState leader participants vaildatorHash

  logState -- log state after initialisation

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
      logState
      checkFinalState config leader participants vaildatorHash

  logInfo' "end"

checkFinalState
  :: RunnerConfig AdditionDatum
  -> Leader
  -> Array Participant
  -> ValidatorHash
  -> Contract Unit
checkFinalState (RunnerConfig config) leader participants vaildatorHash = do
  (BlockhainState blockchainState) <- getBlockhainState leader participants
    vaildatorHash

  checlLeaderUtxos blockchainState
  checkScriptState blockchainState

  where
  checlLeaderUtxos blockchainState = do
    leaderUtxos <- maybe
      (throwContractError "Leader should have UTXOs at the end of test run")
      pure
      blockchainState.leaderUTXOs
    when (size leaderUtxos /= 1) $ throwContractError
      "Leader should have only 1 UTXO at the end of test run"

  checkScriptState blockchainState = do
    let scriptUxos = blockchainState.sctiptUTXOs
    when (size scriptUxos /= 1) $ throwContractError
      "Script should have only 1 UTXO at the end of test run"
    let
      (scriptDatum :: Either String AdditionDatum) =
        note "scriptUtxos is empty!" (head $ values scriptUxos) >>=
          getTypedDatum
      expectedDatum = pure $ config.expectedFinalState
    when (scriptDatum /= expectedDatum)
      $ throwContractError
      $
        "Script should have " <> show expectedDatum
          <> " at the end of test run,  but has "
          <> show scriptDatum

