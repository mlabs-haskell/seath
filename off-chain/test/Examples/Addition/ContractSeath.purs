module Seath.Test.Examples.Addition.ContractSeath (mainTest) where


import Contract.Log (logInfo')
import Contract.Monad (Contract, throwContractError)
import Contract.Prelude (Maybe(..), map, maybe, when, (/=), (>>=))
import Contract.Scripts (ValidatorHash)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (withKeyWallet)
import Control.Monad (bind)
import Data.Array (last, replicate, unzip)
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left))
import Data.List ((!!))
import Data.Map (size, values)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Prelude (discard, pure, ($))
import Seath.HandleActions (actions2TransactionsChain)
import Seath.Test.Examples.Addition.Actions (fixedValidatorHash, getScriptUtxosFromChain, handleAction)
import Seath.Test.Examples.Addition.Contract (initialSeathContract)
import Seath.Test.Examples.Addition.SeathSetup (BlockhainState(BlockhainState), Leader(Leader), Participant(Participant), getBlockhainState, getPublicKeyHash, logBlockchainState)
import Seath.Test.Examples.Addition.SeathSetup as SeathSetup
import Seath.Test.Examples.Addition.Types (AdditionDatum(..))
import Seath.Test.Examples.Utils (getTypedDatum)
import Seath.Test.TestSetup (RunnerConfig(RunnerConfig), runnerConfInfo)
import Seath.Types (ChainBuilderState(ChainBuilderState), SeathConfig(SeathConfig))

mainTest :: RunnerConfig -> Contract Unit
mainTest (RunnerConfig config) = do
  -- contract initialization by some admin
  logInfo' $ "Running with " <> runnerConfInfo (RunnerConfig config)
  firstState <- withKeyWallet (config.admin) initialSeathContract

  logInfo' "----------------------- INIT DONE -------------------------"

  -- Seath round logic
  vaildatorHash <- fixedValidatorHash
  let
    leaderKeyWallet = config.seathLeader
    leader = Leader leaderKeyWallet
  leaderPublicKeyHash <- getPublicKeyHash leaderKeyWallet
  let
    -- TODO: maybe use non-empty for participants everywhere
    participants = NE.toArray $ map Participant (config.seathParticipants)
    seathConfig = SeathConfig
      { leader: leaderPublicKeyHash
      , stateVaildatorHash: vaildatorHash
      , chainStartStateUtxos: getScriptUtxosFromChain
      , actionHandler: handleAction
      }
    logState = logBlockchainState leader participants vaildatorHash

  logState -- log state after initialisation

  actions <- SeathSetup.genUserActions participants
  logInfo' $ "User actions: " <> show actions

  let
    firstBuilderState = ChainBuilderState
      { finalizedTransactions: []
      , lastResult: Left firstState
      , pendingActions: actions
      }
    buildChain = actions2TransactionsChain seathConfig firstBuilderState

  (finalizedTxsAndActions /\ _) <- withKeyWallet leaderKeyWallet buildChain
  let finalizedTxs /\ _ = unzip finalizedTxsAndActions
  -- logInfo' $ "BuildChainResult: " <> show finalizedTxs
  txIds <- SeathSetup.submitChain leader participants finalizedTxs logState

  case last txIds of
    Nothing -> throwContractError
      "No IDs vere received after chain submission. Something is wrong."
    Just txId -> do
      awaitTxConfirmed txId
      logState
      checkFinalState leader participants vaildatorHash

  logInfo' "end"
  where

  distribution
    :: (Array BigInt /\ Array BigInt) /\ (Array (Array BigInt))
  distribution =
    ([ BigInt.fromInt 1_000_000_000 ] /\ [ BigInt.fromInt 1_000_000_000 ]) /\
      replicate 50 [ BigInt.fromInt 1_000_000_000 ]

checkFinalState :: Leader -> Array Participant -> ValidatorHash -> Contract Unit
checkFinalState leader participants vaildatorHash = do
  (BlockhainState bchState) <- getBlockhainState leader participants
    vaildatorHash

  checlLeaderUtxos bchState
  checkScriptState bchState

  where
  checlLeaderUtxos bchState = do
    leaderUtxos <- maybe
      (throwContractError "Leader should have UTXOs at the end of test run")
      pure
      bchState.leaderUTXOs
    when (size leaderUtxos /= 1) $ throwContractError
      "Leader should have only 1 UTXO at the end of test run"

  checkScriptState bchState = do
    let scriptUxos = bchState.sctiptUTXOs
    when (size scriptUxos /= 1) $ throwContractError
      "Script should have only 1 UTXO at the end of test run"
    let
      (scriptDatum :: Maybe AdditionDatum) =
        ((values scriptUxos) !! 0) >>= getTypedDatum
      expectedDatum = Just $ AdditionDatum { lockedAmount: BigInt.fromInt 5100 }
    when (scriptDatum /= expectedDatum)
      $ throwContractError
      $
        "Script should have " <> show expectedDatum
          <> " at the end of test run,  but has "
          <> show scriptDatum

