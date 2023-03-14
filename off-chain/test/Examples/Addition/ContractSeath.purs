module Seath.Test.Examples.Addition.ContractSeath (mainTest) where

import Contract.Log (logInfo')
import Contract.Monad (Aff, Contract, throwContractError)
import Contract.Prelude (when, (/=), (>>=))
import Contract.Scripts (ValidatorHash)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (withKeyWallet)
import Control.Monad (bind)
import Data.Array (last, length, replicate, unzip)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Functor (map)
import Data.List ((!!))
import Data.Map (size, values)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
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
import Seath.Test.Examples.Addition.Types (AdditionDatum(..))
import Seath.Test.Examples.Utils (getTypedDatum)
import Seath.Types (SeathConfig(SeathConfig))

mainTest :: PlutipConfig -> Aff Unit
mainTest config = runPlutipContract config distribution $
  \((admin /\ leader') /\ participants') -> do
    -- contract initialization by some admin
    _ <- withKeyWallet admin initialSeathContract

    logInfo' "----------------------- INIT DONE -------------------------"

    -- Seath round logic
    vaildatorHash <- fixedValidatorHash
    let leader = Leader leader'
    leaderPublicKeyHash <- getPublicKeyHash leader'
    logInfo' $ "@@ participants: " <> show (length participants')
    let
      participants = map Participant participants'
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

    (finalizedTxsAndActions /\ _) <- withKeyWallet leader' $ buildChain
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
        checkFinalState leader participants vaildatorHash

    logInfo' "end"
  where

  distribution
    :: (Array BigInt /\ Array BigInt) /\ (Array (Array BigInt))
  distribution =
    ([ BigInt.fromInt 1_000_000_000 ] /\ [ BigInt.fromInt 1_000_000_000 ]) /\
      replicate 4 [ BigInt.fromInt 1_000_000_000 ]

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

