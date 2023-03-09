module Seath.Test.Examples.Addition.ContractSeath (mainTest) where

import Contract.Log (logInfo')
import Contract.Monad (Aff, Contract, throwContractError)
import Contract.PlutusData (Datum(..), fromData, toData)
import Contract.Prelude (Maybe(..), map, maybe, when, (/=), (>>=))
import Contract.Scripts (ValidatorHash)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Contract.Transaction (TransactionOutputWithRefScript, outputDatumDatum)
import Contract.Wallet (withKeyWallet)
import Control.Monad (bind)
import Ctl.Internal.Plutus.Types.Transaction (_datum, _output)
import Data.Array (unzip)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left))
import Data.Lens ((^.))
import Data.List ((!!))
import Data.Map (size, values)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Prelude (discard, pure, ($))
import Seath.HandleActions (actions2TransactionsChain)
import Seath.Test.Examples.Addition.Actions
  ( fixedValidatorHash
  , handleActionFromBlockChain
  , handleActionFromFinalizedTransaction
  )
import Seath.Test.Examples.Addition.Contract (initialContract)
import Seath.Test.Examples.Addition.SeathSetup
  ( BlockhainState(..)
  , Leader(Leader)
  , Participant(Participant)
  , getBlockhainState
  , getPublicKeyHash
  , logBlockchainState
  )
import Seath.Test.Examples.Addition.SeathSetup as SeathSetup
import Seath.Test.Examples.Addition.Types (AdditionDatum(..))
import Seath.Types
  ( ChainBuilderState(ChainBuilderState)
  , SeathConfig(SeathConfig)
  )

mainTest :: PlutipConfig -> Aff Unit
mainTest config = runPlutipContract config distribution $ \(a /\ b /\ c) ->
  do
    leaderPublicKeyHash <- getPublicKeyHash a
    vaildatorHash <- fixedValidatorHash
    let
      leader = Leader a
      participants = map Participant [ b, c ]
      seathConfig = SeathConfig
        { leader: leaderPublicKeyHash
        , finalizedTxHandler: handleActionFromFinalizedTransaction
        , onchainHandler: handleActionFromBlockChain
        }
      logState = logBlockchainState leader participants vaildatorHash
    logState
    firstTransactionId /\ _ <- withKeyWallet a initialContract
    logState
    actions <- SeathSetup.genUserActions participants
    logInfo' $ "test " <> show actions
    let
      firstBuilderState = ChainBuilderState
        { finalizedTransactions: []
        , lastResult: Left firstTransactionId
        , pendingActions: actions
        }
      buildChain =
        actions2TransactionsChain
          seathConfig
          firstBuilderState
    (finalizedTxsAndActions /\ _) <- withKeyWallet a buildChain
    let finalizedTxs /\ _ = unzip finalizedTxsAndActions
    -- logInfo' $ "BuildChainResult: " <> show finalizedTxs
    _ <- SeathSetup.submitChain leader participants finalizedTxs $
      logState
    logState
    _ <- checkFinalState leader participants vaildatorHash
    logInfo' "end"
  where

  distribution :: Array BigInt /\ Array BigInt /\ Array BigInt
  distribution =
    [ BigInt.fromInt 1_000_000_000 ]
      /\ [ BigInt.fromInt 1_000_000_000 ]
      /\ [ BigInt.fromInt 1_000_000_000 ]

checkFinalState :: Leader -> Array Participant -> ValidatorHash -> Contract Unit
checkFinalState leader participants vaildatorHash = do
  (BlockhainState bchState) <- getBlockhainState leader participants
    vaildatorHash

  checkScriptUtxos bchState
  checlLeaderUtxos bchState
  checkSctipDatum bchState

  where
  checlLeaderUtxos bchState = do
    leaderUtxos <- maybe
      (throwContractError "Leader should have UTXOs at the end of test run")
      pure
      bchState.leaderUTXOs
    when (size leaderUtxos /= 1) $ throwContractError
      "Leader should have only 1 UTXO at the end of test run"

  checkScriptUtxos bchState = do
    let scriptUxos = bchState.sctiptUTXOs
    when (size scriptUxos /= 1) $ throwContractError
      "Script should have only 1 UTXO at the end of test run"

  checkSctipDatum bchState = do
    let scriptUxos = bchState.sctiptUTXOs
    let
      (scriptDatum :: Maybe AdditionDatum) = ((values scriptUxos) !! 0) >>=
        getAdditionDatum
      expectedDatum = Just $ AdditionDatum { lockedAmount: BigInt.fromInt 1200 }
    when (scriptDatum /= expectedDatum)
      $ throwContractError
      $
        "Script should have " <> show scriptDatum
          <> " at the end of test run,  but has "
          <> show expectedDatum

getAdditionDatum :: TransactionOutputWithRefScript -> Maybe AdditionDatum
getAdditionDatum out =
  let
    datum = outputDatumDatum (out ^. _output ^. _datum)
  in
    case datum of
      (Just (Datum d)) -> fromData $ toData d
      Nothing -> Nothing