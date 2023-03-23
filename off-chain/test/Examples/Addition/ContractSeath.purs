module Seath.Test.Examples.Addition.ContractSeath (mainTest, newMainTest) where

import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, throwContractError)
import Contract.Numeric.Natural (fromInt)
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
import Contract.Wallet (getWalletUtxos, withKeyWallet)
import Control.Applicative ((*>))
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader (runReaderT)
import Ctl.Internal.Types.Natural (Natural)
import Data.Array (last, length, range, replicate, unzip)
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Data.Either (note)
import Data.List (head)
import Data.Map (size, values)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Unit (Unit, unit)
import Effect.Aff (error)
import Prelude (discard, pure, ($))
import Seath.Core.ChainBuilder (buildChain)
import Seath.Core.Types (CoreConfiguration(CoreConfiguration))
import Seath.Network.Users (makeUserAction)
import Seath.Network.Utils (getPublicKeyHash, seath2Contract)
import Seath.Test.Examples.Addition.Actions
  ( fixedValidatorHash
  , handleAction
  , queryBlockchainState
  )
import Seath.Test.Examples.Addition.Contract (initialSeathContract)
import Seath.Test.Examples.Addition.SeathSetup
  ( getBlockchainState
  , logBlockchainState
  , stateChangePerAction
  )
import Seath.Test.Examples.Addition.SeathSetup as SeathSetup
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum(AdditionDatum)
  , AdditionRedeemer
  , AdditionState
  , AdditionValidator
  )
import Seath.Test.Examples.Utils (getTypedDatum)
import Seath.Test.Types
  ( BlockchainState(BlockchainState)
  , RunnerConfiguration(RunnerConfiguration)
  )
import Seath.Test.Utils (runnerConfInfo)
import Type.Function (type ($))

newMainTest :: RunnerConfiguration AdditionState -> Contract Unit
newMainTest config = do
  -- todo: check that parties participants have enough funds by config.minAdaRequired
  logInfo' $ "Running with " <> runnerConfInfo config

  let
    leader = (unwrap config).leader
    participants = NE.toArray $ (unwrap config).participants
    participantsNumber = length participants
    plainActions = replicate participantsNumber (AddAmount stateChangePerAction)
    getState = getBlockchainState leader participants queryBlockchainState
    logState = getState >>= logBlockchainState

  startState <- withKeyWallet (unwrap config).admin $ ensureInitialization 10
    getState
  logBlockchainState startState -- state before Seath execution

  coreConfiguration <- runnerConfiguration2CoreConfiguration config

  actions <- seath2Contract $ traverse makeUserAction plainActions
  logInfo' $ "User actions: " <> show actions

  (finalizedTxsAndActions /\ _) <- withKeyWallet (unwrap leader).wallet $
    buildChain
      coreConfiguration
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

mainTest :: RunnerConfiguration AdditionState -> Contract Unit
mainTest config = do
  -- todo: check that parties participants have enough funds by config.minAdaRequired
  logInfo' $ "Running with " <> runnerConfInfo config

  let
    leader = (unwrap config).leader
    participants = NE.toArray $ (unwrap config).participants
    getState = getBlockchainState leader participants queryBlockchainState
    logState = getState >>= logBlockchainState

  startState <- withKeyWallet (unwrap config).admin $ ensureInitialization 10
    getState
  logBlockchainState startState -- state before Seath execution

  coreConfiguration <- runnerConfiguration2CoreConfiguration config

  actions <- SeathSetup.genUserActions participants
  logInfo' $ "User actions: " <> show actions

  (finalizedTxsAndActions /\ _) <- withKeyWallet (unwrap leader).wallet $
    buildChain
      coreConfiguration
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

-- TODO : Add a timeout to avoid waiting forever in testnet
ensureInitialization
  :: forall a
   . Int
  -> Contract $ BlockchainState a
  -> Contract $ BlockchainState a
ensureInitialization waitingTime getState = do
  _ <- waitForFunding waitingTime

  existingState <- hush <$> try getState

  unless (isJust existingState) $ do
    logInfo' "No initialized state found - running initialization"
    -- contract initialization by some admin
    _ <- initialSeathContract
    logInfo' "Initialization - DONE"
  getState

waitForFunding :: Int -> Contract Unit
waitForFunding waitingTime = do
  slotsTime <-
    liftMaybe (error $ "can't convert waiting time: " <> show waitingTime) $
      fromInt waitingTime
  _ <- waitNSlots slotsTime
  loop slotsTime
  where
  loop :: Natural -> Contract Unit
  loop slots = do
    logInfo' "Waiting for funds in wallet"
    mutxos <- getWalletUtxos
    case mutxos of
      Just utxos ->
        if Map.isEmpty utxos then
          waitNSlots slots *> loop slots
        else pure unit
      Nothing -> waitNSlots slots *> loop slots

runnerConfiguration2CoreConfiguration
  :: RunnerConfiguration BigInt
  -> Contract $ CoreConfiguration AdditionAction AdditionState AdditionValidator
       AdditionDatum
       AdditionRedeemer
runnerConfiguration2CoreConfiguration config = do
  let
    leader = (unwrap config).leader
  vaildatorHash <- fixedValidatorHash
  leaderPublicKeyHash <- withKeyWallet (unwrap leader).wallet getPublicKeyHash
  pure $ CoreConfiguration
    { leader: leaderPublicKeyHash
    , stateVaildatorHash: vaildatorHash
    , actionHandler: handleAction
    , queryBlockchainState: queryBlockchainState
    , numberOfBuiltChains: 0
    }

checkFinalState
  :: RunnerConfiguration AdditionState
  -> BlockchainState AdditionState
  -> BlockchainState AdditionState
  -> Contract Unit
checkFinalState
  (RunnerConfiguration config)
  (BlockchainState startState)
  (BlockchainState endState) = do

  checkLeaderUtxos
  checkScriptState

  where
  checkLeaderUtxos = do
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

