module Seath.Test.Examples.Addition.ContractSeath (mainTest, newMainTest) where

import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM, throwContractError)
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
import Control.Applicative (class Applicative, (*>))
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Logger.Class (info)
import Ctl.Internal.Types.Natural (Natural)
import Data.Array (last, length, replicate, unzip, zip)
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Data.Either (note)
import Data.List (head)
import Data.Map (size, values)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (mempty, (<>))
import Data.Show (show)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit, unit)
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Prelude (discard, pure, ($))
import Seath.Core.ChainBuilder (buildChain)
import Seath.Core.Types (CoreConfiguration(CoreConfiguration), UserAction)
import Seath.Network.Leader
  ( getNextBatchOfActions
  , startLeaderServer
  , waitForChainSignatures
  )
import Seath.Network.OrderedMap (OrderedMap)
import Seath.Network.OrderedMap as OrderedMap
import Seath.Network.Types (UserNode)
import Seath.Network.Users (makeUserAction, sendActionToLeader, startUserServer)
import Seath.Network.Utils (getPublicKeyHash)
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
  , Participant(Participant)
  , RunnerConfiguration(RunnerConfiguration)
  )
import Seath.Test.Utils (runnerConfInfo)
import Type.Function (type ($))
import Undefined (undefined)

newMainTest :: RunnerConfiguration AdditionState AdditionAction -> Contract Unit
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

  actions <- makeUserActionsFromActions (zip participants plainActions)
  logInfo' $ "User actions: " <> show actions

  _ <- liftAff $ startLeaderServer (unwrap leader).node
  _ <- liftAff $ traverse (\x -> startUserServer (unwrap x).node) participants

  sendedActions <- liftAff $ performFromParticipantsWithValue
    sendActionToLeader
    (zip participants actions)
  recivedActions <- liftAff $ getNextBatchOfActions (unwrap leader).node
    OrderedMap.empty

  -- TODO: chainBuilder must return the chain and the possible errors processing
  -- actions, so we can notify the user here (and right now a exception balancing
  -- would break the process).
  (finalizedTxsAndActions /\ _) <- withKeyWallet (unwrap leader).wallet $
    buildChain
      coreConfiguration
      -- TODO : fixme
      (undefined recivedActions)
      Nothing
  pure unit

-- signatureRequest <- liftAff $ sendChainToUsersForSignature
--   (unwrap leader).node
--   -- TODO :fixme
--   (undefined finalizedTxsAndActions)
-- let
--   { success: sended, failures: failToSend } = splitSuccessFails
--     signatureRequest

-- _ <- liftEffect $ unless (isEmpty failToSend) $ throw
--   ( "some transactions failed to be send for signature: " <> undefined
--       failToSend
--   )

-- maybeSignedTxs <- liftAff $ waitForChainSignatures (unwrap leader).node sended

-- let
--   { success: toSubmit, failures: toRebuild } = splitSuccessFails
--     maybeSignedTxs

-- liftEffect $ unless (isEmpty toRebuild) $ throw
--   ("failed to be signed: " <> undefined toRebuild)

-- txIds <- SeathSetup.submitChain leader participants (undefined toSubmit)
--   logState

-- case last txIds of
--   Nothing -> throwContractError
--     "No IDs were received after chain submission. Something is wrong."
--   Just txId -> do
--     awaitTxConfirmed txId
--     endState <- getState
--     logBlockchainState endState
--     checkFinalState config startState endState

-- logInfo' "end"

-- where
-- isEmpty :: forall a b. OrderedMap a b -> Boolean
-- isEmpty = undefined

performFromParticipantsWithValue
  :: forall a b (m :: Type -> Type) actionType
   . Applicative m
  => (UserNode actionType -> a -> m b)
  -> Array (Participant actionType /\ a)
  -> m $ Array b
performFromParticipantsWithValue function = traverse makeOne
  where
  makeOne :: Participant actionType /\ a -> m b
  makeOne ((Participant participant) /\ value) = function (participant.node)
    value

-- startUserServers :: Array Participant -> Contract Unit
-- startUserServer = traverse startOne
--   where
--     startOne (Participant participant) = startUserServer participant.node

makeUserActionsFromActions
  :: forall actionType
   . Array (Participant actionType /\ actionType)
  -> Contract $ Array $ UserAction actionType
makeUserActionsFromActions = traverse makeOne
  where
  makeOne
    :: Participant actionType /\ actionType -> Contract $ UserAction actionType
  makeOne ((Participant participant) /\ action) = withKeyWallet
    participant.wallet
    do
      userUTxOs <- liftedM "no UTXOs found" getWalletUtxos
      pure $ makeUserAction participant.node action userUTxOs

mainTest :: RunnerConfiguration AdditionState AdditionAction -> Contract Unit
mainTest config = do
  -- todo: check that parties participants have enough funds by config.minAdaRequired
  info Map.empty "This is a test for logger"
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
  :: RunnerConfiguration BigInt AdditionAction
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
  :: RunnerConfiguration AdditionState AdditionAction
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

