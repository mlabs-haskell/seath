module Test.Examples.Addition.AutomatedEndToEndTest
  ( mainTest
  ) where

import Contract.Prelude

import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv, launchAff_, runContractInEnv)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Natural as Natural
import Contract.Test (withKeyWallet)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe, try)
import Data.Array ((!!))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Posix.Signal (Signal(..))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Unit (Unit)
import Effect.Aff (delay, error, forkAff, joinFiber, killFiber, launchAff)
import Node.Process (onSignal)
import Prelude (show)
import Seath.Core.Types (CoreConfiguration(CoreConfiguration))
import Seath.HTTP.SeathNode as SeathNode
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Utils (mkLeaderConfig, mkUserConfig)
import Seath.Network.Types (RunContract(RunContract), UserNode)
import Seath.Network.Users as Users
import Seath.Network.Utils (getPublicKeyHash, readResults)
import Seath.Test.Examples.Addition.Actions (queryBlockchainState) as Addition.Actions
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.ContractUtils (initialSeathContract) as Addition.Contract
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum
  , AdditionRedeemer
  , AdditionValidator
  )
import Seath.Test.Types (RunnerSetup)
import Test.Examples.Addition.Demo.SeathUsers as DemoUsers

mainTest :: RunnerSetup -> Aff Unit
mainTest setup = do
  let
    env = setup.contractEnv
    admin = setup.adminWallet
    leader = setup.leaderWallet
    users = setup.userWallets

  coreConfig <- runContractInEnv env $ withKeyWallet leader $
    buildAdditionCoreConfig
  let

    leaderPort = 3000
    leaderUrl = "http://localhost:" <> show leaderPort

    mkRunner :: KeyWallet -> RunContract
    mkRunner kw = RunContract (\c -> runContractInEnv env $ withKeyWallet kw c)

    testLeaderConfig =
      mkLeaderConfig
        3000
        4
        3000
        coreConfig
        (mkRunner leader)

    serverConf :: SeathServerConfig
    serverConf = { port: leaderPort }

  checkInitSctipt env admin { waitingTime: 3, maxAttempts: 10 }

  seathNode <-
    SeathNode.start
      serverConf
      testLeaderConfig
      (mkUserConfig leaderUrl (mkRunner leader) (pure <<< Right))

  usersFiber <- forkAff $ DemoUsers.startScenario setup
  delay (wrap 30000.0)
  killFiber (error "Test done - stoppping fibers") usersFiber
  log "Stopping leader  node"
  SeathNode.stop seathNode
  log "Leader  node stopped"
  log "Seath network test end"

checkInitSctipt
  :: ContractEnv
  -> KeyWallet
  -> { waitingTime :: Int, maxAttempts :: Int }
  -> Aff Unit
checkInitSctipt env admin waitingTime = runContractInEnv env
  $ withKeyWallet admin
  $ do
      waitForFunding waitingTime
      scriptState <- try $ Addition.Actions.queryBlockchainState
      case scriptState of
        Left _ -> do
          logInfo' "Initializing Addition script state"
          void $ Addition.Contract.initialSeathContract
        Right _scriptState -> do
          logInfo' $ "Addition script state already initialized"
      currentState <- Addition.Actions.queryBlockchainState
      logInfo' $ "Current script state:\n" <> show currentState

waitForFunding :: { waitingTime :: Int, maxAttempts :: Int } -> Contract Unit
waitForFunding options = do
  slotsTime <-
    liftMaybe
      (error $ "Can't convert waiting time: " <> show options.waitingTime) $
      Natural.fromInt options.waitingTime
  _ <- waitNSlots slotsTime
  loop slotsTime options.maxAttempts
  where
  loop :: Natural -> Int -> Contract Unit
  loop slots remainingAttempts =
    if remainingAttempts == 0 then pure unit
    else do
      logInfo' "Waiting for funds in wallet"
      mutxos <- getWalletUtxos
      case mutxos of
        Just utxos ->
          if Map.isEmpty utxos then
            waitNSlots slots *> loop slots (remainingAttempts - 1)
          else pure unit
        Nothing -> waitNSlots slots *> loop slots (remainingAttempts - 1)

buildAdditionCoreConfig
  ∷ Contract
      ( CoreConfiguration
          AdditionAction
          BigInt
          AdditionValidator
          AdditionDatum
          AdditionRedeemer
      )
buildAdditionCoreConfig = do
  vaildatorHash <- Addition.fixedValidatorHash
  leaderPkh <- getPublicKeyHash
  pure $ CoreConfiguration
    { leader: leaderPkh
    , stateVaildatorHash: vaildatorHash
    , actionHandler: Addition.handleAction
    , queryBlockchainState: Addition.queryBlockchainState
    }
