module Test.Examples.Addition.AutomatedEndToEndTest
  ( mainTest
  ) where

import Contract.Prelude

import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Natural as Natural
import Contract.Test (withKeyWallet)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe, try)
import Data.BigInt (BigInt)
import Data.Map as Map
import Data.Unit (Unit)
import Effect.Aff (delay, error, forkAff, killFiber)
import Prelude (show)
import Seath.Core.Types (CoreConfiguration(CoreConfiguration))
import Seath.HTTP.SeathNode as SeathNode
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Utils as Seath
import Seath.Network.Types (RunContract(RunContract))
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.Actions (queryBlockchainState) as Addition.Actions
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.ContractUtils (initialSeathContract) as Addition.Contract
import Seath.Test.Examples.Addition.Types
  ( AdditionAction
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

  coreConfig <- runContractInEnv env $ withKeyWallet leader $
    buildAdditionCoreConfig
  let

    leaderPort = 3000
    leaderUrl = "http://localhost:" <> show leaderPort

    mkRunner :: KeyWallet -> RunContract
    mkRunner kw = RunContract (\c -> runContractInEnv env $ withKeyWallet kw c)

    leaderConfig =
      Seath.mkLeaderConfig
        3000 -- timeout before launch chain building
        4 -- numberof actions in queue to trigger chain building
        3000 -- time to wait for signatures before submitting chain of transactions
        coreConfig -- core configuration gfor chain builder
        (mkRunner leader) -- function to execute CTL `Contract`s

    userConfig = Seath.mkUserConfig leaderUrl (mkRunner leader) (pure <<< Right)

    serverConf :: SeathServerConfig
    serverConf = { port: leaderPort }

  checkInitSctipt env admin { waitingTime: 3, maxAttempts: 10 }

  seathNode <- SeathNode.start serverConf leaderConfig userConfig

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
    , stateValidatorHash: vaildatorHash
    , actionHandler: Addition.handleAction
    , queryBlockchainState: Addition.queryBlockchainState
    }
