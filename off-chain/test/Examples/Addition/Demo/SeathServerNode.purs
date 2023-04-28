module Test.Examples.Addition.Demo.SeathServerNode
  ( startLeaderSeathNode
  ) where

import Contract.Prelude

import Contract.Monad (launchAff_, runContractInEnv)
import Contract.Test (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Data.Posix.Signal (Signal(..))
import Data.Unit (Unit)
import Effect.Aff (delay)
import Node.Process (onSignal)
import Prelude (show)
import Seath.HTTP.SeathNode as SeathNode
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Utils (mkLeaderConfig, mkUserConfig)
import Seath.Network.Types (RunContract(RunContract))
import Seath.Test.Examples.Addition.ContractUtils (buildAdditionCoreConfig) as Addition
import Seath.Test.Types (RunnerSetup)

startLeaderSeathNode
  :: RunnerSetup -> Aff Unit
startLeaderSeathNode setup = do

  let
    env = setup.contractEnv
    leader = setup.leaderWallet

  coreConfig <- runContractInEnv env $ withKeyWallet leader $
    Addition.buildAdditionCoreConfig
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

  let
    serverConf :: SeathServerConfig
    serverConf = { port: leaderPort }

  seathNode <-
    SeathNode.start
      serverConf
      testLeaderConfig
      (mkUserConfig leaderUrl (mkRunner leader) (pure <<< Right))

  liftEffect $ onSignal SIGINT $ launchAff_ do
    SeathNode.stop seathNode

  delay (wrap 3000000.0)
  log "server done"
