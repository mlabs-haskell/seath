module Test.Examples.Addition.Demo.FullLeaderNode
  ( startNode
  ) where

import Contract.Prelude

import Contract.Monad (launchAff_, runContractInEnv)
import Contract.Test (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Control.Monad.Rec.Class (forever)
import Data.Posix.Signal (Signal(..))
import Data.Unit (Unit)
import Effect.Aff (delay)
import Node.Process (onSignal)
import Prelude (show)
import Seath.HTTP.SeathNode as SeathNode
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Utils as Seath
import Seath.Network.Types (RunContract(RunContract))
import Seath.Test.Examples.Addition.ContractUtils (buildAdditionCoreConfig) as Addition
import Seath.Test.Types (RunnerSetup)

startNode
  :: RunnerSetup -> Aff Unit
startNode setup = do

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

    leaderNodeConfig =
      Seath.mkLeaderConfig
        3000 -- timeout for building chain
        4 -- number of pending actions in queue
        3000 -- timeout for signatures awaiting
        coreConfig
        (mkRunner leader)

    userNodeConfig = Seath.mkUserConfig leaderUrl (mkRunner leader)
      (pure <<< Right)

    serverConf :: SeathServerConfig
    serverConf = { port: leaderPort }

  seathNode <- SeathNode.start serverConf leaderNodeConfig userNodeConfig

  liftEffect $ onSignal SIGINT $ launchAff_ do
    log "Shutting down Seath leader node"
    SeathNode.stop seathNode
    log "server - done"

  waitFrever

waitFrever :: Aff Unit
waitFrever = forever $ delay (wrap 3000000.0)