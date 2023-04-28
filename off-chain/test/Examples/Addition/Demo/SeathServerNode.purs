module Test.Examples.Addition.Demo.SeathServerNode
  ( startLeaderSeathNode
  ) where

import Contract.Prelude

import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Contract.Test (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Data.BigInt (BigInt)
import Data.Unit (Unit)
import Effect.Aff (delay)
import Prelude (show)
import Seath.Core.Types (CoreConfiguration(CoreConfiguration))
import Seath.HTTP.SeathNode as SeathNode
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Utils (mkLeaderConfig, mkUserConfig)
import Seath.Network.Types (RunContract(RunContract))
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.ContractUtils as Addition
import Seath.Test.Examples.Addition.Types
  ( AdditionAction
  , AdditionDatum
  , AdditionRedeemer
  , AdditionValidator
  )

startLeaderSeathNode
  :: ContractEnv -> KeyWallet -> KeyWallet -> Array KeyWallet -> Aff Unit
startLeaderSeathNode env _admin leader _users = do
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
  delay (wrap 300000.0)
  log "server done"
