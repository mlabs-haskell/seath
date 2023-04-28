module Test.Examples.Addition.Demo.SeathUsers
  ( startScenario
  ) where

import Contract.Prelude

import Aeson (class EncodeAeson)
import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv, launchAff_, runContractInEnv)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Natural as Natural
import Contract.Test (withKeyWallet)
import Contract.Transaction (Transaction)
import Contract.Utxos (getWalletUtxos)
import Control.Monad.Error.Class (liftMaybe, try)
import Ctl.Internal.Wallet.Key (KeyWallet)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Posix.Signal (Signal(..))
import Data.Unit (Unit)
import Effect.Aff (Fiber, delay, error, killFiber)
import Node.Process (onSignal)
import Prelude (show)
import Seath.HTTP.Utils (mkUserConfig)
import Seath.Network.Types (RunContract(RunContract), UserNode)
import Seath.Network.Users as Users
import Seath.Network.Utils (readResults)
import Seath.Test.Examples.Addition.Actions (queryBlockchainState) as Addition.Actions
import Seath.Test.Examples.Addition.ContractUtils (initialSeathContract) as Addition.Contract
import Seath.Test.Examples.Addition.Types (AdditionAction(AddAmount))
import Seath.Test.Types (RunnerSetup)

startScenario
  :: RunnerSetup -> Aff Unit
startScenario setup = do
  let

    leaderPort = 3000
    leaderUrl = "http://localhost:" <> show leaderPort

    mkRunner :: KeyWallet -> RunContract
    mkRunner kw = RunContract
      (\c -> runContractInEnv setup.contractEnv $ withKeyWallet kw c)

  checkInitSctipt setup.contractEnv setup.adminWallet
    { waitingTime: 3, maxAttempts: 10 }

  let
    refuser = 2

    ixName :: Int -> String
    ixName i = "User-" <> show i
    zipFun user ix = do
      (fiber /\ (node :: UserNode AdditionAction)) <-
        mkUser
          leaderUrl
          mkRunner
          ( if ix == refuser then (\_ -> pure $ Left "refuse to sign")
            else (pure <<< Right)
          )
          user
      pure (ix /\ fiber /\ node)

  namedNodes <- Array.zipWithA zipFun setup.userWallets
    (Array.range 1 (length setup.userWallets))

  log $ "Starting with num of users " <> show (Array.length namedNodes)

  for_ namedNodes $ \(ix /\ _ /\ node) -> do
    log $ ixName ix <> ": preform include action request"
    Users.performAction node (AddAmount $ BigInt.fromInt ix)

  liftEffect $ onSignal SIGINT $ launchAff_ do
    for_ namedNodes $ \(ix /\ fiber /\ node) -> do
      log $ ixName ix <> " results"
      readResults node >>= log <<< show
      killFiber (error "can't cleanup user") fiber

  delay (wrap 3000000.0)
  log "Users end"

mkUser
  :: forall a
   . Show a
  => EncodeAeson a
  => String
  -> (KeyWallet -> RunContract)
  -> (Transaction -> Aff (Either String Transaction))
  -> KeyWallet
  -> Aff (Fiber Unit /\ UserNode a)
mkUser leaderUrl mkRunner txCheck kw = Users.startUserNode
  ( mkUserConfig leaderUrl (mkRunner kw)
      txCheck
  )

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
