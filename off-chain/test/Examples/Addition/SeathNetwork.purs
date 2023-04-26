module Test.Examples.Addition.SeathNetwork
  ( mainTest
  ) where

import Contract.Prelude

import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Natural as Natural
import Contract.Test (withKeyWallet)
import Contract.Transaction (FinalizedTransaction, Transaction)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe, try)
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Unit (Unit)
import Effect.Aff (delay, error, forkAff, killFiber)
import Payload.Server as Payload.Server
import Prelude (show)
import Seath.Core.ChainBuilder as ChainBuilder
import Seath.Core.Types (CoreConfiguration(CoreConfiguration), UserAction)
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.UserHandlers as HttpUser
import Seath.Network.Leader as Leader
import Seath.Network.Types
  ( LeaderConfiguration(LeaderConfiguration)
  , LeaderNode
  , RunContract(..)
  , UserConfiguration(UserConfiguration)
  , UserNode
  )
import Seath.Network.Users as Users
import Seath.Network.Utils (getPublicKeyHash, readResults)
import Seath.Test.Examples.Addition.Actions (queryBlockchainState) as Addition.Actions
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.Contract (initialSeathContract) as Addition.Contract
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum
  , AdditionRedeemer
  , AdditionValidator
  )
import Undefined (undefined)

mainTest :: ContractEnv -> KeyWallet -> KeyWallet -> Array KeyWallet -> Aff Unit
mainTest env admin leader users = do
  let
    initializationOptions = { waitingTime: 3, maxAttempts: 10 }

    mkRunner :: KeyWallet -> RunContract
    mkRunner kw = RunContract (\c -> runContractInEnv env $ withKeyWallet kw c)
    leaderUrl = "http://localhost:3000"

  checkInitSctipt env admin initializationOptions

  let
    serverConf :: SeathServerConfig
    serverConf = undefined

  user1 <- liftMaybe (error "No user wallet") (users !! 0)
  user2 <- liftMaybe (error "No user wallet") (users !! 1)
  user3 <- liftMaybe (error "No user wallet") (users !! 2)
  user4 <- liftMaybe (error "No user wallet") (users !! 3)

  coreConfig <- runContractInEnv env $ withKeyWallet leader $
    mkAdditionCoreConfig

  let -- hadler for the User to make action using CTL Contract
    buildChain actions =
      runContractInEnv env $ withKeyWallet leader $ fst
        <$> ChainBuilder.buildChain coreConfig actions Nothing

  log "Starting user-1 node"
  (userFiber1 /\ (userNode1 :: UserNode AdditionAction)) <- Users.startUserNode
    (makeTestUserConf leaderUrl (mkRunner user1) (pure <<< Right))

  log "Starting user-2 node"
  (userFiber2 /\ (userNode2 :: UserNode AdditionAction)) <- Users.startUserNode
    (makeTestUserConf leaderUrl (mkRunner user2) (pure <<< Right))

  log "Starting user-3 node"
  (userFiber3 /\ (userNode3 :: UserNode AdditionAction)) <- Users.startUserNode
    ( makeTestUserConf leaderUrl (mkRunner user3)
        (\_ -> pure $ Left "refuse to sign")
    )

  log "Starting user-4 node"
  (userFiber4 /\ (userNode4 :: UserNode AdditionAction)) <- Users.startUserNode
    (makeTestUserConf leaderUrl (mkRunner user4) (pure <<< Right))

  log "Initializing leader node"
  (leaderNode :: LeaderNode AdditionAction) <- Leader.newLeaderNode
    (makeTestLeaderConf (mkRunner leader) buildChain)

  log "Starting leader node loop"
  leaderLoopFiber <- forkAff $ Leader.leaderLoop leaderNode

  log "Starting server"
  server <- Server.runServer serverConf leaderNode >>= liftEither <<< lmap error
  log "Leader server started"

  log "Delay before user include action request"
  delay $ Milliseconds 1000.0
  log "Fire user-1 include action request"
  Users.performAction userNode1
    (AddAmount $ BigInt.fromInt 1)
  log "Fire user-2 include action request"
  Users.performAction userNode2
    (AddAmount $ BigInt.fromInt 10)
  log "Fire user-3 include action request"
  Users.performAction userNode3
    (AddAmount $ BigInt.fromInt 100)

  log "Fire user-4 include action request"
  Users.performAction userNode4
    (AddAmount $ BigInt.fromInt 1000)

  delay (wrap 10000.0)
  log "User 1 res:"
  readResults userNode1 >>= log <<< show
  log "User 4 res:"
  readResults userNode4 >>= log <<< show
  -- we don't really need this as all is run in supervise, but is good to have 
  -- the option
  killFiber (error "can't cleanup user") userFiber1
  killFiber (error "can't cleanup user") userFiber2
  killFiber (error "can't cleanup user") userFiber3
  killFiber (error "can't cleanup user") userFiber4
  killFiber (error "can't cleanup leader loop") leaderLoopFiber
  Payload.Server.close server
  log "end"

-- LeaderNode config
makeTestLeaderConf
  :: RunContract
  -> ( Array (UserAction AdditionAction)
       -> Aff (Array (FinalizedTransaction /\ UserAction AdditionAction))
     )
  -> LeaderConfiguration AdditionAction
makeTestLeaderConf runContract buildChain =
  LeaderConfiguration
    { maxWaitingTimeForSignature: 3000
    , maxQueueSize: 4
    , numberOfActionToTriggerChainBuilder: 4
    , maxWaitingTimeBeforeBuildChain: 3000
    , runContract
    , buildChain
    }

makeTestUserConf
  :: String
  -> RunContract
  -> (Transaction -> Aff (Either String Transaction))
  -> UserConfiguration AdditionAction
makeTestUserConf leaderUrl runContract checkChainedTx =
  UserConfiguration
    { networkHandlers: HttpUser.mkHttpHandlers leaderUrl
    , runContract
    , checkChainedTx
    }

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

mkAdditionCoreConfig
  ∷ Contract
      ( CoreConfiguration
          AdditionAction
          BigInt
          AdditionValidator
          AdditionDatum
          AdditionRedeemer
      )
mkAdditionCoreConfig = do
  vaildatorHash <- Addition.fixedValidatorHash
  leaderPkh <- getPublicKeyHash
  pure $ CoreConfiguration
    { leader: leaderPkh
    , stateVaildatorHash: vaildatorHash
    , actionHandler: Addition.handleAction
    , queryBlockchainState: Addition.queryBlockchainState
    }
