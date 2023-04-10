module Seath.Test.HttpDebug
  ( config
  , genAction
  , main
  , runWithPlutip
  , userHandlerSendAction
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeJsonString)
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Contract.Address (PubKeyHash, getWalletAddressesWithNetworkTag)
import Contract.Config (emptyHooks)
import Contract.Monad (Contract, launchAff_, liftedM, runContractInEnv)
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Milliseconds(..), Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.UUID (UUID, parseUUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (delay, forkAff, try)
import Payload.ResponseTypes (Response(..))
import Prelude (show)
import Seath.Core.ChainBuilder as ChainBuilder
import Seath.Core.Types
  ( ChangeAddress(..)
  , CoreConfiguration(..)
  , UserAction(..)
  )
import Seath.HTTP.Client as Client
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.Types (IncludeRequest(..), JSend, UID(..))
import Seath.Network.Leader as Leader
import Seath.Network.OrderedMap as OMap
import Seath.Network.Types
  ( ActionStatus
  , GetStatusError(..)
  , IncludeActionError(..)
  , LeaderConfiguration(..)
  , LeaderNode
  , UserConfiguration(..)
  , UserNode
  , getPending
  , readSentActions
  )
import Seath.Network.Users as Users
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.SeathSetup (stateChangePerAction)
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(..)
  , AdditionDatum
  , AdditionRedeemer
  , AdditionValidator
  )
import Undefined (undefined)

main :: Effect Unit
main = do
  runWithPlutip

runWithPlutip :: Effect Unit
runWithPlutip = launchAff_ $ withPlutipContractEnv config distrib $
  \env (leader /\ user) -> do
    leaderPKH <- runContractInEnv env $ withKeyWallet leader getPublicKeyHash
    utxos <- runContractInEnv env $ withKeyWallet leader getWalletUtxos
    log $ "UTXOS: show " <> show utxos

    -- log $ "UTXOS: show " <> show utxos

    (userNode :: UserNode AdditionAction) <-
      Users.startUserNode _testUserConf

    coreConfig <- runContractInEnv env $ withKeyWallet leader $
      getTestCoreConfig leaderPKH
    let
      buildChain actions =
        runContractInEnv env $ withKeyWallet leader $ fst
          <$> ChainBuilder.buildChain coreConfig actions Nothing

    let leaderConf = _testLeaderConf leaderPKH
    (leaderNode :: LeaderNode AdditionAction) <-
      Leader.startLeaderNode
        buildChain
        -- (pure []) -- dummy
        leaderConf

    _ <- liftAff $ forkAff $ do
      log "Starting server"
      let
        serverConf :: SeathServerConfig
        serverConf = undefined
      liftEffect $ Server.runServer serverConf leaderNode

      log "Leader server started"

    runContractInEnv env $ withKeyWallet user $ do
      log' "Delay before user include action request"
      delay' 1000.0

      log' "Fire user include action request 1"
      Users.performAction userNode
        (AddAmount $ BigInt.fromInt 1)

      -- delay' 5000.0
      log' "Fire user include action request 2"
      Users.performAction userNode
        (AddAmount $ BigInt.fromInt 2)

    liftAff $ Leader.showDebugState leaderNode >>= log

    userActions <- (map snd <<< OMap.orderedElems) <$> (getPending leaderNode)

    log $ "User actions: " <> show userActions

    tryBuildResult <- try $ buildChain userActions
    log $ "Try build result: " <> show tryBuildResult

    log "end"

  where
  log' :: String -> Contract Unit
  log' = liftAff <<< log

  delay' :: Number -> Contract Unit
  delay' = liftAff <<< delay <<< Milliseconds
  distrib =
    ([ BigInt.fromInt 1_000_000_000 ] /\ [ BigInt.fromInt 1_000_000_000 ])

genAction :: KeyWallet -> Contract (UserAction AdditionAction)
genAction w =
  withKeyWallet w $ do
    ownUtxos <- liftedM "no UTxOs found" getWalletUtxos
    publicKeyHash <- getPublicKeyHash
    changeAddress <- liftedM "can't get Change address" $ head <$>
      getWalletAddressesWithNetworkTag
    pure $ UserAction
      { action: AddAmount stateChangePerAction
      , publicKey: publicKeyHash
      , userUTxOs: ownUtxos
      , changeAddress: ChangeAddress changeAddress
      }

-- Assembling LeaderNode

_testLeaderConf :: PubKeyHash -> LeaderConfiguration AdditionAction
_testLeaderConf leaderPKH =
  LeaderConfiguration
    { maxWaitingTimeForSignature: 0
    , maxQueueSize: 4
    , numberOfActionToTriggerChainBuilder: 3
    , maxWaitingTimeBeforeBuildChain: 0
    , leaderPkh: leaderPKH
    }

-- Assembling UserNode
_testUserConf :: UserConfiguration AdditionAction
_testUserConf = UserConfiguration
  { maxQueueSize: undefined
  , clientHandlers:
      { submitToLeader: userHandlerSendAction -- TODO: arch: naming
      , acceptSignedTransaction: undefined
      , rejectToSign: undefined
      , getActionStatus: userHandlerGetStatus
      }

  }

userHandlerSendAction
  :: forall a
   . EncodeAeson a
  => UserAction a
  -> Aff (Either IncludeActionError UUID)
userHandlerSendAction action = do
  res <- (Client.mkUserClient).leader.includeAction
    { body: IncludeRequest action }
  pure $ case res of
    Right resp -> do
      convertResonse resp
    Left r -> Left $ IAOtherError $ "Leader failed to respond: " <> show r
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      note (IAOtherError "Can't parse request ID") $ parseUUID r.body.data
    else either (show >>> IAOtherError >>> Left) Left
      (decodeJsonString r.body.data)

{- FIXME 
  used Affjax insted - can't compile with 

  res <- (Client.mkUserClient).leader.actionStatus  { params: {uid: UID uuid} }
          ^^^^^^^^^^^^^^^^^^^
  No type class instance was found for
    Aeson.EncodeAeson t3
  The instance head contains unknown type variables. Consider adding a type annotation.
-}
-- userHandlerGetStatus :: UUID -> Aff (Either GetStatusError ActionStatus)
-- userHandlerGetStatus uuid = do
--   res <- (Client.mkUserClient).leader.actionStatus  { params: {uid: UID uuid} }
--   pure $ case res of
--     Right resp -> do
--       log $ "Get staus resp: " <> show resp
--       Right Processing
--       -- convertResonse resp
--     Left r -> Left $ GSOtherError $ "Leader failed to respond: " <> show r

userHandlerGetStatus :: UUID -> Aff (Either GetStatusError ActionStatus)
userHandlerGetStatus uuid = do
  let url = "http://localhost:3000/leader/action-status/" <> show (UID uuid)
  resp <- Affjax.get ResponseFormat.string url
  pure $ case resp of
    Left _ -> Left $ GSOtherError "Got Affjax error"
    Right r -> do
      (jsend :: JSend GetStatusError ActionStatus) <- decode r.body
      if (jsend.status == "success") then
        decode jsend.data
      else either (show >>> GSOtherError >>> Left) Left
        (decode jsend.data)
  where
  decode :: forall a. DecodeAeson a => String -> (Either GetStatusError a)
  decode = lmap (show >>> GSOtherError) <<< decodeJsonString

getTestCoreConfig
  ∷ PubKeyHash
  → Contract
      ( CoreConfiguration
          AdditionAction
          BigInt
          AdditionValidator
          AdditionDatum
          AdditionRedeemer
      )
getTestCoreConfig leaderPublicKeyHash = do
  vaildatorHash <- Addition.fixedValidatorHash

  pure $ CoreConfiguration
    { leader: leaderPublicKeyHash
    , stateVaildatorHash: vaildatorHash
    , actionHandler: Addition.handleAction
    , queryBlockchainState: Addition.queryBlockchainState
    }

-- Plutip config
config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Info
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 1.0 }
  }
