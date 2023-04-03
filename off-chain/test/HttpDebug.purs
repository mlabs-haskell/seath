module Seath.Test.HttpDebug where

import Aeson (class EncodeAeson)
import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.Config (LogLevel(Info), ServerConfig, emptyHooks)
import Contract.Monad (Aff, Contract, launchAff_, liftedM)
import Contract.Prelude
  ( Either(..)
  , bind
  , discard
  , liftEffect
  , log
  , note
  , pure
  , (<$>)
  , (<>)
  , (==)
  )
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Milliseconds(..), Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.UUID (UUID, genUUID, parseUUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (delay, forkAff)
import Payload.ResponseTypes (Response(..))
import Prelude (show)
import Prelude (($))
import Seath.Core.Types (ChangeAddress(..), UserAction(..))
import Seath.HTTP.Client as Client
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.Types (IncludeRequest(..))
import Seath.Network.Types
  ( IncludeActionError(..)
  , LeaderNode
  , UserConfiguration(..)
  , UserNode(..)
  , UserState(..)
  )
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.SeathSetup (stateChangePerAction)
import Seath.Test.Examples.Addition.Types (AdditionAction(..))
import Undefined (undefined)

main :: Effect Unit
main = do
  let
    serverConf :: SeathServerConfig
    serverConf = undefined

    leaderNode :: LeaderNode AdditionAction
    leaderNode = undefined

    userNode :: UserNode AdditionAction
    userNode = undefined

  launchAff_ do
    _ <- forkAff $ do
      log "Starting server"
      liftEffect $ Server.runServer serverConf leaderNode
      log "Leader server started"

    log "Delay before user include action request"
    delay $ Milliseconds 1000.0
    action <- genActionPlutip
    log "Fire user include action request"
    -- res <- userNode `userHandlerSubmitAction` action
    -- log $ "Include request result: " <> show res
    log "end"



genActionPlutip :: Aff (UserAction AdditionAction)
genActionPlutip = runPlutipContract config distrib
  $
    \(a /\ _b) -> genAction a
  -- log $ stringifyAeson $ encodeAeson req
  where
  distrib =
    ([ BigInt.fromInt 1_000_000_000 ] /\ [ BigInt.fromInt 1_000_000_000 ])

  genAction :: KeyWallet -> Contract (UserAction AdditionAction)
  genAction w =
    withKeyWallet w $ do
      ownUtxos <- liftedM "no UTxOs found" getWalletUtxos
      publicKeyHash <- withKeyWallet w getPublicKeyHash
      changeAddress <- liftedM "can't get Change address" $ head <$>
        getWalletAddressesWithNetworkTag
      pure $ UserAction
        { action: AddAmount stateChangePerAction
        , publicKey: publicKeyHash
        , userUTxOs: ownUtxos
        , changeAddress: ChangeAddress changeAddress
        }

-- Assembling UserNode
_user_node :: UserNode AdditionAction
_user_node = UserNode
  { state: UserState
      { pendingResponse: undefined
      , actionsSent: undefined
      , transactionsSent: undefined
      , submitedTransactions: undefined
      , numberOfActionsRequestsMade: undefined
      }
  , configuration: UserConfiguration
      { maxQueueSize: undefined
      , clientHandlers:
          { includeAction: userHandlerSubmitAction -- TODO: arch: naming
          , acceptSignedTransaction: undefined
          , rejectToSign: undefined
          , getActionStatus: undefined
          }

      }

  }

-- TODO: not real handler yet, just for server debugging
userHandlerSubmitAction
  :: forall a
   . EncodeAeson a
  => UserAction a
  -> Aff (Either IncludeActionError UUID)
userHandlerSubmitAction action = do
  res <- (Client.mkUserClient).leader.includeAction
    { body: IncludeRequest action }
  pure $ case res of
    Right resp -> convertResonse resp
    Left r -> Left $ OtherError $ "Leader failed to respond: " <> show r
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      note (OtherError "Can't parse request ID") $ parseUUID r.body.data
    -- FIXME: parse error from errData
    else Left $ OtherError $ "FIXME: parse error from errData: " <>
      r.body.errData


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
