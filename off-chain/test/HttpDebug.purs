module Seath.Test.HttpDebug where

import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.Config (LogLevel(Info), ServerConfig, emptyHooks)
import Contract.Monad (Aff, Contract, launchAff_, liftedM)
import Contract.Prelude (bind, discard, liftEffect, log, pure, (<$>))
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Show (show)
import Data.Time.Duration (Milliseconds(..), Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (delay, forkAff)
import Prelude (($))
import Seath.Core.Types (ChangeAddress(..), UserAction(..))
import Seath.HTTP.Client as Client
import Seath.HTTP.Server (SeathServerConfig)
import Seath.HTTP.Server as Server
import Seath.HTTP.Types (IncludeRequest(..))
import Seath.Network.Types (LeaderNode, UserNode)
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
    delay $ Milliseconds 2000.0
    action <- runContract
    log "Fire user include action request"
    res <- userNode `userHandlerSubmitAction` action
    log $ show res
    log "end"

  where
  -- TODO: not real handler, just for server debugging
  userHandlerSubmitAction userNode action =
    (Client.mkUserClient userNode).leader.includeAction
      { body: IncludeRequest action }

runContract :: Aff (UserAction AdditionAction)
runContract = runPlutipContract config distrib
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
