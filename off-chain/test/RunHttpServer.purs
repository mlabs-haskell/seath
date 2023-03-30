-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module Seath.Test.RunHttpServer (main) where

import Aeson

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson)
import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.Config (LogLevel(Info), emptyHooks)
import Contract.Monad (Contract, launchAff_, liftedM)
import Contract.Monad (launchAff_)
import Contract.Prelude (Either, bind, log, map, pure, (<$>))
import Contract.Test (withKeyWallet)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Array.NonEmpty as NE
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Show (show)
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (error)
import Payload.Server.DecodeBody (decodeBody)
import Prelude (($))
import Seath.Core.Types (ChangeAddr(..), UserAction(UserAction))
import Seath.Network.Server (runServer)
import Seath.Network.Types (IncludeActionRequest(IncludeActionRequest))
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.SeathSetup (stateChangePerAction)
import Seath.Test.Examples.Addition.Types (AdditionAction(..))
import Seath.Test.QuickCheck (makeDistribution)
import Seath.Test.Types (Participant(Participant))
import Undefined (undefined)

main :: Effect Unit
main = do
  _ <- log $ show $ encodeAeson $ TestA { a: "fff", b: 4 }
  _ <- runConract

  let serverConf = undefined
  runServer serverConf

newtype TestA = TestA { a :: String, b :: Int }

-- derive instance Generic TestA _
derive newtype instance DecodeAeson TestA
derive newtype instance EncodeAeson TestA

-- derive instance TestA _

runConract :: Effect Unit
runConract = launchAff_
  $ runPlutipContract config distrib
  $
    \(a /\ _b) -> do
      action <- genAction a

      let
        req :: IncludeActionRequest AdditionAction
        req = IncludeActionRequest
          { controlNumber: 1
          , ip: "IP"
          , port: "port"
          , action: action
          }
      log $ stringifyAeson $ encodeAeson req
  -- let res :: Either String (UserAction AdditionAction) 
  --     res = decodeBody jsonAction
  -- log $ show $ res
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
      , changeAddress: ChangeAddr changeAddress
      }

jsonAction =
  "{\"userUTxOs\":[[{\"transactionId\":\"440b5c0a9b585cc3e21aae2384e729383b50291f33669b35c029609ceeddafb6\",\"index\":0},{\"scriptRef\":null,\"output\":{\"referenceScript\":null,\"datum\":{\"tag\":\"NoOutputDatum\",\"contents\":{}},\"amount\":{\"getValue\":[[{\"unCurrencySymbol\":\"\"},[[{\"unTokenName\":\"\"},1000000000]]]]},\"address\":{\"addressStakingCredential\":null,\"addressCredential\":{\"tag\":\"PubKeyCredential\",\"contents\":{\"getPubKeyHash\":\"ad3355081bf5d0375ce0b55431337a7fa8d63dd15400d61d0da18642\"}}}}}]],\"publicKey\":{\"getPubKeyHash\":\"ad3355081bf5d0375ce0b55431337a7fa8d63dd15400d61d0da18642\"},\"changeAddress\":\"addr1vxknx4ggr06aqd6uuz64gvfn0fl6343a692qp4sapkscvssrtmzl5\",\"action\":100}"

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