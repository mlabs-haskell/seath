module Seath.Test.Utils
  ( Distribution
  , makeDistribution
  , makeKeyWallet
  , plutipConfig
  ) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Test.Plutip (PlutipConfig)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet)
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Control.Alternative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (try)
import Data.Array (replicate)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (hush)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt as UInt
import Effect.Aff (Aff)
import Node.Path as Path

makeKeyWallet :: String -> Aff KeyWallet
makeKeyWallet keysDir = do
  payment <- privatePaymentKeyFromFile $ Path.concat
    [ keysDir, "payment.skey" ]
  mbStake <- hush <$> try
    ( privateStakeKeyFromFile $ Path.concat
        [ keysDir, "stake.skey" ]
    )
  pure $ privateKeysToKeyWallet payment mbStake

type Distribution = ((Array BigInt /\ Array BigInt) /\ Array (Array BigInt))

makeDistribution :: Int -> Distribution
makeDistribution participantsNumber =
  let
    adminDistribution = [ BigInt.fromInt 1_000_000_000 ]
    leaderDistribution = [ BigInt.fromInt 1_000_000_000 ]
    usersDistribution = replicate participantsNumber
      [ BigInt.fromInt 1_000_000_000 ]
  in
    (adminDistribution /\ leaderDistribution) /\ usersDistribution

plutipConfig :: PlutipConfig
plutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Info
  , ogmiosConfig:
      { port: UInt.fromInt 8081
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 8080
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
