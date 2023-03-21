module Seath.Test.TestSetup where

import Contract.Prelude

import Contract.Wallet (privateKeysToKeyWallet)
import Contract.Wallet.Key (KeyWallet)
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Control.Monad.Error.Class (try)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Node.Path as Path

newtype RunnerConfig (s :: Type) = RunnerConfig
  { admin :: KeyWallet -- wallet that will run init contract
  , seathLeader :: KeyWallet
  , seathParticipants :: NonEmptyArray KeyWallet
  , minAdaRequired :: BigInt
  , expectedStateChange :: s -> s
  }

derive instance Newtype (RunnerConfig s) _

runnerConfInfo :: forall s. RunnerConfig s -> String
runnerConfInfo conf =
  "RunnerConfig { participants = "
    <> show (NE.length (unwrap conf).seathParticipants)
    <> " }"

makeKeyWallet :: String -> Aff KeyWallet
makeKeyWallet keysDir = do
  payment <- privatePaymentKeyFromFile $ Path.concat
    [ keysDir, "payment.skey" ]
  mbStake <- hush <$> try
    ( privateStakeKeyFromFile $ Path.concat
        [ keysDir, "stake.skey" ]
    )
  pure $ privateKeysToKeyWallet payment mbStake