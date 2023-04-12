module Seath.Test.Utils
  ( runnerConfInfo
  , makeKeyWallet
  , makeDistribution
  ) where

import Contract.Wallet (KeyWallet, privateKeysToKeyWallet)
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Control.Alternative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (try)
import Data.Array (replicate)
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (hush)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Node.Path as Path
import Seath.Test.Types (RunnerConfiguration)

runnerConfInfo
  :: forall s actionType. RunnerConfiguration s actionType -> String
runnerConfInfo conf =
  "RunnerConfig { participants = "
    <> show (NE.length (unwrap conf).participants)
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
