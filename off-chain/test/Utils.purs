module Seath.Test.Utils
  ( runnerConfInfo
  , makeKeyWallet
  , makeDistribution
  ) where

import Contract.Chain (waitNSlots)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Numeric.Natural (fromInt)
import Contract.Wallet (KeyWallet, getWalletUtxos, privateKeysToKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Control.Alternative (pure)
import Control.Applicative ((*>))
import Control.Monad (bind, unless)
import Control.Monad.Error.Class (liftMaybe, try)
import Data.Array (replicate)
import Data.Array.NonEmpty as NE
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (hush)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Monoid ((<>))
import Data.Natural (Natural)
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit, unit)
import Effect.Aff (Aff, error)
import Node.Path as Path
import Prelude (discard)
import Seath.Test.Types (BlockchainState, RunnerConfiguration)

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

