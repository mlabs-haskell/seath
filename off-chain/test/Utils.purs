module Seath.Test.Utils
  ( getPublicKeyHash
  , runnerConfInfo
  , makeKeyWallet
  , gen2Contract
  ) where

import Contract.Address (PubKeyHash, getWalletAddresses, toPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Control.Alternative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe, try)
import Data.Array (head)
import Data.Array.NonEmpty as NE
import Data.Either (hush)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Node.Path as Path
import Seath.Test.Types (RunnerConfiguration)
import Test.QuickCheck.Gen (Gen, randomSampleOne)

getPublicKeyHash :: KeyWallet -> Contract PubKeyHash
getPublicKeyHash kw = withKeyWallet kw do
  address <- liftedM "can't get the address of KeyWallet" $ head <$>
    getWalletAddresses
  liftMaybe (error "can't get pubKeyHash of KeyWallet") $ toPubKeyHash address

runnerConfInfo :: forall s. RunnerConfiguration s -> String
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

gen2Contract :: forall a. Gen a -> Contract a
gen2Contract generator = liftEffect $ randomSampleOne generator
