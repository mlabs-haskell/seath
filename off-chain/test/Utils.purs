module Seath.Test.Utils
  ( runnerConfInfo
  , makeKeyWallet
  , gen2Contract
  ) where

import Contract.Monad (Contract)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet)
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Control.Alternative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (try)
import Data.Array.NonEmpty as NE
import Data.Either (hush)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path as Path
import Seath.Test.Types (RunnerConfiguration)
import Test.QuickCheck.Gen (Gen, randomSampleOne)

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

gen2Contract :: forall a. Gen a -> Contract a
gen2Contract generator = liftEffect $ randomSampleOne generator
