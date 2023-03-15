-- | This is temporary canary test to make sure Plutip tests are runnable after environment updates.

module Seath.Test.Setup.Main (main) where

import Contract.Prelude
import Data.Time.Duration
import Undefined

import Contract.Config (emptyHooks, testnetConfig)
import Contract.Monad (launchAff_, runContract, runContractInEnv, withContractEnv)
import Contract.Test.Plutip (PlutipConfig)
import Contract.Wallet (privateKeysToKeyWallet)
import Contract.Wallet.Key (KeyWallet(..), keyWalletPrivatePaymentKey)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Control.Monad.Error.Class (liftMaybe, try)
import Data.UInt (fromInt) as UInt
import Effect.Class.Console (log)
import Node.FS.Aff (readdir)
import Node.Path as Path
import Seath.Test.Setup.ShareContract as Share

main :: Effect Unit
main = launchAff_ $ do
  (faucet /\ seathKeys) <-
    mkKeys "./wallet-setup/keys/faucet" "./wallet-setup/keys/seath_keys"

  withContractEnv config $ \env -> do
    runContractInEnv env (Share.payTo faucet faucet 500)

  log "end"

-- runContract config $ Share.share mkShareConf

-- mkShareConf :: Aff ShareConf
mkKeys :: String -> String -> Aff (KeyWallet /\ Array KeyWallet)
mkKeys faucetPath targetKeys = do
  faucet <- makeFaucetWallet
  log $ show (keyWalletPrivatePaymentKey faucet)
  seathKeys <- mekeSeathKeys
  pure (faucet /\ seathKeys)
  where

  makeFaucetWallet = makeKeyWallet faucetPath

  mekeSeathKeys = do
    keyDirs <- readdir targetKeys
    for keyDirs $ \keyDir -> makeKeyWallet $ Path.concat [targetKeys, keyDir]

  makeKeyWallet keysDir = do
    payment <- privatePaymentKeyFromFile $ Path.concat
      [ keysDir, "payment.skey" ]
    mbStake <- hush <$> try
      ( privateStakeKeyFromFile $ Path.concat
          [ keysDir, "stake.skey" ]
      )
    pure $ privateKeysToKeyWallet payment mbStake

config = testnetConfig