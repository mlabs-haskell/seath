module Seath.Test.Faucet.Main (main) where

import Contract.Config
  ( ContractParams
  , defaultOgmiosWsConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Monad (launchAff_, runContractInEnv, withContractEnv)
import Contract.Prelude
  ( type (/\)
  , Aff
  , Effect
  , Maybe(..)
  , Unit
  , bind
  , discard
  , for
  , for_
  , hush
  , pure
  , show
  , ($)
  , (/\)
  , (<$>)
  )
import Contract.Wallet (privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet.Key (KeyWallet, keyWalletPrivatePaymentKey)
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Control.Monad.Error.Class (try)
import Data.UInt (fromInt) as UInt
import Effect.Class.Console (log)
import Node.FS.Aff (readdir)
import Node.Path as Path
import Seath.Test.Faucet.Contract as Faucet

main :: Effect Unit
main = launchAff_ $ do
  let
    faucetPath = "./test/keys/faucet"
    targetsPath = "./test/keys/seath_keys"
    adaToPay = 500

  (faucet /\ seathKeys) <- mkKeys faucetPath targetsPath

  withContractEnv config $ \env -> do
    for_ seathKeys $ \seathWallet ->
      runContractInEnv env
        (withKeyWallet faucet $ Faucet.payTo seathWallet adaToPay)
  log "Funds seding end"

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
    for keyDirs $ \keyDir -> makeKeyWallet $ Path.concat [ targetKeys, keyDir ]

  makeKeyWallet keysDir = do
    payment <- privatePaymentKeyFromFile $ Path.concat
      [ keysDir, "payment.skey" ]
    mbStake <- hush <$> try
      ( privateStakeKeyFromFile $ Path.concat
          [ keysDir, "stake.skey" ]
      )
    pure $ privateKeysToKeyWallet payment mbStake

config :: ContractParams
config = testnetConfig
  { backendParams = mkCtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig
      , kupoConfig:
          { port: UInt.fromInt 1442
          , host: "localhost"
          , secure: false
          , path: Nothing
          }
      }
  }
