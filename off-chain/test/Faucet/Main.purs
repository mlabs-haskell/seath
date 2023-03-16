module Seath.Test.Faucet.Main (main) where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , defaultOgmiosWsConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Monad (launchAff_, runContractInEnv, withContractEnv)
import Contract.Wallet (KeyWallet, withKeyWallet)
import Data.UInt (fromInt) as UInt
import Effect.Class.Console (log)
import Node.FS.Aff (readdir)
import Node.Path as Path
import Seath.Test.Faucet.Contract as Faucet
import Seath.Test.TestSetup (makeKeyWallet)

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
  seathKeys <- mekeSeathKeys
  pure (faucet /\ seathKeys)
  where

  makeFaucetWallet = makeKeyWallet faucetPath

  mekeSeathKeys = do
    keyDirs <- readdir targetKeys
    for keyDirs $ \keyDir -> makeKeyWallet $ Path.concat [ targetKeys, keyDir ]

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
