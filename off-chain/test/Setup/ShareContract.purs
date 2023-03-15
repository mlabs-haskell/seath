module Seath.Test.Setup.ShareContract
  ( payTo
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Wallet (getWalletUtxos, withKeyWallet)
import Control.Monad (bind)
import Ctl.Internal.Serialization.Address (addressBech32)
import Ctl.Internal.Wallet.Key (KeyWallet)
import Data.Unit (Unit)


payTo :: KeyWallet -> KeyWallet -> Int -> Contract Unit
payTo from to adaAmount = do
  withKeyWallet from $ do
    netId <- getNetworkId
    logInfo' $ "NetId: " <> show netId
    let serializableAddr = (unwrap from).address netId
    logInfo' $ "Faucet addr: " <> addressBech32 serializableAddr
    utxos <- getWalletUtxos
    logInfo' $ "Wallet UTXOs: " <> show utxos
    
{-
[INFO] 2023-03-15T10:24:54.617Z NetId: TestnetId
[INFO] 2023-03-15T10:24:54.621Z Faucet addr: addr_test1qzl8xfarvp90k605ppgpcqltfr62qahgd3pgy0jkal6zq5p8kp6hs4d6gdytf20gjqdfqzsl6vg04d5yfesyeeltyyfszsd4ph
[INFO] 2023-03-15T10:24:54.646Z Faucet UTXOs: Nothing
-}