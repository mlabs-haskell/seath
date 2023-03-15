module Seath.Test.Setup.ShareContract
  ( payTo
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId)
import Contract.Chain (getTip)
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
    tip <- getTip
    logInfo' $ "Tip: " <> show tip
    let serializableAddr = (unwrap from).address netId
    logInfo' $ "Faucet addr: " <> addressBech32 serializableAddr
    utxos <- getWalletUtxos
    logInfo' $ "Wallet UTXOs: " <> show utxos
    
{-
[INFO] 2023-03-15T11:01:13.821Z NetId: TestnetId
[DEBUG] 2023-03-15T11:01:13.822Z sending: {"version":"1.0","type":"jsonwsp/request","servicename":"ogmios","mirror":"Query-3byu3qrlf9knoim","methodname":"Query","args":{"query":"chainTip"}}
[DEBUG] 2023-03-15T11:01:13.825Z message: {"type":"jsonwsp/response","version":"1.0","servicename":"ogmios","methodname":"Query","result":{"slot":23194872,"hash":"4acd92f8ad1afeceb4243a3b9bbb1da20429995ffb230191cc82a98888298c5a"},"reflection":"Query-3byu3qrlf9knoim"}
[INFO] 2023-03-15T11:01:13.827Z Tip: (Tip (ChainTip { blockHeaderHash: (BlockHeaderHash "4acd92f8ad1afeceb4243a3b9bbb1da20429995ffb230191cc82a98888298c5a"), slot: (Slot fromString "23194872") }))
[INFO] 2023-03-15T11:01:13.828Z Faucet addr: addr_test1qzl8xfarvp90k605ppgpcqltfr62qahgd3pgy0jkal6zq5p8kp6hs4d6gdytf20gjqdfqzsl6vg04d5yfesyeeltyyfszsd4ph
[INFO] 2023-03-15T11:01:13.837Z Wallet UTXOs: Nothing
-}