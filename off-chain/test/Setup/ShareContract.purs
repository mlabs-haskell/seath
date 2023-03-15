module Seath.Test.Setup.ShareContract
  ( payTo
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId, getWalletAddress, getWalletAddresses)
import Contract.Chain (getTip)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Utxos (utxosAt)
import Contract.Wallet (getWalletUtxos, withKeyWallet)
import Control.Monad (bind)
import Ctl.Internal.Plutus.Types.Address (getAddress)
import Ctl.Internal.Serialization.Address (addressBech32)
import Ctl.Internal.Wallet.Key (KeyWallet(..))
import Data.Newtype (class Newtype)
import Data.Unit (Unit)

{-



- genreate keys, probably with staking

DONE
- use one as faucet
  - make faucet key
-}

type ShareConf = {
  faucet :: KeyWallet
}

payTo :: KeyWallet -> KeyWallet -> Int -> Contract Unit
payTo from to adaAmount = do
  withKeyWallet from $ do
    netId <- getNetworkId
    logInfo' $ "NetId: " <> show netId
    let serializableAddr = (unwrap from).address netId
    logInfo' $ "Faucet addr: " <> addressBech32 serializableAddr
    utxos <- getWalletUtxos
    logInfo' $ "Wallet UTXOs: " <> show utxos
    