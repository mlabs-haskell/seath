module Seath.Test.Faucet.Contract
  ( payTo
  ) where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash
  , StakePubKeyHash
  , getNetworkId
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (mustPayToPubKeyAddress)
import Contract.Value (Coin(Coin), Value, coinToValue)
import Contract.Wallet (withKeyWallet)
import Control.Monad (bind)
import Ctl.Internal.Serialization.Address (addressBech32)
import Ctl.Internal.Wallet.Key (KeyWallet)
import Data.BigInt as BigInt
import Data.Unit (Unit)

-- | Using "current wallet" as faucet pay specified amount of Ada to KeyWallet
payTo :: KeyWallet -> Int -> Contract Unit
payTo toWallet adaAmount = do
  netId <- getNetworkId
  let addr = addressBech32 $ (unwrap toWallet).address netId
  logInfo' $ "Sending " <> show adaAmount <> " Ada to " <> addr

  (pkh /\ skh) <- getKeysHashes toWallet
  let constraints = mustPayToPubKeyAddress pkh skh (adaValue adaAmount)
  let lookups = mempty :: ScriptLookups Void
  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  logInfo' $ "Received " <> show adaAmount <> " Ada by " <> addr

getKeysHashes :: KeyWallet -> Contract (PaymentPubKeyHash /\ StakePubKeyHash)
getKeysHashes kw = do
  withKeyWallet kw $ do
    pkh <- liftedM "Failed to get PubKeyHash" ownPaymentPubKeyHash
    skh <- liftedM "Failed to get StakeKeyHash" ownStakePubKeyHash

    pure (pkh /\ skh)

adaValue ∷ Int → Value
adaValue ada = coinToValue
  (Coin $ (BigInt.fromInt ada) * (BigInt.fromInt 1_000_000))
