module Seath.Network.Utils (getPublicKeyHash) where

import Contract.Address (PubKeyHash, getWalletAddresses, toPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Function (($))
import Data.Functor ((<$>))
import Effect.Aff (error)

getPublicKeyHash :: Contract PubKeyHash
getPublicKeyHash = do
  address <- liftedM "can't get the address of KeyWallet" $ head <$>
    getWalletAddresses
  liftMaybe (error "can't get pubKeyHash of KeyWallet") $ toPubKeyHash address
