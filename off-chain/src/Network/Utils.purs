module Seath.Network.Utils (getPublicKeyHash, seath2Contract, contract2Seath) where

import Contract.Address (PubKeyHash, getWalletAddresses, toPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Function (($))
import Data.Functor ((<$>))
import Effect.Aff (error)
import Seath.Network.Types (SeathMonad)
import Undefined (undefined)

-- TODO: If we use SeathMonad a = ReaderT _ Contract a
-- we can call those `liftSeat` and `liftContract`

seath2Contract :: forall env a. SeathMonad env a -> Contract a
seath2Contract = undefined

contract2Seath :: forall env a. Contract a -> SeathMonad env a
contract2Seath = undefined

getPublicKeyHash :: Contract PubKeyHash
getPublicKeyHash = do
  address <- liftedM "can't get the address of KeyWallet" $ head <$>
    getWalletAddresses
  liftMaybe (error "can't get pubKeyHash of KeyWallet") $ toPubKeyHash address
