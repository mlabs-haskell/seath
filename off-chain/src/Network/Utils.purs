module Seath.Network.Utils (getPublicKeyHash,seathUUIDShow,seathParseUUID) where

import Contract.Address (PubKeyHash, getWalletAddresses, toPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just,Nothing))
import Data.UUID (UUID)
import Effect.Aff (error)

getPublicKeyHash :: Contract PubKeyHash
getPublicKeyHash = do
  address <- liftedM "can't get the address of KeyWallet" $ head <$>
    getWalletAddresses
  liftMaybe (error "can't get pubKeyHash of KeyWallet") $ toPubKeyHash address

foreign import _seathUUIDShow :: UUID -> String
foreign import _seathParseUUID ::  (forall x . x -> Maybe x) -> (forall x . Maybe x) ->String -> Maybe UUID

seathUUIDShow :: UUID -> String
seathUUIDShow = _seathUUIDShow

seathParseUUID :: String -> Maybe UUID
seathParseUUID = _seathParseUUID Just Nothing 
