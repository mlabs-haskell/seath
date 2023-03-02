module SeathData where

import Actions (class SeathAction)
import Contract.Transaction (PublicKey, TransactionHash)
import Data.Newtype (class Newtype)
import Undefined (undefined)

data UserAction a = UserAction
  { publickKey :: PublicKey
  , action :: a
  , userUTxos :: Array TransactionHash
  }

instance SeathAction a => SeathAction (UserAction a) where
  seathToData = undefined
