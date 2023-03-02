module Seath.Data (UserAction(UserAction), LeaderData(LeaderData)) where

import Contract.Prelude (class Newtype, Unit, undefined)
import Contract.Transaction (PublicKey, TransactionHash)
import Data.Newtype (wrap)

newtype UserAction a = UserAction
  { publicKey :: PublicKey
  , action :: a
  , userUTxo :: TransactionHash
  }

instance
  Newtype (UserAction a)
    { publicKey :: PublicKey
    , action :: a
    , userUTxo :: TransactionHash
    }

newtype LeaderData = LeaderData
  { publicKey :: PublicKey
  }

derive instance Newtype LeaderData _
