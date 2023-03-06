module Seath.Types
  ( UserAction(UserAction)
  , LeaderData(LeaderData)
  , StateReturn(StateReturn)
  ) where

import Contract.Prelude (class Newtype)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (PublicKey, TransactionHash)
import Contract.TxConstraints (TxConstraints)

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

newtype StateReturn validatorType redeemerType datumType stateType = StateReturn
  { constraints ::
      TxConstraints redeemerType datumType
  , lookups ::
      ScriptLookups validatorType
  , userState :: stateType
  }

derive instance Newtype (StateReturn a b c d) _
