module Seath.Types
  ( UserAction(UserAction)
  , LeaderData(LeaderData)
  , StateReturn(StateReturn)
  ) where

import Prelude

import Contract.Prelude (class Newtype)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (PublicKey, TransactionHash)
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (UtxoMap)

newtype UserAction a = UserAction
  { publicKey :: PublicKey
  , action :: a
  , userUTxo :: UtxoMap
  }

instance showUserAction :: Show (UserAction a) where
  show (UserAction a) = 
    "UserAction { publicKey :: " 
    <> show a.publicKey
    <> ", userUTxo :: " <> show a.userUTxo
    <> " }"

instance
  Newtype (UserAction a)
    { publicKey :: PublicKey
    , action :: a
    , userUTxo :: UtxoMap
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
