module Seath.Types
  ( UserAction(UserAction)
  , StateReturn(StateReturn)
  , SeathConfig(SeathConfig)
  , ChainBuilderState(ChainBuilderState)
  ) where

import Contract.Address (PubKeyHash(..))
import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, class ToData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts (class DatumType, class RedeemerType)
import Contract.Transaction (FinalizedTransaction, TransactionHash)
import Contract.TxConstraints (TxConstraints)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))

newtype UserAction a = UserAction
  { publicKey :: PubKeyHash
  , action :: a
  , userUTxo :: TransactionHash
  }

instance
  Newtype (UserAction a)
    { publicKey :: PubKeyHash
    , action :: a
    , userUTxo :: TransactionHash
    }

newtype StateReturn validatorType datumType redeemerType stateType = StateReturn
  { constraints ::
      TxConstraints redeemerType datumType
  , lookups ::
      ScriptLookups validatorType
  , userState :: stateType
  }

derive instance Newtype (StateReturn a b c d) _

newtype SeathConfig
  (actionType :: Type)
  (userStateType :: Type)
  (validatorType :: Type)
  (datumType :: Type)
  (redeemerType :: Type) = SeathConfig
  { leader :: PubKeyHash
  , finalizedTxHandler ::
      DatumType validatorType datumType
      => RedeemerType validatorType redeemerType
      => FromData datumType
      => ToData datumType
      => FromData redeemerType
      => ToData redeemerType
      => ( UserAction actionType
           -> userStateType
           -> FinalizedTransaction
           -> Contract
                (StateReturn validatorType datumType redeemerType userStateType)
         )
  , onchainHandler ::
      DatumType validatorType datumType
      => RedeemerType validatorType redeemerType
      => FromData datumType
      => ToData datumType
      => FromData redeemerType
      => ToData redeemerType
      => ( UserAction actionType
           -> TransactionHash
           -> Contract
                (StateReturn validatorType datumType redeemerType userStateType)
         )
  }

newtype ChainBuilderState actionType userStateType = ChainBuilderState
  { pendingActions ::
      Array (UserAction actionType)
  , finalizedTransactions ::
      Array (FinalizedTransaction /\ UserAction actionType)
  , lastResult :: Either TransactionHash (FinalizedTransaction /\ userStateType)
  }

instance
  Newtype (ChainBuilderState actionType userStateType)
    { pendingActions ::
        Array (UserAction actionType)
    , finalizedTransactions ::
        Array (FinalizedTransaction /\ UserAction actionType)
    , lastResult ::
        Either TransactionHash (FinalizedTransaction /\ userStateType)
    }
