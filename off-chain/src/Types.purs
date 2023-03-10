module Seath.Types
  ( UserAction(UserAction)
  , StateReturn(StateReturn)
  , SeathConfig(SeathConfig)
  , ChainBuilderState(ChainBuilderState)
  ) where

import Contract.Address (AddressWithNetworkTag, PubKeyHash)
import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, class ToData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts (class DatumType, class RedeemerType, ValidatorHash)
import Contract.Transaction (FinalizedTransaction)
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (UtxoMap)
import Data.Either (Either)
import Data.Monoid ((<>))
import Data.Newtype (class Newtype)
import Data.Show (class Show, show)
import Data.Tuple.Nested (type (/\))

newtype UserAction a = UserAction
  { publicKey :: PubKeyHash
  , action :: a
  , userUTxo :: UtxoMap
  , changeAddress :: AddressWithNetworkTag
  }

instance showUserAction :: Show (UserAction a) where
  show (UserAction a) =
    "UserAction { publicKey :: "
      <> show a.publicKey
      <> ", userUTxo :: "
      <> show a.userUTxo
      <> " }"

instance
  Newtype (UserAction a)
    { publicKey :: PubKeyHash
    , action :: a
    , userUTxo :: UtxoMap
    , changeAddress :: AddressWithNetworkTag
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
  , stateVaildatorHash :: ValidatorHash
  -- used for the first transaction in chain
  -- on next iteration hadler swithched to one that gets script utxos from previous transaction
  , chainStartStateUtxos :: Contract UtxoMap
  , actionHandler ::
      DatumType validatorType datumType
      => RedeemerType validatorType redeemerType
      => FromData datumType
      => ToData datumType
      => FromData redeemerType
      => ToData redeemerType
      => ( UserAction actionType
           -> userStateType
           -> Contract UtxoMap
           -> Contract
                (StateReturn validatorType datumType redeemerType userStateType)
         )
  -- , finalizedTxHandler ::
  --     DatumType validatorType datumType
  --     => RedeemerType validatorType redeemerType
  --     => FromData datumType
  --     => ToData datumType
  --     => FromData redeemerType
  --     => ToData redeemerType
  --     => ( UserAction actionType
  --          -> userStateType
  --          -> FinalizedTransaction
  --          -> Contract
  --               (StateReturn validatorType datumType redeemerType userStateType)
  --        )
  -- , onchainHandler ::
  --     DatumType validatorType datumType
  --     => RedeemerType validatorType redeemerType
  --     => FromData datumType
  --     => ToData datumType
  --     => FromData redeemerType
  --     => ToData redeemerType
  --     => ( UserAction actionType
  --          -> TransactionHash
  --          -> Contract
  --               (StateReturn validatorType datumType redeemerType userStateType)
  --        )
  }

newtype ChainBuilderState actionType userStateType = ChainBuilderState
  { pendingActions ::
      Array (UserAction actionType)
  , finalizedTransactions ::
      Array (FinalizedTransaction /\ UserAction actionType)
  , lastResult :: Either userStateType (FinalizedTransaction /\ userStateType)
  }

instance
  Newtype (ChainBuilderState actionType userStateType)
    { pendingActions ::
        Array (UserAction actionType)
    , finalizedTransactions ::
        Array (FinalizedTransaction /\ UserAction actionType)
    , lastResult ::
        Either userStateType (FinalizedTransaction /\ userStateType)
    }
