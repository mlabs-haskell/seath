module Seath.Core.Types
  ( ChainBuilderState(ChainBuilderState)
  , ChangeAddress(ChangeAddress)
  , CoreConfiguration(CoreConfiguration)
  , StateReturn(StateReturn)
  , UserAction(UserAction)
  , changeAddress'
  ) where

import Contract.Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , fromString
  , toString
  )
import Contract.Address
  ( AddressWithNetworkTag
  , PubKeyHash
  , addressWithNetworkTagFromBech32
  , addressWithNetworkTagToBech32
  )
import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, class ToData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts (class DatumType, class RedeemerType, ValidatorHash)
import Contract.Transaction (FinalizedTransaction)
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (UtxoMap)
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Either (Either(Left))
import Data.Function (($))
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show, show)
import Data.Tuple.Nested (type (/\))

newtype UserAction a = UserAction
  { publicKey :: PubKeyHash
  , action :: a
  , userUTxOs :: UtxoMap
  , changeAddress :: ChangeAddress
  }

changeAddress' ∷ forall a. UserAction a -> AddressWithNetworkTag
changeAddress' (UserAction a) = unwrap a.changeAddress

instance showUserAction :: Show a => Show (UserAction a) where
  show (UserAction a) =
    "UserAction { publicKey: "
      <> show a.publicKey
      <> ", action: "
      <> show a.action
      <> " }"

instance
  Newtype (UserAction a)
    { publicKey :: PubKeyHash
    , action :: a
    , userUTxOs :: UtxoMap
    , changeAddress :: ChangeAddress
    }

derive newtype instance Eq a => Eq (UserAction a)
derive newtype instance EncodeAeson a => EncodeAeson (UserAction a)
derive newtype instance DecodeAeson a => DecodeAeson (UserAction a)

newtype ChangeAddress = ChangeAddress AddressWithNetworkTag

derive instance Newtype ChangeAddress _
derive newtype instance Eq ChangeAddress

instance DecodeAeson ChangeAddress where
  decodeAeson a = do
    addrS <- maybe (Left $ TypeMismatch "String") pure $ toString a
    addr <- maybe (Left $ TypeMismatch "Bech32 AddressWithNetworkTag") pure $
      addressWithNetworkTagFromBech32 addrS
    pure $ ChangeAddress addr

instance EncodeAeson ChangeAddress where
  encodeAeson (ChangeAddress addr) =
    fromString $ addressWithNetworkTagToBech32 addr

newtype StateReturn validatorType datumType redeemerType stateType = StateReturn
  { constraints ::
      TxConstraints redeemerType datumType
  , lookups ::
      ScriptLookups validatorType
  , userState :: stateType
  }

derive instance Newtype (StateReturn a b c d) _

newtype CoreConfiguration
  (actionType :: Type)
  (userStateType :: Type)
  (validatorType :: Type)
  (datumType :: Type)
  (redeemerType :: Type) = CoreConfiguration
  { leader :: PubKeyHash
  , stateValidatorHash :: ValidatorHash
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
  , queryBlockchainState :: Contract (UtxoMap /\ userStateType)
  }

newtype ChainBuilderState actionType userStateType = ChainBuilderState
  { pendingActions ::
      Array (UserAction actionType)
  , finalizedTransactions ::
      Array (FinalizedTransaction /\ UserAction actionType)
  , lastResult :: UtxoMap /\ userStateType
  }

instance
  Newtype (ChainBuilderState actionType userStateType)
    { pendingActions ::
        Array (UserAction actionType)
    , finalizedTransactions ::
        Array (FinalizedTransaction /\ UserAction actionType)
    , lastResult :: UtxoMap /\ userStateType
    }
