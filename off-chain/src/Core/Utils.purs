module Seath.Core.Utils
  ( findOwnOutputs
  , getFinalizedTransactionHash
  , mkActionContract
  ) where

import Contract.Address
  ( Address
  , getNetworkId
  , getWalletAddressesWithNetworkTag
  , validatorHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Prelude (unwrap, wrap)
import Contract.Scripts (ValidatorHash)
import Contract.Transaction
  ( FinalizedTransaction
  , TransactionHash
  , TransactionOutputWithRefScript
  )
import Contract.Utxos (UtxoMap, getWalletUtxos)
import Control.Applicative (pure)
import Control.Monad (bind)
import Ctl.Internal.Hashing as Ctl.Internal.Hashing
import Ctl.Internal.Plutus.Types.Transaction (_output)
import Ctl.Internal.Serialization as Ctl.Internal.Serialization
import Data.Array (head)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Lens ((^.))
import Data.Map as Map
import Effect.Class (liftEffect)
import Seath.Core.Types (UserAction(UserAction))
import Seath.Network.Utils (getPublicKeyHash)

findOwnOutputs :: ValidatorHash -> UtxoMap -> Contract UtxoMap
findOwnOutputs valHash utxos = do
  netId <- getNetworkId
  addr <- liftContractM "cannot get validator address" $
    validatorHashEnterpriseAddress netId valHash
  pure $ Map.filter (hasSameAddr addr) utxos
  where
  hasSameAddr :: Address -> TransactionOutputWithRefScript -> Boolean
  hasSameAddr addr out = addr == (unwrap (out ^. _output)).address

getFinalizedTransactionHash :: FinalizedTransaction -> Contract TransactionHash
getFinalizedTransactionHash fTx = do
  liftEffect $ Ctl.Internal.Hashing.transactionHash <$>
    Ctl.Internal.Serialization.convertTransaction (unwrap fTx)

mkActionContract :: forall a. a -> Contract (UserAction a)
mkActionContract action = do
  ownUtxos <- liftedM "Error making action: no UTxOs found" getWalletUtxos
  publicKeyHash <- getPublicKeyHash
  changeAddress <- liftedM "can't get Change address" $ head <$>
    getWalletAddressesWithNetworkTag
  pure $ UserAction
    { action: action
    , publicKey: publicKeyHash
    , userUTxOs: ownUtxos
    , changeAddress: (wrap changeAddress)
    }