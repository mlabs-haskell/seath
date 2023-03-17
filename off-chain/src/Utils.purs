module Seath.Utils (findOwnOutputs, getFinalizedTransactionHash) where

import Contract.Address (Address, getNetworkId, validatorHashEnterpriseAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.Prelude (unwrap)
import Contract.Scripts (ValidatorHash)
import Contract.Transaction
  ( FinalizedTransaction
  , TransactionHash
  , TransactionOutputWithRefScript
  )
import Contract.Utxos (UtxoMap)
import Control.Applicative (pure)
import Control.Monad (bind)
import Ctl.Internal.Hashing as Ctl.Internal.Hashing
import Ctl.Internal.Plutus.Types.Transaction (_output)
import Ctl.Internal.Serialization as Ctl.Internal.Serialization
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Lens ((^.))
import Data.Map as Map
import Effect.Class (liftEffect)

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

