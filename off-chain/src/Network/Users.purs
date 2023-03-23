module Seath.Network.Users
  ( getSeathConfiguration
  , sendActionToLeader
  , waitForTransaction
  , sendSignedTxToLeader
  , sendRejectionToLeader
  , waitForActionConfirmation
  , makeUserAction
  ) where

import Contract.Address
  ( AddressWithNetworkTag
  , getWalletAddressesWithNetworkTag
  )
import Contract.Monad (liftedM)
import Contract.Transaction (FinalizedTransaction)
import Contract.Utxos (UtxoMap)
import Contract.Wallet (getWalletUtxos)
import Control.Applicative (pure)
import Control.Monad (bind)
import Control.Monad.Reader.Trans (asks)
import Data.Array (head)
import Data.Either (Either)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Seath.Core.Types (CoreConfiguration, UserAction)
import Seath.Network.Types (Request, SeathMonad, SignedTransaction, UserNode)
import Seath.Network.Utils (contract2Seath)
import Type.Function (type ($))
import Undefined (undefined)

getSeathConfiguration
  :: forall actionType userStateType validatorType datumType redeemerType
   . SeathMonad UserNode $
       CoreConfiguration actionType userStateType validatorType datumType
         redeemerType
getSeathConfiguration = undefined

sendActionToLeader
  :: forall a. UserAction a -> SeathMonad UserNode $ Request $ UserAction a
sendActionToLeader = undefined

waitForTransaction
  :: forall a
   . Request $ UserAction a
  -> SeathMonad UserNode $ Either String (FinalizedTransaction /\ UserAction a)
waitForTransaction = undefined

sendSignedTxToLeader
  :: forall a
   . SignedTransaction
  -> UserAction a
  -> SeathMonad UserNode $ Request $ (SignedTransaction /\ UserAction a)
sendSignedTxToLeader = undefined

sendRejectionToLeader
  :: forall a
   . SignedTransaction
  -> UserAction a
  -> SeathMonad UserNode $ Request (SignedTransaction /\ UserAction a)
sendRejectionToLeader = undefined

waitForActionConfirmation
  :: forall a
   . Request (SignedTransaction /\ UserAction a)
  -> SeathMonad UserNode Unit
waitForActionConfirmation = undefined

makeUserActionWith
  :: forall a
   . a
  -> UtxoMap
  -> AddressWithNetworkTag
  -> SeathMonad UserNode $ UserAction a
makeUserActionWith action userUTxOs changeAddress = do
  publicKey <- asks \x -> (unwrap (unwrap x).configuration).pubKeyHash
  pure $ wrap { action, publicKey, userUTxOs, changeAddress }

makeUserAction :: forall a. a -> SeathMonad UserNode $ UserAction a
makeUserAction action = do
  userUTxOs <- contract2Seath $ liftedM "no UTXOs found" getWalletUtxos
  -- TODO: fix this to use the `getChangeAddress` function
  changeAddress <- contract2Seath $ liftedM "can't get Change address" $ head
    <$>
      getWalletAddressesWithNetworkTag
  makeUserActionWith action userUTxOs changeAddress
