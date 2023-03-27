module Seath.Network.Users
  ( getSeathConfiguration
  , sendActionToLeader
  , waitForTransaction
  , sendSignedTxToLeader
  , sendRejectionToLeader
  , waitForActionConfirmation
  , makeUserAction
  , makeUserActionAndSend
  , startUserServer
  , stopUserServer
  ) where

import Contract.Monad (Contract, liftedM)
import Contract.Transaction (FinalizedTransaction)
import Contract.Utxos (UtxoMap, getWalletUtxos)
import Control.Monad (bind)
import Data.Either (Either)
import Data.Function (($))
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Seath.Core.Types (CoreConfiguration, UserAction)
import Seath.Network.Types (Request, SignedTransaction, UserNode)
import Type.Function (type ($))
import Undefined (undefined)

startUserServer :: UserNode -> Aff Unit
startUserServer = undefined

stopUserServer :: UserNode -> Aff Unit
stopUserServer = undefined

getSeathConfiguration
  :: forall actionType userStateType validatorType datumType redeemerType
   . UserNode
  -> CoreConfiguration actionType userStateType validatorType datumType
       redeemerType
getSeathConfiguration = undefined

sendActionToLeader
  :: forall a. UserNode -> UserAction a -> Aff $ Request $ UserAction a
sendActionToLeader = undefined

waitForTransaction
  :: forall a
   . UserNode
  -> Request $ UserAction a
  -> Aff $ Either String $ FinalizedTransaction /\ UserAction a
waitForTransaction = undefined

sendSignedTxToLeader
  :: forall a
   . UserNode
  -> SignedTransaction
  -> UserAction a
  -> Aff $ Request $ SignedTransaction /\ UserAction a
sendSignedTxToLeader = undefined

sendRejectionToLeader
  :: forall a
   . UserNode
  -> SignedTransaction
  -> UserAction a
  -> Aff $ Request $ SignedTransaction /\ UserAction a
sendRejectionToLeader = undefined

waitForActionConfirmation
  :: forall a
   . UserNode
  -> Request $ SignedTransaction /\ UserAction a
  -> Aff Unit
waitForActionConfirmation = undefined

makeUserAction :: forall a. UserNode -> a -> UtxoMap -> UserAction a
makeUserAction nodeConfig action userUTxOs =
  let
    publicKey = (unwrap (unwrap nodeConfig).configuration).pubKeyHash
    changeAddress = (unwrap (unwrap nodeConfig).configuration).changeAddress
  in
    wrap { action, publicKey, userUTxOs, changeAddress }

makeUserActionAndSend
  :: forall a. UserNode -> a -> Contract $ Request $ UserAction a
makeUserActionAndSend nodeConfig actionRaw = do
  walletUTxOs <- liftedM "can't get walletUtxos" getWalletUtxos
  let
    action = makeUserAction nodeConfig actionRaw walletUTxOs
  liftAff $ sendActionToLeader nodeConfig action

