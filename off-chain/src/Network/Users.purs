module Seath.Network.Users
  ( getSeathCoreConfiguration
  , sendActionToLeader
  , waitForTransaction
  , sendSignedTransactionToLeader
  , sendRejectionToLeader
  , waitForConfirmationOfFinalChainPartìcipation
  , makeUserAction
  , makeUserActionAndSend
  , startUserServer
  , stopUserServer
  , newUserState
  ) where

import Contract.Monad (Contract, liftedM)
import Contract.Utxos (UtxoMap, getWalletUtxos)
import Control.Monad (bind)
import Data.Either (Either)
import Data.Function (($))
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Seath.Core.Types (CoreConfiguration, UserAction)
import Seath.Network.Types
  ( AskForSignature
  , IncludeActionResponse
  , Ip
  , ProcessingActionError
  , SignedTransaction
  , UserNode
  , UserState
  )
import Type.Function (type ($))
import Undefined (undefined)

startUserServer :: forall a. UserNode a -> Aff Unit
startUserServer = undefined

stopUserServer :: forall a. UserNode a -> Aff Unit
stopUserServer = undefined

getSeathCoreConfiguration
  :: forall actionType userStateType validatorType datumType redeemerType
   . UserNode actionType
  -> CoreConfiguration actionType userStateType validatorType datumType
       redeemerType
getSeathCoreConfiguration = undefined

-- | This function won't raise a exception if we can't reach the network.
sendActionToLeader
  :: forall a
   . UserNode a
  -> UserAction a
  -> Aff $ Either String IncludeActionResponse
sendActionToLeader = undefined

waitForTransaction
  :: forall a
   . UserNode a
  -> IncludeActionResponse
  -- the first Either is to catch the network errors
  -> Aff $ Either String $ Either ProcessingActionError $ AskForSignature
waitForTransaction = undefined

sendSignedTransactionToLeader
  :: forall a
   . UserNode a
  -> AskForSignature
  -> SignedTransaction
  -- the first Either is to catch the network errors
  -> Aff $ Either String $ Either ProcessingActionError Unit
sendSignedTransactionToLeader = undefined

-- | We refuse to sign the given transaction and inform the server
-- | explicitly.
sendRejectionToLeader
  :: forall a
   . UserNode a
  -> AskForSignature
  -> Aff $ Either String Unit
sendRejectionToLeader = undefined

waitForConfirmationOfFinalChainPartìcipation
  :: forall a
   . UserNode a
  -> AskForSignature
  -> Aff Unit
waitForConfirmationOfFinalChainPartìcipation = undefined

makeUserAction :: forall a. UserNode a -> a -> UtxoMap -> UserAction a
makeUserAction nodeConfig action userUTxOs = undefined

-- TODO : Fixme
--   let
--     publicKey = (unwrap (unwrap nodeConfig).configuration).pubKeyHash
--     changeAddress = (unwrap (unwrap nodeConfig).configuration).changeAddress
--   in
--     wrap { action, publicKey, userUTxOs, changeAddress }

makeUserActionAndSend
  :: forall a. UserNode a -> a -> Contract $ Either String IncludeActionResponse
makeUserActionAndSend nodeConfig actionRaw = do
  walletUTxOs <- liftedM "can't get walletUtxos" getWalletUtxos
  let
    action = makeUserAction nodeConfig actionRaw walletUTxOs
  liftAff $ sendActionToLeader nodeConfig action

-- | Return a new mutable `userState`
newUserState :: forall a. Aff $ UserState a
newUserState = undefined

getOwnIp :: Aff Ip
getOwnIp = undefined
