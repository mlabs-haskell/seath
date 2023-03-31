module Seath.Network.Users
  ( getSeathCoreConfiguration
  , sendActionToLeader
  , getActionStatus
  , sendSignedTransactionToLeader
  , sendRejectionToLeader
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
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Seath.Core.Types (CoreConfiguration, UserAction)
import Seath.Network.Types
  ( IncludeActionError
  , SignedTransaction
  , StatusResponse
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
  -> Effect $ Either IncludeActionError UUID
sendActionToLeader = undefined

getActionStatus :: forall a. UserNode a -> UUID -> Effect StatusResponse
getActionStatus = undefined

sendSignedTransactionToLeader
  :: forall a
   . UserNode a
  -> UUID
  -> SignedTransaction
  -- the first Either is to catch the network errors
  -> Effect Unit
sendSignedTransactionToLeader = undefined

-- | We refuse to sign the given transaction and inform the server
-- | explicitly.
sendRejectionToLeader
  :: forall a
   . UserNode a
  -> UUID
  -> Aff Unit
sendRejectionToLeader = undefined

makeUserAction :: forall a. UserNode a -> a -> UtxoMap -> UserAction a
makeUserAction nodeConfig action userUTxOs = undefined

makeUserActionAndSend
  :: forall a. UserNode a -> a -> Contract $ Either IncludeActionError UUID
makeUserActionAndSend nodeConfig actionRaw = do
  walletUTxOs <- liftedM "can't get walletUtxos" getWalletUtxos
  let
    action = makeUserAction nodeConfig actionRaw walletUTxOs
  liftEffect $ sendActionToLeader nodeConfig action

-- | Return a new mutable `userState`
newUserState :: forall a. Aff $ UserState a
newUserState = undefined

