module Seath.Network.Users
  ( getActionStatus
  , getSeathCoreConfiguration
  , makeUserAction
  , makeUserActionAndSend
  , newUserState
  , performAction
  , sendActionToLeader
  , sendRejectionToLeader
  , sendSignedTransactionToLeader
  , startUserNode
  , startUserServer
  , stopUserServer
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedM)
import Contract.Utxos (UtxoMap, getWalletUtxos)
import Control.Monad (bind)
import Data.Either (Either)
import Data.Function (($))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UUID (UUID)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, try)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Seath.Core.Types (CoreConfiguration, UserAction)
import Seath.Network.OrderedMap as OMap
import Seath.Network.Types
  ( ActionStatus
  , GetStatusError
  , IncludeActionError
  , SignedTransaction
  , UserConfiguration
  , UserNode(UserNode)
  , UserState(UserState)
  , addToSentActions
  , readSentActions
  , userHandlers
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
  -> Aff $ Either IncludeActionError UUID
sendActionToLeader userNode action =
  (userHandlers userNode).submitToLeader action

-- Query server for action status
getActionStatus
  :: forall a. UserNode a -> UUID -> Aff (Either GetStatusError ActionStatus)
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
sendRejectionToLeader userNode uuid = do
  res <- try $ (userHandlers userNode).refuseToSign uuid
  log $ "User: refusing to sing " <> show uuid <> ", result: " <> show res

makeUserAction :: forall a. UserNode a -> a -> UtxoMap -> UserAction a
makeUserAction nodeConfig action userUTxOs = undefined

makeUserActionAndSend
  :: forall a. UserNode a -> a -> Contract $ Either IncludeActionError UUID
makeUserActionAndSend nodeConfig actionRaw = do
  walletUTxOs <- liftedM "can't get walletUtxos" getWalletUtxos
  let
    action = makeUserAction nodeConfig actionRaw walletUTxOs
  liftAff $ sendActionToLeader nodeConfig action

-- | Return a new mutable `userState`
newUserState :: forall a. Aff $ UserState a
newUserState = undefined

performAction :: forall a. UserNode a -> a -> Aff Unit
performAction userNode action = do
  userAction <- (unwrap userNode).makeAction action
  result <- userNode `sendActionToLeader` userAction
  case result of
    Left err -> log $ "TODO: React to error " <> show err
    Right uid -> do
      userNode `addToSentActions` (uid /\ action)

startUserNode
  :: forall a
   . Show a
  => (a -> Aff (UserAction a))
  -> UserConfiguration a
  -> Aff (UserNode a)
startUserNode makeAction conf = do
  actionsSent <- liftEffect $ Ref.new OMap.empty

  let
    node = UserNode
      { state: UserState
          { pendingResponse: undefined
          , actionsSent: actionsSent
          , transactionsSent: undefined
          , submitedTransactions: undefined
          , numberOfActionsRequestsMade: undefined
          }
      , configuration: conf
      , makeAction: makeAction

      }
  startActionStatusCheck node
  pure node

startActionStatusCheck :: forall a. Show a => UserNode a -> Aff Unit
startActionStatusCheck userNode = do
  log $ "Start checking actions"
  -- TODO: probably, need to collect this fibers to kill them properly on node stop
  _fiber <- forkAff check
  pure unit
  where
  check = do
    sent <- readSentActions userNode
    for_ sent $ \(uid /\ action) -> do
      -- TODO: process response
      res <- (userHandlers userNode).getActionStatus uid
      log $ "User: status of action " <> show uid <> ": " <> show res
      delay $ Milliseconds 500.0
    delay $ Milliseconds 2000.0
    check

