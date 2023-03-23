module Seath.Network.Users
  ( getSeathConfiguration
  , sendActionToLeader
  , waitForTransaction
  , sendSignedTxToLeader
  , sendRejectionToLeader
  , waitForActionConfirmation
  ) where

import Contract.Transaction (FinalizedTransaction)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Seath.Core.Types (CoreConfiguration, UserAction)
import Seath.Network.Types (Request, SeathMonad, SignedTransaction, UserNode)
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
