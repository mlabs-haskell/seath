module Seath.Network.Users where

import Contract.Transaction (FinalizedTransaction)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Seath.Core.Types (SeathConfig, UserAction)
import Seath.Network.Types (Request, SeathMonad, SignedTransaction)
import Type.Function (type ($))
import Undefined (undefined)

getSeathConfiguration
  :: forall actionType userStateType validatorType datumType redeemerType
   . SeathMonad
       ( SeathConfig actionType userStateType validatorType datumType
           redeemerType
       )
getSeathConfiguration = undefined

-- TODO: What's the right type for this?
-- queryLeader

sendActionToLeader
  :: forall a. UserAction a -> SeathMonad $ Request $ UserAction a
sendActionToLeader = undefined

waitForTransaction
  :: forall a
   . Request $ UserAction a
  -> SeathMonad $ Either String (FinalizedTransaction /\ UserAction a)
waitForTransaction = undefined

sendSignedTxToLeader
  :: forall a
   . SignedTransaction
  -> UserAction a
  -> SeathMonad $ Request $ (SignedTransaction /\ UserAction a)
sendSignedTxToLeader = undefined

sendRejectionToLeader
  :: forall a
   . SignedTransaction
  -> UserAction a
  -> SeathMonad $ Request (SignedTransaction /\ UserAction a)
sendRejectionToLeader = undefined

waitForActionConfirmation
  :: forall a. Request (SignedTransaction /\ UserAction a) -> SeathMonad Unit
waitForActionConfirmation = undefined
