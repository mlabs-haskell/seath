module Test.Seath.Examples.Addition.Actions (executeExample) where

import Effect.Aff (Aff)
import Prelude (Unit)
import Undefined (undefined)

import Test.Seath.Examples.Addition.Types
  ( AdditionAction
  , AdditionDatum
  , AdditionRedeemer
  )
import SeathData (UserAction)

import Contract.Transaction (Transaction, TransactionHash)

action2UTxO :: UserAction AdditionAction -> TransactionHash -> Transaction
action2UTxO userAction useUTxO = undefined

--  case (unwrap userAction).action of 
--      AddAmount amount -> undefined -- TODO create a transaction consuming useUTxO that 
-- need to be signed by leader and user

executeExample :: Aff Unit
executeExample = undefined
