module Test.Seath.Examples.Addition.Actions
  ( executeExample
  ) where

import Contract.Transaction
  ( Transaction
  , TransactionHash
  )
import Effect.Aff
  ( Aff
  )
import Prelude
  ( Unit
  )
import Seath.Test.Examples.Addition.Types
  ( AdditionAction
  , AdditionDatum
  , AdditionRedeemer
  )
import SeathData
  ( UserAction
  )
import Undefined
  ( undefined
  )

action2UTxO
  :: UserAction
       AdditionAction
  -> TransactionHash
  -> Transaction
action2UTxO
  userAction
  useUTxO =
  undefined

--  case (unwrap userAction).action of 
--      AddAmount amount -> undefined -- TODO create a transaction consuming useUTxO that 
-- need to be signed by leader and user

executeExample
  :: Aff
       Unit
executeExample =
  undefined
