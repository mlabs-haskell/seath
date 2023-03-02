module Seath.Test.Examples.Addition.Actions (action2ConstraintsAndLookup) where

import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\))
import Seath.Data (UserAction)
import Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum
  , AdditionRedeemer
  , AdditionValidator
  )
import Undefined (undefined)

-- TODO : How can we pass the old datum to this function? 
action2ConstraintsAndLookup
  :: UserAction AdditionAction
  -> Contract
       ( TxConstraints AdditionRedeemer AdditionDatum /\ ScriptLookups
           AdditionValidator
       )
action2ConstraintsAndLookup userAction =
  case (unwrap userAction).action of
    AddAmount _ -> undefined
