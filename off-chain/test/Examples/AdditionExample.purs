module AdditionExample(executeExample) where

import Actions (class SeathAction)
import Contract.PlutusData (class ToData, toData)
import Contract.Prelude (genericShow, unwrap)
import Contract.Transaction (Transaction, TransactionHash)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.UInt (UInt)
import Effect.Aff (Aff)
import Prelude (Unit, class Eq, class Show)
import SeathData (UserAction)
import Undefined (undefined)

newtype AdditionAction = AddAmount UInt

instance SeathAction AdditionAction where
  seathToData = undefined


newtype AdditionDatum = AdditionDatum {lockedAmount::UInt}

derive instance Eq AdditionDatum
derive instance Generic AdditionDatum _
derive instance Newtype AdditionDatum _

instance Show AdditionDatum where
  show = genericShow

-- TODO : Define a CTL PlutusDataScheme and use generics
instance ToData AdditionDatum where
  toData (AdditionDatum {lockedAmount}) = toData lockedAmount

newtype AdditionRedeemer = AdditionRedeemer {increaseAmount :: UInt}

derive instance Eq AdditionRedeemer
derive instance Generic AdditionRedeemer _
derive instance Newtype AdditionRedeemer _

instance Show AdditionRedeemer where
  show = genericShow

-- TODO : Define a CTL PlutusDataScheme and use generics
instance ToData AdditionRedeemer where
  toData (AdditionRedeemer {increaseAmount}) = toData increaseAmount

action2UTxO :: UserAction AdditionAction -> TransactionHash -> Transaction
action2UTxO userAction useUTxO = undefined
--  case (unwrap userAction).action of 
--      AddAmount amount -> undefined -- TODO create a transaction consuming useUTxO that 
      -- need to be signed by leader and user

executeExample :: Aff Unit
executeExample = undefined
