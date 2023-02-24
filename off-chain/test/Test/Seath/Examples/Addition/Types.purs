module Test.Seath.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum(AdditionDatum)
  , AdditionRedeemer(AdditionRedeemer)
  ) where

import Actions (class SeathAction)
import Contract.PlutusData (class ToData, toData)
import Contract.Prelude (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.UInt (UInt)
import Prelude (class Eq, class Show)
import Undefined (undefined)

newtype AdditionAction = AddAmount UInt

instance SeathAction AdditionAction where
  seathToData = undefined

newtype AdditionDatum = AdditionDatum { lockedAmount :: UInt }

derive instance Eq AdditionDatum
derive instance Generic AdditionDatum _
derive instance Newtype AdditionDatum _

instance Show AdditionDatum where
  show = genericShow

-- TODO : Define a CTL PlutusDataScheme and use generics
instance ToData AdditionDatum where
  toData (AdditionDatum { lockedAmount }) = toData lockedAmount

newtype AdditionRedeemer = AdditionRedeemer { increaseAmount :: UInt }

derive instance Eq AdditionRedeemer
derive instance Generic AdditionRedeemer _
derive instance Newtype AdditionRedeemer _

instance Show AdditionRedeemer where
  show = genericShow

-- TODO : Define a CTL PlutusDataScheme and use generics
instance ToData AdditionRedeemer where
  toData (AdditionRedeemer { increaseAmount }) = toData increaseAmount
