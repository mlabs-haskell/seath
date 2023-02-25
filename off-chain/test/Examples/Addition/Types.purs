module Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionDatum(AdditionDatum)
  , AdditionRedeemer(AdditionRedeemer)
  , AdditionParams
  ) where

import Actions (class SeathAction)
import Contract.PlutusData (class FromData, class ToData, fromData, toData)
import Contract.Prelude (genericShow)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Unit (Unit)
import Prelude (class Eq, class Show, ($))
import Undefined (undefined)
import Control.Monad (bind)
import Control.Applicative (pure)

type AdditionParams = Unit

newtype AdditionAction = AddAmount BigInt

instance SeathAction AdditionAction where
  seathToData = undefined

newtype AdditionDatum = AdditionDatum { lockedAmount :: BigInt }

derive instance Eq AdditionDatum
derive instance Generic AdditionDatum _
derive instance Newtype AdditionDatum _

instance Show AdditionDatum where
  show = genericShow

-- TODO : Define a CTL PlutusDataScheme and use generics
instance ToData AdditionDatum where
  toData (AdditionDatum { lockedAmount }) = toData lockedAmount

instance FromData AdditionDatum where
  fromData dat = do
    lockedValue <- fromData dat
    pure $ AdditionDatum { lockedAmount: lockedValue }

newtype AdditionRedeemer = AdditionRedeemer { increaseAmount :: BigInt }

derive instance Eq AdditionRedeemer
derive instance Generic AdditionRedeemer _
derive instance Newtype AdditionRedeemer _

instance Show AdditionRedeemer where
  show = genericShow

-- TODO : Define a CTL PlutusDataScheme and use generics
instance ToData AdditionRedeemer where
  toData (AdditionRedeemer { increaseAmount }) = toData increaseAmount

instance FromData AdditionRedeemer where
  fromData dat = do
    increase <- fromData dat
