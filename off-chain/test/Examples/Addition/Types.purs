module Seath.Test.Examples.Addition.Types
  ( AdditionAction(AddAmount)
  , AdditionParams
  , AdditionRedeemer(AdditionRedeemer)
  , AdditionState
  , AdditionValidator
  , AdditionDatum(AdditionDatum)
  , initialState
  ) where

import Aeson (class DecodeAeson, class EncodeAeson)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  , Z
  , genericFromData
  , genericToData
  )
import Contract.Prelude (genericShow)
import Contract.Scripts (class DatumType, class RedeemerType)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Unit (Unit)
import Prelude (class Eq, class Show)

type AdditionState = BigInt

initialState :: AdditionState
initialState = BigInt.fromInt 100

type AdditionParams = Unit

data AdditionValidator

instance DatumType AdditionValidator AdditionDatum
instance RedeemerType AdditionValidator AdditionRedeemer

newtype AdditionAction = AddAmount BigInt

derive newtype instance EncodeAeson AdditionAction
derive newtype instance DecodeAeson AdditionAction

derive instance Eq AdditionAction
derive instance Generic AdditionAction _
derive instance Newtype AdditionAction _

instance Show AdditionAction where
  show = genericShow

newtype AdditionDatum = AdditionDatum { lockedAmount :: BigInt }

derive instance Eq AdditionDatum
derive instance Generic AdditionDatum _
derive instance Newtype AdditionDatum _

instance Show AdditionDatum where
  show = genericShow

instance
  HasPlutusSchema
    AdditionDatum
    ( "AdditionDatum" := ("lockedAmount" := I BigInt :+ PNil) @@ Z
        :+ PNil
    )

instance ToData AdditionDatum where
  toData = genericToData

instance FromData AdditionDatum where
  fromData = genericFromData

newtype AdditionRedeemer = AdditionRedeemer { increaseAmount :: BigInt }

derive instance Eq AdditionRedeemer
derive instance Generic AdditionRedeemer _
derive instance Newtype AdditionRedeemer _

instance Show AdditionRedeemer where
  show = genericShow

instance
  HasPlutusSchema
    AdditionRedeemer
    ( "AdditionRedeemer" := ("increaseAmount" := I BigInt :+ PNil) @@ Z
        :+ PNil
    )

instance ToData AdditionRedeemer where
  toData = genericToData

instance FromData AdditionRedeemer where
  fromData = genericFromData
