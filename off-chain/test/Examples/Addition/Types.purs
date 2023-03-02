module Seath.Test.Examples.Addition.Types
  ( AdditionAction
      ( AddAmount
      )
  , AdditionDatum
      ( AdditionDatum
      )
  , AdditionRedeemer
      ( AdditionRedeemer
      )
  , AdditionParams
  ) where

import Actions
  ( class SeathAction
  )
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
import Contract.Prelude
  ( genericShow
  )
import Data.BigInt
  ( BigInt
  )
import Data.Generic.Rep
  ( class Generic
  )
import Data.Newtype
  ( class Newtype
  )
import Data.Unit
  ( Unit
  )
import Prelude
  ( class Eq
  , class Show
  )
import Undefined
  ( undefined
  )

type AdditionParams =
  Unit

newtype AdditionAction =
  AddAmount
    BigInt

instance
  SeathAction AdditionAction where
  seathToData =
    undefined

newtype AdditionDatum =
  AdditionDatum
    { lockedAmount ::
        BigInt
    }

derive instance
  Eq AdditionDatum

derive instance
  Generic AdditionDatum
    _

derive instance
  Newtype AdditionDatum
    _

instance
  Show AdditionDatum where
  show =
    genericShow

instance
  HasPlutusSchema
    AdditionDatum
    ( "AdditionDatum"
        :=
          ( "lockedAmount"
              :=
                I
                  BigInt
              :+
                PNil
          )
        @@
          Z
        :+
          PNil
    )

instance
  ToData AdditionDatum where
  toData =
    genericToData

instance
  FromData AdditionDatum where
  fromData =
    genericFromData

newtype AdditionRedeemer =
  AdditionRedeemer
    { increaseAmount ::
        BigInt
    }

derive instance
  Eq AdditionRedeemer

derive instance
  Generic AdditionRedeemer
    _

derive instance
  Newtype AdditionRedeemer
    _

instance
  Show AdditionRedeemer where
  show =
    genericShow

instance
  HasPlutusSchema
    AdditionRedeemer
    ( "AdditionRedeemer"
        :=
          ( "increaseAmount"
              :=
                I
                  BigInt
              :+
                PNil
          )
        @@
          Z
        :+
          PNil
    )

instance
  ToData AdditionRedeemer where
  toData =
    genericToData

instance
  FromData AdditionRedeemer where
  fromData =
    genericFromData
