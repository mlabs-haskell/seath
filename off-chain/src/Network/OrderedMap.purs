module Seath.Network.OrderedMap
  ( OrderedMap(OrderedMap)
  , length
  , push
  , splitEither
  , empty
  ) where

import Contract.Prelude as Array
import Data.Either (Either)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Type.Function (type ($))
import Undefined (undefined)

-- This represent a Map that can remember the order of insertion.
-- it seems that we can use purescript-ordered-collections with a 
-- type like this.
-- I would like to use purescript-ordered-collections, but the 
-- Ord instance it has is over the keys, and then we would need 
-- to cast it to a plain map quite often.
newtype OrderedMap keys values = OrderedMap
  { map :: Map keys (Int /\ values), array :: Array (keys /\ values) }

derive instance Newtype (OrderedMap keys values) _

length :: forall keys values. OrderedMap keys values -> Int
length (OrderedMap ordMap) = Array.length ordMap.array

push
  :: forall keys values
   . OrderedMap keys values
  -> keys
  -> values
  -> OrderedMap keys values
push (OrderedMap ordMap) key value = undefined

splitEither
  :: forall a b c
   . OrderedMap a $ Either b c
  -> { success :: OrderedMap a c, failures :: OrderedMap a b }
splitEither = undefined

empty :: forall a b. OrderedMap a b
empty = undefined
