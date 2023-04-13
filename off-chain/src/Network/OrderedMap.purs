module Seath.Network.OrderedMap
  ( OrderedMap(..)
  , drop
  , empty
  , length
  , lookupPosition
  , orderedElems
  , push
  , splitEither
  , take
  ) where

import Contract.Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
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

lookupPosition :: forall k v. Ord k => k -> OrderedMap k v -> Maybe Int
lookupPosition k (OrderedMap oMap) = fst <$> Map.lookup k oMap.map

-- TODO: tests when API will stabilaze
push
  :: forall keys values
   . Ord keys
  => keys
  -> values
  -> OrderedMap keys values
  -> OrderedMap keys values
push key value (OrderedMap ordMap) =
  let
    currIndex = Array.length ordMap.array
    map = Map.insert key (currIndex /\ value) ordMap.map
    array = Array.snoc ordMap.array (key /\ value)
  in
    OrderedMap { map, array }

splitEither
  :: forall a b c
   . OrderedMap a $ Either b c
  -> { success :: OrderedMap a c, failures :: OrderedMap a b }
splitEither = undefined

empty :: forall a b. OrderedMap a b
empty = OrderedMap { map: Map.empty, array: [] }

orderedElems :: forall k v. OrderedMap k v -> Array (k /\ v)
orderedElems (OrderedMap oMap) = oMap.array

take :: forall k v. Ord k => Int -> OrderedMap k v -> OrderedMap k v
take n (OrderedMap oMap) = foldr (uncurry push) empty
  (Array.take n oMap.array)

drop :: forall k v. Ord k => Int -> OrderedMap k v -> OrderedMap k v
drop n (OrderedMap oMap) = foldr (uncurry push) empty
  (Array.drop n oMap.array)
