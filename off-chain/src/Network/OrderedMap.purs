module Seath.Network.OrderedMap
  ( OrderedMap(OrderedMap)
  , drop
  , empty
  , fromFoldable
  , length
  , lookup
  , lookupPosition
  , lookupWithPosition
  , orderedElems
  , orderedKeys
  , push
  , splitEither
  , take
  , union
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
derive newtype instance (Show k, Show v) => Show (OrderedMap k v)

length :: forall keys values. OrderedMap keys values -> Int
length (OrderedMap ordMap) = Array.length ordMap.array

lookupPosition :: forall k v. Ord k => k -> OrderedMap k v -> Maybe Int
lookupPosition k _map = fst <$> lookupWithPosition k _map

lookup :: forall k v. Ord k => k -> OrderedMap k v -> Maybe v
lookup k _map = snd <$> lookupWithPosition k _map

lookupWithPosition
  :: forall k v. Ord k => k -> OrderedMap k v -> Maybe (Int /\ v)
lookupWithPosition k (OrderedMap oMap) = Map.lookup k oMap.map

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

orderedKeys :: forall k v. OrderedMap k v -> Array k
orderedKeys (OrderedMap oMap) = fst <$> oMap.array

fromFoldable
  :: forall f k v
   . Foldable f
  => Ord k
  => f (k /\ v)
  -> OrderedMap k v
fromFoldable = foldr (\(k /\ v) -> push k v) empty

union :: forall k v. Ord k => OrderedMap k v -> OrderedMap k v -> OrderedMap k v
union first second = fromFoldable $ Array.concat
  [ orderedElems first, orderedElems second ]
