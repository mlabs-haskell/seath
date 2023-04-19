module Seath.Network.OrderedMap
  ( OrderedMap(OrderedMap)
  , delete
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
  , take
  , toArray
  , union
  ) where

import Contract.Prelude hiding (lookup)

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Partial.Unsafe (unsafePartial)

-- We would like to use purescript-ordered-collections, but the 
-- Ord instance it has is over the k, and then we would need 
-- to cast it to a plain map quite often.
newtype OrderedMap k v = OrderedMap
  { map :: Map k (Int /\ v), array :: Array (k /\ v) }

derive instance Newtype (OrderedMap k v) _
derive newtype instance (Show k, Show v) => Show (OrderedMap k v)
derive newtype instance (Eq k, Eq v) => Eq (OrderedMap k v)

length :: forall k v. OrderedMap k v -> Int
length (OrderedMap ordMap) = Array.length ordMap.array

lookupPosition :: forall k v. Ord k => k -> OrderedMap k v -> Maybe Int
lookupPosition k _map = fst <$> lookupWithPosition k _map

lookup :: forall k v. Ord k => k -> OrderedMap k v -> Maybe v
lookup k _map = snd <$> lookupWithPosition k _map

lookupWithPosition
  :: forall k v. Ord k => k -> OrderedMap k v -> Maybe (Int /\ v)
lookupWithPosition k (OrderedMap oMap) = Map.lookup k oMap.map

-- TODO: tests when API will stabilaze

-- | Add `key` and `value` to `OrderedMap` tracking order of addition.
-- | Will update element if it member of `OrderedMap`
push
  :: forall k v
   . Ord k
  => k
  -> v
  -> OrderedMap k v
  -> OrderedMap k v
push key value (OrderedMap ordMap) =
  case Map.lookup key ordMap.map of
    Just (i /\ _oldValue) ->
      let
        map = Map.insert key (i /\ value) ordMap.map
        array =
          unsafePartial $ fromJust
            (Array.updateAt i (key /\ value) ordMap.array)
      in
        OrderedMap { map, array }
    Nothing ->
      let
        currIndex = Array.length ordMap.array
        map = Map.insert key (currIndex /\ value) ordMap.map
        array = Array.snoc ordMap.array (key /\ value)
      in
        OrderedMap { map, array }

empty :: forall a b. OrderedMap a b
empty = OrderedMap { map: Map.empty, array: [] }

orderedElems :: forall k v. OrderedMap k v -> Array (k /\ v)
orderedElems (OrderedMap oMap) = oMap.array

toArray :: forall k v. Ord k => OrderedMap k v -> Array (k /\ v)
toArray = orderedElems

take :: forall k v. Ord k => Int -> OrderedMap k v -> OrderedMap k v
take n (OrderedMap oMap) = fromFoldable (Array.take n oMap.array)

drop :: forall k v. Ord k => Int -> OrderedMap k v -> OrderedMap k v
drop n (OrderedMap oMap) = fromFoldable (Array.drop n oMap.array)

orderedKeys :: forall k v. OrderedMap k v -> Array k
orderedKeys (OrderedMap oMap) = fst <$> oMap.array

fromFoldable
  :: forall f k v
   . Foldable f
  => Ord k
  => f (k /\ v)
  -> OrderedMap k v
fromFoldable = foldl (\m (k /\ v) -> push k v m) empty

union :: forall k v. Ord k => OrderedMap k v -> OrderedMap k v -> OrderedMap k v
union first second = fromFoldable $ Array.concat
  [ orderedElems first, orderedElems second ]

delete
  :: forall k v
   . Ord k
  => k
  -> OrderedMap k v
  -> OrderedMap k v
delete k oMap =
  case lookupWithPosition k oMap of
    Just (i /\ _) ->
      fromFoldable
        $ unsafePartial
        $ fromJust
        $ Array.deleteAt i
        $ toArray oMap
    Nothing -> oMap
