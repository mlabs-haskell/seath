module Seath.Test.Unit.OrderedMap (spec) where

import Contract.Prelude hiding (length, lookup)

import Data.Array as Array
import Data.Map as Map
import Seath.Network.OrderedMap
  ( OrderedMap
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
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec =
  describe "OrderedMap" do
    it "lookupPosition" testLookupPosition
    it "lookup" testLookup
    it "lookupWithPosition" testLookupWithPosition
    it "length" testLength
    it "empty" testEmpty
    it "push" testPush
    it "conversions to array" testToArray
    it "orderedKeys" testOrderedKeys
    it "take" testTake
    it "drop" testDrop
    it "fromFoldable" testFromFoldable
    it "union" testUnion
    it "delete" testDelete

testMap ∷ OrderedMap String String
testMap = fromFoldable [ "1" /\ "a", "2" /\ "b", "3" /\ "c" ]

testLength :: Aff Unit
testLength =
  do
    length testMap `shouldEqual` 3
    length empty `shouldEqual` 0

testLookupPosition :: Aff Unit
testLookupPosition = do
  lookupPosition "2" testMap `shouldEqual` Just 1
  lookupPosition "2" empty `shouldEqual` Nothing
  lookupPosition "4" testMap `shouldEqual` Nothing

testLookup :: Aff Unit
testLookup = do
  lookup "2" testMap `shouldEqual` Just "b"
  lookup "2" (empty :: OrderedMap String String) `shouldEqual` Nothing
  lookup "4" testMap `shouldEqual` Nothing

testLookupWithPosition :: Aff Unit
testLookupWithPosition = do
  lookupWithPosition "2" testMap `shouldEqual` Just (1 /\ "b")
  lookupWithPosition "2" (empty :: OrderedMap String String)
    `shouldEqual` Nothing
  lookupWithPosition "4" testMap `shouldEqual` Nothing

testEmpty :: Aff Unit
testEmpty =
  (empty :: OrderedMap Int String)
    `shouldEqual`
      (wrap { map: Map.empty, array: [] })

testPush :: Aff Unit
testPush =
  let
    orderedMap = (push "1" "a" >>> push "2" "b" >>> push "3" "c") empty
    expectedArray = [ "1" /\ "a", "2" /\ "b", "3" /\ "c" ]
    expectedMap = Map.fromFoldable
      [ "1" /\ (0 /\ "a"), "2" /\ (1 /\ "b"), "3" /\ (2 /\ "c") ]

    updatedArray = [ "1" /\ "a", "2" /\ "b", "3" /\ "FF" ]
    updatedMap = Map.fromFoldable
      [ "1" /\ (0 /\ "a"), "2" /\ (1 /\ "b"), "3" /\ (2 /\ "FF") ]
  in
    do
      orderedMap `shouldEqual` (wrap { map: expectedMap, array: expectedArray })
      (push "1" "a" orderedMap) `shouldEqual` orderedMap
      (push "3" "FF" orderedMap) `shouldEqual`
        (wrap { map: updatedMap, array: updatedArray })

testToArray :: Aff Unit
testToArray =
  let
    expectedArray = [ "1" /\ "a", "2" /\ "b", "3" /\ "c" ]
  in
    do
      orderedElems testMap `shouldEqual` expectedArray
      toArray testMap `shouldEqual` orderedElems testMap

testOrderedKeys :: Aff Unit
testOrderedKeys = do
  let expectedKeys = [ "1", "2", "3" ]
  orderedKeys testMap `shouldEqual` expectedKeys
  orderedKeys empty `shouldEqual` ([] :: Array String)
  (orderedKeys $ fromFoldable $ Array.reverse $ toArray testMap)
    `shouldEqual` Array.reverse expectedKeys

testTake :: Aff Unit
testTake =
  let
    oMap = (push 1 11 >>> push 2 22 >>> push 3 33) empty
  in
    do
      take 2 oMap `shouldEqual` (push 1 11 >>> push 2 22) empty
      take 0 oMap `shouldEqual` empty
      take 10 oMap `shouldEqual` oMap

testDrop :: Aff Unit
testDrop =
  let
    oMap = (push 1 11 >>> push 2 22 >>> push 3 33) empty
  in
    do
      drop 2 oMap `shouldEqual` (push 3 33 empty)
      drop 0 oMap `shouldEqual` oMap
      drop 10 oMap `shouldEqual` empty

testFromFoldable :: Aff Unit
testFromFoldable =
  let
    array = [ "1" /\ "a", "2" /\ "b", "3" /\ "c" ]
  in
    do
      fromFoldable array `shouldEqual`
        (push "1" "a" >>> push "2" "b" >>> push "3" "c") empty
      toArray (fromFoldable array) `shouldEqual` array

testUnion :: Aff Unit
testUnion =
  let
    mapA = fromFoldable [ "1" /\ "a", "2" /\ "b" ]
    mapB = fromFoldable [ "3" /\ "c", "4" /\ "d" ]
    mapAB = fromFoldable [ "1" /\ "a", "2" /\ "b", "3" /\ "c", "4" /\ "d" ]
  in
    do
      union mapA mapB `shouldEqual` mapAB
      union mapB mapA `shouldNotEqual` mapAB
      union empty mapA `shouldEqual` mapA
      union mapA empty `shouldEqual` mapA

testDelete :: Aff Unit
testDelete =
  let
    oMap = fromFoldable [ "1" /\ "a", "2" /\ "b", "3" /\ "c" ]
    emptyMap = empty :: OrderedMap String Int
  in
    do
      delete "2" oMap `shouldEqual` fromFoldable [ "1" /\ "a", "3" /\ "c" ]
      delete "4" oMap `shouldEqual` oMap
      delete "4" emptyMap `shouldEqual` emptyMap