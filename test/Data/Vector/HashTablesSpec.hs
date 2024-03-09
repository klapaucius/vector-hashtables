{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Data.Vector.HashTablesSpec where

import           Control.Monad.Primitive
import           Data.Hashable                   (Hashable (hashWithSalt))
import qualified Data.List                       as L
import           Data.Primitive.MutVar
import           Data.Proxy                      (Proxy (..))
import qualified Data.Set                        as Set
import           Data.Vector.Generic             (Mutable, Vector)
import qualified Data.Vector.Generic             as VI
import           Data.Vector.Generic.Mutable     (MVector)
import qualified Data.Vector.Mutable             as M
import qualified Data.Vector.Storable.Mutable    as SM
import qualified Data.Vector.Unboxed             as U
import qualified Data.Vector.Unboxed.Mutable     as UM
import           GHC.Generics                    (Generic)
import           Test.Hspec.QuickCheck           (modifyMaxSuccess)
import           Test.QuickCheck                 (Arbitrary (..), Gen,
                                                  NonNegative (..),
                                                  Positive (..), Property,
                                                  choose, elements, forAll,
                                                  generate, property, shuffle,
                                                  vector)

import           Test.Hspec                      (Spec, describe, errorCall, it,
                                                  shouldBe, shouldThrow)

import qualified Data.Vector.Hashtables.Internal as VH

newtype AlwaysCollide = AC Int
    deriving newtype (Arbitrary, SM.Storable, Num, Eq, Ord, Show)
    deriving stock Generic

instance Hashable AlwaysCollide where
    hashWithSalt _ _ = 1

listN :: Int -> Gen [(Int, Int)]
listN n = do
  keys <- vector n
  vals <- vector n
  let keys' = Set.toList (Set.fromList keys)
  return (zip keys' vals)

shuffledListN :: Int -> Gen ([(Int, Int)], [(Int, Int)])
shuffledListN n = do
  testData <- listN n
  shuffledTestData <- shuffle testData
  return (testData, shuffledTestData)

listsForRemoveN :: Int -> Gen ([(Int, Int)], [Int])
listsForRemoveN n = do
  testData <- listN n
  dropCount <- min (n - 1) <$> choose (1, n)
  let deleteData = fst <$> take dropCount testData
  return (testData, deleteData)

twoListsN :: Int -> Gen ([(Int, Int)], [(Int, Int)])
twoListsN n = do
  list1 <- listN n
  list2 <- listN n
  return (list1, list2)

spec :: Spec
spec = mutableSpec
  *> storableMutableSpec
  *> storableKeysSpec
  *> unboxedKeysSpec

class HashTableTest ks vs where

  specDescription :: Proxy ks -> Proxy vs -> String

  testInit :: Proxy ks -> Proxy vs -> Int -> IO (VH.Dictionary (PrimState IO) ks Int vs Int)

  testInsert :: (VH.Dictionary (PrimState IO) ks Int vs Int) -> Int -> Int -> IO ()

  testAt :: (VH.Dictionary (PrimState IO) ks Int vs Int) -> Int -> IO Int

  testAt' :: (VH.Dictionary (PrimState IO) ks Int vs Int) -> Int -> IO (Maybe Int)

  testDelete :: (VH.Dictionary (PrimState IO) ks Int vs Int) -> Int -> IO ()

  testInitCollide
    :: Proxy ks
    -> Proxy vs
    -> Int
    -> IO (VH.Dictionary (PrimState IO) ks AlwaysCollide vs Int)

  testInsertCollide
    :: (VH.Dictionary (PrimState IO) ks AlwaysCollide vs Int) -> AlwaysCollide -> Int -> IO ()

  testAtCollide
    :: (VH.Dictionary (PrimState IO) ks AlwaysCollide vs Int) -> AlwaysCollide -> IO Int

  testFromList
    :: Proxy ks -> Proxy vs -> [(Int, Int)] -> IO (VH.Dictionary (PrimState IO) ks Int vs Int)

  testToList :: VH.Dictionary (PrimState IO) ks Int vs Int -> IO [(Int, Int)]

  testLength :: VH.Dictionary (PrimState IO) ks Int vs Int -> IO Int

  testNull :: VH.Dictionary (PrimState IO) ks Int vs Int -> IO Bool

  testMember :: VH.Dictionary (PrimState IO) ks Int vs Int -> Int -> IO Bool

  testAlter :: VH.Dictionary (PrimState IO) ks Int vs Int -> (Maybe Int -> Maybe Int) -> Int -> IO ()

  testUpsert :: VH.Dictionary (PrimState IO) ks Int vs Int -> (Maybe Int -> Int) -> Int -> IO ()

  testUnion
    :: VH.Dictionary (PrimState IO) ks Int vs Int
    -> VH.Dictionary (PrimState IO) ks Int vs Int
    -> IO (VH.Dictionary (PrimState IO) ks Int vs Int)

  testDifference
    :: VH.Dictionary (PrimState IO) ks Int vs Int
    -> VH.Dictionary (PrimState IO) ks Int vs Int
    -> IO (VH.Dictionary (PrimState IO) ks Int vs Int)

  testIntersection
    :: VH.Dictionary (PrimState IO) ks Int vs Int
    -> VH.Dictionary (PrimState IO) ks Int vs Int
    -> IO (VH.Dictionary (PrimState IO) ks Int vs Int)

mkSpec
  :: forall ks vs. (HashTableTest ks vs)
  => Proxy ks -> Proxy vs -> Spec
mkSpec ksp vsp = describe (specDescription ksp vsp) $
    modifyMaxSuccess (const 1000) $ do
      it "lookup for inserted value at specific index returns value" $
        property prop_insertLookup

      it "lookup for inserted value at specific index returns nothing" $
        property prop_insertLookupNothing

      it "lookup for inserted value at specific index throws error" $
        property prop_insertLookupError

      it "lookup for updated value at specific index returns updated value" $
        property prop_insertUpdateLookup

      it "lookup for deleted value at specific index returns nothing" $
        property prop_insertDeleteLookupNothing

      it "lookup for deleted value at specific index throws error" $
        property prop_insertDeleteLookupError

      it "table size increases when multiple elements added" $
        property prop_insertMultipleElements

      it "lookup for inserted value with hash collision returns value" $
        property prop_insertLookupHashCollisions

      it "fromList . toList === id" $ property prop_fromListToList

      it "deleted entries are not present in key-value list after deleting from hashtable" $
        property prop_insertDeleteKeysSize

      it "new table is null" $ property prop_newIsNull

      it "non-empty table is not null" $ property prop_fromListIsNotNull

      it "inserted key is table member" $ property prop_isMember

      it "deleted key is not a member" $ property prop_isNotMember

      it "when altering is nothing - key deleted from table" $ property prop_alterDelete

      it "when altering is just a result - key updated with result" $ property prop_alterUpdate

      it "when upserting a new key - key is set to value" $ property prop_upsertInsert

      it "when upserting an existing key - key updated with result" $ property prop_upsertUpdate

      it "intersection + symmetric difference of two tables is equal to union of two tables" $ property prop_union

  where
    prop_insertLookup
      :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_insertLookup (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht x y
      v <- testAt ht x
      v `shouldBe` y

    prop_insertLookupNothing :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_insertLookupNothing (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht (x + 1) y
      v <- testAt' ht x
      v `shouldBe` Nothing

    prop_insertLookupError :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_insertLookupError (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht (x + 1) y
      testAt ht x `shouldThrow` errorCall "KeyNotFoundException!"

    prop_insertUpdateLookup :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_insertUpdateLookup (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht x y
      testInsert ht x (y + 1)
      v <- testAt ht x
      v `shouldBe` (y + 1)

    prop_insertDeleteLookupNothing :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_insertDeleteLookupNothing (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht x y
      testDelete ht x
      v <- testAt' ht x
      v `shouldBe` Nothing

    prop_insertDeleteLookupError :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_insertDeleteLookupError (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht 0 1
      testDelete ht 0
      testAt ht 0 `shouldThrow` errorCall "KeyNotFoundException!"

    prop_insertMultipleElements
      :: HashTableTest ks vs
      => (NonNegative Int) -> Property
    prop_insertMultipleElements (NonNegative n) = forAll (listN n) $ \xs -> do
      ht <- testInit (Proxy @ks) (Proxy @vs) 2
      mapM_ (uncurry (testInsert ht)) xs
      htl <- testLength ht
      htl `shouldBe` (length . Set.toList . Set.fromList) (fst <$> xs)

    prop_insertLookupHashCollisions
      :: HashTableTest ks vs => (AlwaysCollide, Int) -> (AlwaysCollide, Int) -> IO ()
    prop_insertLookupHashCollisions (x1, y1) (x2, y2) = do
      ht <- testInitCollide (Proxy @ks) (Proxy @vs) 10
      let x2' = if x1 /= x2 then x2 else x2 + 1
      testInsertCollide ht x1 y1
      testInsertCollide ht x2' y2
      v <- testAtCollide ht x1
      v `shouldBe` y1

    prop_fromListToList :: NonNegative Int -> Property
    prop_fromListToList (NonNegative n) = forAll (shuffledListN n) $ \(xs, ys) -> do
      ht <- testFromList (Proxy @ks) (Proxy @vs) xs
      xs' <- testToList ht
      L.sort xs' `shouldBe` L.sort ys

    prop_insertDeleteKeysSize :: NonNegative Int -> Property
    prop_insertDeleteKeysSize (NonNegative n) = forAll (listsForRemoveN n) go
      where
        go (insertData, deleteData) = do
          ht <- testInit (Proxy @ks) (Proxy @vs) 2
          mapM_ (uncurry (testInsert ht)) insertData
          mapM_ (testDelete ht) deleteData
          kvs <- testToList ht
          L.length insertData - L.length deleteData `shouldBe` L.length kvs

    prop_newIsNull :: IO ()
    prop_newIsNull = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 2
      result <- testNull ht
      result `shouldBe` True

    prop_fromListIsNotNull :: Positive Int -> Property
    prop_fromListIsNotNull (Positive n) = forAll (listN n) $ \xs -> do
      ht <- testFromList (Proxy @ks) (Proxy @vs) xs
      result <- testNull ht
      result `shouldBe` False

    prop_isMember :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_isMember (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht x y
      v <- testMember ht x
      v `shouldBe` True

    prop_isNotMember :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_isNotMember (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht x y
      testDelete ht x
      v <- testMember ht x
      v `shouldBe` False

    prop_alterDelete :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_alterDelete (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht x y
      testAlter ht (const Nothing) x
      v <- testMember ht x
      v `shouldBe` False

    prop_alterUpdate :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_alterUpdate (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht x y
      testAlter ht (fmap negate) x
      v <- testAt ht x
      v `shouldBe` (negate y)

    prop_upsertInsert :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_upsertInsert (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testUpsert ht (maybe 0 negate) x
      v <- testAt ht x
      v `shouldBe` 0

    prop_upsertUpdate :: HashTableTest ks vs => (Int, Int) -> IO ()
    prop_upsertUpdate (x, y) = do
      ht <- testInit (Proxy @ks) (Proxy @vs) 10
      testInsert ht x y
      testUpsert ht (maybe 0 negate) x
      v <- testAt ht x
      v `shouldBe` (negate y)

    prop_union :: Positive Int -> Property
    prop_union (Positive n) = forAll (twoListsN n) $ \(xs, ys) -> do
      ht1 <- testFromList (Proxy @ks) (Proxy @vs) xs
      ht2 <- testFromList (Proxy @ks) (Proxy @vs) ys
      u1  <- testUnion ht1 ht2
      d1  <- testDifference ht1 ht2
      d2  <- testDifference ht2 ht1
      i   <- testIntersection ht1 ht2

      res <- do
        u2  <- testUnion d1 d2
        testUnion i u2

      resultList <- testToList res
      unionList  <- testToList u1

      Set.fromList resultList `shouldBe` Set.fromList unionList


instance HashTableTest M.MVector M.MVector where
  specDescription _ _ = "Data.Vector.HashTables.Mutable keys and values"
  testInit _ _ n = VH.initialize n
  testInitCollide _ _ n = VH.initialize n
  testInsert = VH.insert
  testAt = VH.at
  testAt' = VH.at'
  testDelete = VH.delete
  testInsertCollide = VH.insert
  testAtCollide = VH.at
  testLength = VH.length
  testFromList _ _ = VH.fromList
  testToList = VH.toList
  testNull = VH.null
  testMember = VH.member
  testAlter = VH.alter
  testUpsert = VH.upsert
  testUnion = VH.union
  testDifference = VH.difference
  testIntersection = VH.intersection

mutableSpec :: Spec
mutableSpec = mkSpec (Proxy :: Proxy M.MVector) (Proxy :: Proxy M.MVector)


instance HashTableTest SM.MVector SM.MVector where
  specDescription _ _ = "Data.Vector.HashTables.Storable.Mutable keys and values"
  testInit _ _ n = VH.initialize n
  testInitCollide _ _ n = VH.initialize n
  testInsert = VH.insert
  testAt = VH.at
  testAt' = VH.at'
  testDelete = VH.delete
  testInsertCollide = VH.insert
  testAtCollide = VH.at
  testLength = VH.length
  testFromList _ _ = VH.fromList
  testToList = VH.toList
  testNull = VH.null
  testMember = VH.member
  testAlter = VH.alter
  testUpsert = VH.upsert
  testUnion = VH.union
  testDifference = VH.difference
  testIntersection = VH.intersection

storableMutableSpec :: Spec
storableMutableSpec = mkSpec (Proxy @SM.MVector) (Proxy @SM.MVector)


instance HashTableTest SM.MVector M.MVector where
  specDescription _ _ = "Data.Vector.HashTables.Mutable keys and Data.Vector.HashTables.Storable.Mutable values"
  testInit _ _ n = VH.initialize n
  testInitCollide _ _ n = VH.initialize n
  testInsert = VH.insert
  testAt = VH.at
  testAt' = VH.at'
  testDelete = VH.delete
  testInsertCollide = VH.insert
  testAtCollide = VH.at
  testLength = VH.length
  testFromList _ _ = VH.fromList
  testToList = VH.toList
  testNull = VH.null
  testMember = VH.member
  testAlter = VH.alter
  testUpsert = VH.upsert
  testUnion = VH.union
  testDifference = VH.difference
  testIntersection = VH.intersection

storableKeysSpec :: Spec
storableKeysSpec = mkSpec (Proxy @SM.MVector) (Proxy @M.MVector)


instance HashTableTest M.MVector UM.MVector where
  specDescription _ _ = "Data.Vector.HashTables.Mutable keys and Data.Vector.HashTables.Unboxed.Mutable values"
  testInit _ _ n = VH.initialize n
  testInitCollide _ _ n = VH.initialize n
  testInsert = VH.insert
  testAt = VH.at
  testAt' = VH.at'
  testDelete = VH.delete
  testInsertCollide = VH.insert
  testAtCollide = VH.at
  testLength = VH.length
  testFromList _ _ = VH.fromList
  testToList = VH.toList
  testNull = VH.null
  testMember = VH.member
  testAlter = VH.alter
  testUpsert = VH.upsert
  testUnion = VH.union
  testDifference = VH.difference
  testIntersection = VH.intersection

unboxedKeysSpec :: Spec
unboxedKeysSpec = mkSpec (Proxy @M.MVector) (Proxy @UM.MVector)
