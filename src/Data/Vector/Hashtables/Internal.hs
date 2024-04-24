{-|
Module      : Data.Vector.Hashtables.Internal
Description : Provides internals of hashtables and set of utilities.
Copyright   : (c) klapaucius, swamp_agr, 2016-2021
License     : BSD3
-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnboxedTuples    #-}
module Data.Vector.Hashtables.Internal where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Hashable
import           Data.Maybe
import           Data.Primitive.MutVar
import           Data.Vector.Generic          (Mutable, Vector)
import qualified Data.Vector.Generic          as VI
import           Data.Vector.Generic.Mutable  (MVector)
import qualified Data.Vector.Generic.Mutable  as V
import qualified Data.Vector.Mutable          as B
import qualified Data.Vector.Storable         as SI
import qualified Data.Vector.Storable.Mutable as S
import qualified Data.Vector.Unboxed          as UI
import qualified Data.Vector.Unboxed.Mutable  as U
import qualified GHC.Exts                     as Exts
import           Prelude                      hiding (length, lookup)

import qualified Data.Primitive.PrimArray as A
import qualified Data.Primitive.PrimArray.Utils as A

import           Data.Vector.Hashtables.Internal.Mask (mask)

-- | Alias for 'MutablePrimArray' @s@ 'Int'.
type IntArray s = A.MutablePrimArray s Int

-- | Single-element mutable array of 'Dictionary_' with primitive state token
-- parameterized with state, keys and values types.
--
-- Different flavors of 'MVector' could be used for keys and values.
-- It's preferable to use "Data.Vector.Unboxed.Mutable"
-- or "Data.Vector.Storable.Mutable" if possible. Otherwise,
-- if you must use boxed vectors, consider employing strict ones from
-- [@strict-containers@](https://hackage.haskell.org/package/strict-containers)
-- to eliminate potential accumulation of thunks.
--
-- ==== Example
--
-- >>> import qualified Data.Vector.Storable.Mutable as VM
-- >>> import qualified Data.Vector.Unboxed.Mutable  as UM
-- >>> import Data.Vector.Hashtables
-- >>> type HashTable k v = Dictionary (PrimState IO) VM.MVector k UM.MVector v
--
newtype Dictionary s ks k vs v = DRef { getDRef :: MutVar s (Dictionary_ s ks k vs v) }

-- | Represents collection of hashtable internal primitive arrays and vectors.
--
-- - hash codes,
--
-- - references to the next element,
--
-- - buckets,
--
-- - keys
--
-- - and values.
--
data Dictionary_ s ks k vs v = Dictionary {
    hashCode,
    next,
    buckets,
    refs :: !(IntArray s),
    key :: !(ks s k),
    value :: !(vs s v),
    remSize :: {-# UNPACK #-} !FastRem
}

getCount, getFreeList, getFreeCount :: Int
getCount = 0
getFreeList = 1
getFreeCount = 2

-- | Represents immutable dictionary as collection of immutable arrays and vectors.
-- See 'unsafeFreeze' and 'unsafeThaw' for conversions from/to mutable dictionary.
data FrozenDictionary ks k vs v = FrozenDictionary {
    fhashCode,
    fnext,
    fbuckets :: !(A.PrimArray Int),
    count, freeList, freeCount :: !Int,
    fkey :: !(ks k),
    fvalue :: !(vs v),
    fremSize :: {-# UNPACK #-} !FastRem
} deriving (Eq, Ord, Show)

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Find dictionary entry by given key in immutable 'FrozenDictionary'.
-- If entry not found @-1@ returned.
findElem :: (Vector ks k, Vector vs v, Hashable k, Eq k)
         => FrozenDictionary ks k vs v -> k -> Int
findElem FrozenDictionary{..} key' = go $ fbuckets !. (hashCode' `fastRem` fremSize) where
    hashCode' = hash key' .&. mask
    go i | i >= 0 =
            if fhashCode !. i == hashCode' && fkey !.~ i == key'
                then i else go $ fnext !. i
         | otherwise = -1
{-# INLINE findElem #-}

-- | Infix version of @unsafeRead@.
(!~) :: (MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> m a
(!~) xs !i = V.unsafeRead xs i
-- Why do we need ! before i?
-- The reason is that V.unsafeRead is essentially V.basicUnsafeRead,
-- which is an opaque class member and, unless V.unsafeRead was
-- already specialised to a specific v, GHC has no clue that i is most certainly
-- to be used eagerly. Bang before i hints this vital for optimizer information.

-- | Infix version of @unsafeIndex@.
(!.~) :: (Vector v a) => v a -> Int -> a
(!.~) xs !i = VI.unsafeIndex xs i

-- | Infix version of @unsafeWrite@.
(<~~) :: (MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> a -> m ()
(<~~) xs !i x = V.unsafeWrite xs i x

-- | Infix version of @readPrimArray@.
(!) :: PrimMonad m => A.MutablePrimArray (PrimState m) Int -> Int -> m Int
(!) = A.readPrimArray 

-- | Infix version of @indexPrimArray@.
(!.) :: A.PrimArray Int -> Int -> Int
(!.) = A.indexPrimArray

-- | Infix version of @writePrimArray@.
(<~) :: PrimMonad m => A.MutablePrimArray (PrimState m) Int -> Int -> Int -> m ()
(<~) = A.writePrimArray

-- | /O(1)/ Dictionary with given capacity.
initialize
    :: (MVector ks k, MVector vs v, PrimMonad m)
    => Int
    -> m (Dictionary (PrimState m) ks k vs v)
initialize capacity = do
    let !remSize = getFastRem capacity
        size = frmPrime remSize
    hashCode <- A.replicate size 0
    next     <- A.replicate size 0
    key      <- V.new size
    value    <- V.new size
    buckets  <- A.replicate size (-1)
    refs     <- A.replicate 3 0
    refs     <~ 1 $ -1
    dr       <- newMutVar Dictionary {..}
    return . DRef $ dr

-- | Create a copy of mutable dictionary.
clone
    :: (MVector ks k, MVector vs v, PrimMonad m)
    => Dictionary (PrimState m) ks k vs v
    -> m (Dictionary (PrimState m) ks k vs v)
clone DRef {..} = do
    Dictionary {..} <- readMutVar getDRef
    hashCode        <- A.clone hashCode
    next            <- A.clone next
    key             <- V.clone key
    value           <- V.clone value
    buckets         <- A.clone buckets
    refs            <- A.clone refs
    dr              <- newMutVar Dictionary {..}
    return . DRef $ dr

-- | /O(1)/ Unsafe convert a mutable dictionary to an immutable one without copying.
-- The mutable dictionary may not be used after this operation.
unsafeFreeze
    :: (VI.Vector ks k, VI.Vector vs v, PrimMonad m)
    => Dictionary (PrimState m) (Mutable ks) k (Mutable vs) v
    -> m (FrozenDictionary ks k vs v)
unsafeFreeze DRef {..} = do
    Dictionary {..} <- readMutVar getDRef
    let fremSize = remSize
    fhashCode       <- A.unsafeFreeze hashCode
    fnext           <- A.unsafeFreeze next
    fbuckets        <- A.unsafeFreeze buckets
    count           <- refs ! getCount
    freeList        <- refs ! getFreeList
    freeCount       <- refs ! getFreeCount
    fkey            <- VI.unsafeFreeze key
    fvalue          <- VI.unsafeFreeze value
    return FrozenDictionary {..}

-- | /O(1)/ Unsafely convert immutable 'FrozenDictionary' to a mutable 'Dictionary' without copying.
-- The immutable dictionary may not be used after this operation.
unsafeThaw
    :: (Vector ks k, Vector vs v, PrimMonad m)
    => FrozenDictionary ks k vs v
    -> m (Dictionary (PrimState m) (Mutable ks) k (Mutable vs) v)
unsafeThaw FrozenDictionary {..} = do
    let remSize = fremSize
    hashCode <- A.unsafeThaw fhashCode
    next     <- A.unsafeThaw fnext
    buckets  <- A.unsafeThaw fbuckets
    refs     <- A.unsafeThaw $ A.primArrayFromListN 3 [count, freeList, freeCount]
    key      <- VI.unsafeThaw fkey
    value    <- VI.unsafeThaw fvalue
    dr       <- newMutVar Dictionary {..}
    return . DRef $ dr

-- | /O(n)/ Retrieve list of keys from 'Dictionary'.
keys :: (Vector ks k, PrimMonad m)
     => Dictionary (PrimState m) (Mutable ks) k vs v -> m (ks k)
keys DRef{..} = do
    Dictionary{..} <- readMutVar getDRef
    hcs <- A.freeze hashCode
    ks <- VI.freeze key
    count <- refs ! getCount
    return . VI.ifilter (\i _ -> hcs !. i >= 0) . VI.take count $ ks

-- | /O(n)/ Retrieve list of values from 'Dictionary'.
values :: (Vector vs v, PrimMonad m)
       => Dictionary (PrimState m) ks k (Mutable vs) v -> m (vs v)
values DRef{..} = do
    Dictionary{..} <- readMutVar getDRef
    hcs <- A.freeze hashCode
    vs <- VI.freeze value
    count <- refs ! getCount
    return . VI.ifilter (\i _ -> hcs !. i >= 0) . VI.take count $ vs

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Find value by given key in 'Dictionary'. Throws an error if value not found.
at :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
   => Dictionary (PrimState m) ks k vs v -> k -> m v
at d k = fromMaybe (error "KeyNotFoundException!") <$!> at' d k
{-# INLINE at #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Find value by given key in 'Dictionary'. Like 'at'' but return 'Nothing' if value not found.
at' :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
    => Dictionary (PrimState m) ks k vs v -> k -> m (Maybe v)
at' d k = do
  d_@Dictionary{..} <- readMutVar . getDRef $ d
  i <- findEntry_ d_ k
  if i >= 0
    then Just <$> value !~ i
    else return Nothing
{-# INLINE at' #-}

atWithOrElse :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
     => Dictionary (PrimState m) ks k vs v
     -> k
     -> (Dictionary (PrimState m) ks k vs v -> Int -> m a)
     -> (Dictionary (PrimState m) ks k vs v -> m a)
     -> m a
atWithOrElse d k onFound onNothing = do
  i <- findEntry d k
  if i >= 0
      then onFound d i
      else onNothing d
{-# INLINE atWithOrElse #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Find dictionary entry by given key. If entry not found @-1@ returned.
findEntry :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
          => Dictionary (PrimState m) ks k vs v -> k -> m Int
findEntry d key' = do
    d_ <- readMutVar . getDRef $ d
    findEntry_ d_ key'
{-# INLINE findEntry #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Same as 'findEntry', but for 'Dictionary_'.
findEntry_ :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
          => Dictionary_ (PrimState m) ks k vs v -> k -> m Int
findEntry_ Dictionary{..} key' = do
    let hashCode' = hash key' .&. mask
        go i | i >= 0 = do
                hc <- hashCode ! i
                if hc == hashCode'
                    then do
                        k <- key !~ i
                        if k == key'
                            then return i
                            else go =<< next ! i
                    else go =<< next ! i
             | otherwise = return $ -1
    go =<< buckets ! (hashCode' `fastRem` remSize)
{-# INLINE findEntry_ #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Insert key and value in dictionary by key's hash.
-- If entry with given key found value will be replaced.
insert :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
       => Dictionary (PrimState m) ks k vs v -> k -> v -> m ()
insert DRef{..} key' value' = do
    d@Dictionary{..} <- readMutVar getDRef
    let
        hashCode' = hash key' .&. mask
        !targetBucket = hashCode' `fastRem` remSize

        go i    | i >= 0 = do
                    hc <- hashCode ! i
                    if hc == hashCode'
                        then do
                            k  <- key !~ i
                            if k == key'
                                then value <~~ i $ value'
                                else go =<< next ! i
                        else go =<< next ! i
                | otherwise = addOrResize

        addOrResize = do
            freeCount <- refs ! getFreeCount
            if freeCount > 0
                then do
                    index <- refs ! getFreeList
                    nxt <- next ! index
                    refs <~ getFreeList $ nxt
                    refs <~ getFreeCount $ freeCount - 1
                    add index targetBucket
                else do
                    count <- refs ! getCount
                    refs <~ getCount $ count + 1
                    nextLen <- A.length next
                    if count == nextLen
                        then do
                            nd <- resize d count hashCode' key' value'
                            writeMutVar getDRef nd
                        else add (fromIntegral count) (fromIntegral targetBucket)

        add !index !targetBucket = do
            hashCode <~ index $ hashCode'
            b <- buckets ! targetBucket
            next <~ index $ b
            key <~~ index $ key'
            value <~~ index $ value'
            buckets <~ targetBucket $ index

    go =<< buckets ! targetBucket
{-# INLINE insert #-}

insertWithIndex
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => Int
  -- ^ Target bucket, key's hash modulo table size
  -> Int
  -- ^ Key's hash
  -> k
  -- ^ Key
  -> v
  -- ^ Value
  -> MutVar (PrimState m) (Dictionary_ (PrimState m) ks k vs v)
  -- ^ MutVar with 'Dictionary_'
  -> Dictionary_ (PrimState m) ks k vs v
  -- ^ 'Dictionary_' itself
  -> Int
  -> m ()
insertWithIndex !targetBucket !hashCode' key' value' getDRef d@Dictionary{..} = go where
  go i
    | i >= 0 = do
         hc <- hashCode ! i
         if hc == hashCode'
             then do
                 k  <- key !~ i
                 if k == key'
                     then value <~~ i $ value'
                     else go =<< next ! i
             else go =<< next ! i
    | otherwise = addOrResize targetBucket hashCode' key' value' getDRef d
{-# INLINE insertWithIndex #-}

addOrResize
    :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
    => Int
    -- ^ Target bucket, key's hash modulo table size
    -> Int
    -- ^ Key's hash
    -> k
    -- ^ Key
    -> v
    -- ^ Value
    -> MutVar (PrimState m) (Dictionary_ (PrimState m) ks k vs v)
    -- ^ MutVar with 'Dictionary_'
    -> Dictionary_ (PrimState m) ks k vs v
    -- ^ 'Dictionary_' itself
    -> m ()
addOrResize !targetBucket !hashCode' !key' !value' dref d@Dictionary{..}  = do
    freeCount <- refs ! getFreeCount
    if freeCount > 0
        then do
            index <- refs ! getFreeList
            nxt <- next ! index
            refs <~ getFreeList $ nxt
            refs <~ getFreeCount $ freeCount - 1
            add index targetBucket hashCode' key' value' d
        else do
            count <- refs ! getCount
            refs <~ getCount $ count + 1
            nextLen <- A.length next
            if count == nextLen
                then do
                    nd <- resize d count hashCode' key' value'
                    writeMutVar dref nd
                else add (fromIntegral count) (fromIntegral targetBucket) hashCode' key' value' d
{-# INLINE addOrResize #-}

add :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
    => Int -> Int -> Int -> k -> v -> Dictionary_ (PrimState m) ks k vs v -> m ()
add !index !targetBucket !hashCode' !key' !value' Dictionary{..} = do
    hashCode <~ index $ hashCode'
    b <- buckets ! targetBucket
    next <~ index $ b
    key <~~ index $ key'
    value <~~ index $ value'
    buckets <~ targetBucket $ index
{-# INLINE add #-}

resize
    :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
    => Dictionary_ (PrimState m) ks k vs v
    -- ^ The original 'Dictionary_'
    -> Int
    --
    -> Int
    -- ^ Key's hash
    -> k
    -- ^ Key
    -> v
    -- ^ Value
    -> m (Dictionary_ (PrimState m) ks k vs v)
resize Dictionary{..} index hashCode' key' value' = do
    let !newRemSize = getFastRem (index*2)
        newSize = frmPrime newRemSize
        delta = newSize - index

    buckets <- A.replicate newSize (-1)

    hashCode <- A.growNoZ hashCode delta
    next <- A.growNoZ next delta
    key <- V.grow key delta
    value <- V.grow value delta

    let go i | i < index = do
                hc <- hashCode ! i
                when (hc >= 0) $ do
                    let !bucket = hc `fastRem` newRemSize
                    nx <- buckets ! bucket
                    next <~ i $ nx
                    buckets <~ bucket $ i
                go (i + 1)
             | otherwise = return ()
    go 0

    let !targetBucket = hashCode' `fastRem` newRemSize
    hashCode <~ index $ hashCode'
    b <- buckets ! targetBucket
    next <~ index $ b
    key <~~ index $ key'
    value <~~ index $ value'
    buckets <~ targetBucket $ index
    let remSize = newRemSize
    return Dictionary{..}

{-# INLINE resize #-}

class DeleteEntry xs where
    deleteEntry :: (MVector xs x, PrimMonad m) => xs (PrimState m) x -> Int -> m ()

instance DeleteEntry S.MVector where
    deleteEntry _ _ = return ()

instance DeleteEntry U.MVector where
    deleteEntry _ _ = return ()

instance DeleteEntry B.MVector where
    deleteEntry v i = v <~~ i $ undefined

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Delete entry from 'Dictionary' by given key.
delete :: (Eq k, MVector ks k, MVector vs v, Hashable k, PrimMonad m, DeleteEntry ks, DeleteEntry vs)
       => Dictionary (PrimState m) ks k vs v -> k -> m ()
delete DRef{..} key' = do
    Dictionary{..} <- readMutVar getDRef
    let hashCode' = hash key' .&. mask
        !bucket = hashCode' `fastRem` remSize
        go !last !i | i >= 0 = do
            hc <- hashCode ! i
            k  <- key !~ i
            if hc == hashCode' && k == key' then do
                nxt <- next ! i
                if last < 0
                    then buckets <~ bucket $ nxt
                    else next <~ last $ nxt
                hashCode <~ i $ -1
                next <~ i =<< refs ! getFreeList
                deleteEntry key i
                deleteEntry value i
                refs <~ getFreeList $ i
                fc <- refs ! getFreeCount
                refs <~ getFreeCount $ fc + 1
            else go i =<< next ! i
            | otherwise = return ()
    go (-1) =<< buckets ! bucket

{-# INLINE delete #-}

deleteWithIndex
  :: (Eq k, MVector ks k, MVector vs v, Hashable k, PrimMonad m, DeleteEntry ks, DeleteEntry vs)
  => Int -> Int -> Dictionary_ (PrimState m) ks k vs v -> k -> Int -> Int -> m ()
deleteWithIndex !bucket !hashCode' d@Dictionary{..} key' !last !i
  | i >= 0 = do
      hc <- hashCode ! i
      k <- key !~ i
      if hc == hashCode' && k == key' then do
          nxt <- next ! i
          if last < 0
              then buckets <~ bucket $ nxt
              else next <~ last $ nxt
          hashCode <~ i $ -1
          next <~ i =<< refs ! getFreeList
          deleteEntry key i
          deleteEntry value i
          refs <~ getFreeList $ i
          fc <- refs ! getFreeCount
          refs <~ getFreeCount $ fc + 1
        else deleteWithIndex bucket hashCode' d key' i =<< next ! i
  | otherwise = return ()
{-# INLINE deleteWithIndex #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Find value by given key in 'Dictionary'. Like 'lookup'' but return 'Nothing' if value not found.
lookup :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
   => Dictionary (PrimState m) ks k vs v -> k -> m (Maybe v)
lookup = at'
{-# INLINE lookup #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Find value by given key in 'Dictionary'. Throws an error if value not found.
lookup' :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
   => Dictionary (PrimState m) ks k vs v -> k -> m v
lookup' = at
{-# INLINE lookup' #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Lookup the index of a key, which is its zero-based index in the sequence sorted by keys.
-- The index is a number from 0 up to, but not including, the size of the dictionary.
lookupIndex :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
   => Dictionary (PrimState m) ks k vs v -> k -> m (Maybe Int)
lookupIndex ht k = do
    let safeIndex i | i < 0 = Nothing
                    | otherwise = Just i
    return . safeIndex =<< findEntry ht k
{-# INLINE lookupIndex #-}

-- | /O(1)/ Return 'True' if dictionary is empty, 'False' otherwise.
null
  :: (MVector ks k, PrimMonad m)
  => Dictionary (PrimState m) ks k vs v -> m Bool
null ht = return . (== 0) =<< length ht
{-# INLINE null #-}

-- | /O(1)/ Return the number of non-empty entries of dictionary.
length
  :: (MVector ks k, PrimMonad m)
  => Dictionary (PrimState m) ks k vs v -> m Int
length DRef {..} = do
    Dictionary {..} <- readMutVar getDRef
    count           <- refs ! getCount
    freeCount       <- refs ! getFreeCount
    return (count - freeCount)
{-# INLINE length #-}

-- | /O(1)/ Return the number of non-empty entries of dictionary. Synonym of 'length'.
size
  :: (MVector ks k, PrimMonad m)
  => Dictionary (PrimState m) ks k vs v -> m Int
size = length
{-# INLINE size #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- Return 'True' if the specified key is present in the dictionary, 'False' otherwise.
member
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => Dictionary (PrimState m) ks k vs v -> k -> m Bool
member ht k = return . (>= 0) =<< findEntry ht k
{-# INLINE member #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- The expression @'findWithDefault' ht def k@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the dictionary.
findWithDefault
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => Dictionary (PrimState m) ks k vs v -> v -> k -> m v
findWithDefault ht v k = return . fromMaybe v =<< at' ht k
{-# INLINE findWithDefault #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- The expression (@'upsert' ht f k@) updates or inserts the value @x@ at @k@.
--
-- It's a responsibility of 'MVector' @vs@ to force evaluation of the updated value.
-- Unboxed / storable vectors do it automatically. If you use boxed vectors,
-- consider employing strict ones from
-- [@strict-containers@](https://hackage.haskell.org/package/strict-containers)
-- to eliminate potential accumulation of thunks.
--
-- > let f _ = "c"
-- > ht <- fromList [(5,"a"), (3,"b")]
-- > upsert ht f 7
-- > toList ht
-- > [(3, "b"), (5, "a"), (7, "c")]
--
-- > ht <- fromList [(5,"a"), (3,"b")]
-- > upsert ht f 5
-- > toList ht
-- > [(3, "b"), (5, "c")]
--
upsert
  :: ( MVector ks k, MVector vs v
     , PrimMonad m, Hashable k, Eq k
     )
  => Dictionary (PrimState m) ks k vs v -> (Maybe v -> v) -> k -> m ()
upsert ht f k = do
  d@Dictionary{..} <- readMutVar . getDRef $ ht
  let
      hashCode' = hash k .&. mask
      !targetBucket = hashCode' `fastRem` remSize

      onFound' value' dict i = insertWithIndex targetBucket hashCode' k value' (getDRef ht) dict i

      onFound dict i = do
        d'@Dictionary{..} <- readMutVar . getDRef $ dict
        v <- value !~ i
        onFound' (f (Just v)) d' i

      onNothing dict = do
        d' <- readMutVar . getDRef $ dict
        onFound' (f Nothing) d' (-1)

  void $ atWithOrElse ht k onFound onNothing

{-# INLINE upsert #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- The expression (@'alter' ht f k@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Dictionary'.
--
-- It's a responsibility of 'MVector' @vs@ to force evaluation of the updated value.
-- Unboxed / storable vectors do it automatically. If you use boxed vectors,
-- consider employing strict ones from
-- [@strict-containers@](https://hackage.haskell.org/package/strict-containers)
-- to eliminate potential accumulation of thunks.
--
-- > let f _ = Nothing
-- > ht <- fromList [(5,"a"), (3,"b")]
-- > alter ht f 7
-- > toList ht
-- > [(3, "b"), (5, "a")]
--
-- > ht <- fromList [(5,"a"), (3,"b")]
-- > alter ht f 5
-- > toList ht
-- > [(3 "b")]
-- 
-- > let f _ = Just "c"
-- > ht <- fromList [(5,"a"), (3,"b")]
-- > alter ht f 7
-- > toList ht
-- > [(3, "b"), (5, "a"), (7, "c")]
-- 
-- > ht <- fromList [(5,"a"), (3,"b")]
-- > alter ht f 5
-- > toList ht
-- > [(3, "b"), (5, "c")]
-- 
alter
  :: ( MVector ks k, MVector vs v, DeleteEntry ks, DeleteEntry vs
     , PrimMonad m, Hashable k, Eq k
     )
  => Dictionary (PrimState m) ks k vs v -> (Maybe v -> Maybe v) -> k -> m ()
alter ht f k = do
  d@Dictionary{..} <- readMutVar . getDRef $ ht
  let
      hashCode' = hash k .&. mask
      !targetBucket = hashCode' `fastRem` remSize

      onFound' value' dict i = insertWithIndex targetBucket hashCode' k value' (getDRef ht) dict i
      onNothing' dict i = deleteWithIndex targetBucket hashCode' d k (-1) i

      onFound dict i = do
        d'@Dictionary{..} <- readMutVar . getDRef $ dict
        v <- value !~ i
        case f (Just v) of
          Nothing -> onNothing' d' i
          Just v' ->  onFound' v' d' i

      onNothing dict = do
        d' <- readMutVar . getDRef $ dict
        case f Nothing of
          Nothing -> return ()
          Just v' -> onFound' v' d' (-1)

  void $ atWithOrElse ht k onFound onNothing

{-# INLINE alter #-}

-- | /O(1)/ in the best case, /O(n)/ in the worst case.
-- The expression (@'alterM' ht f k@) alters the value @x@ at @k@, or absence thereof.
-- 'alterM' can be used to insert, delete, or update a value in a 'Dictionary' in the same @'PrimMonad' m@.
alterM
  :: ( MVector ks k, MVector vs v, DeleteEntry ks, DeleteEntry vs
     , PrimMonad m, Hashable k, Eq k
     )
  => Dictionary (PrimState m) ks k vs v -> (Maybe v -> m (Maybe v)) -> k -> m ()
alterM ht f k = do
  d@Dictionary{..} <- readMutVar . getDRef $ ht
  let
      hashCode' = hash k .&. mask
      !targetBucket = hashCode' `fastRem` remSize

      onFound' value' dict i = insertWithIndex targetBucket hashCode' k value' (getDRef ht) dict i
      onNothing' dict i = deleteWithIndex targetBucket hashCode' d k (-1) i

      onFound dict i = do
        d'@Dictionary{..} <- readMutVar . getDRef $ dict
        v <- value !~ i
        res <- f (Just v)
        case res of
          Nothing -> onNothing' d' i
          Just v' ->  onFound' v' d' i

      onNothing dict = do
        d' <- readMutVar . getDRef $ dict
        res <- f Nothing
        case res of
          Nothing -> return ()
          Just v' -> onFound' v' d' (-1)

  void $ atWithOrElse ht k onFound onNothing

{-# INLINE alterM #-}

-- * Combine

-- | /O(min n m)/ in the best case, /O(min n m * max n m)/ in the worst case.
-- The union of two maps.
-- If a key occurs in both maps,
-- the mapping from the first will be the mapping in the result.
union
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => Dictionary (PrimState m) ks k vs v
  -> Dictionary (PrimState m) ks k vs v
  -> m (Dictionary (PrimState m) ks k vs v)
union = unionWith const

{-# INLINE union #-}

-- | /O(min n m)/ in the best case, /O(min n m * max n m)/ in the worst case.
-- The union of two maps.
-- The provided function (first argument) will be used to compute the result.
unionWith
  :: (MVector ks k, MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => (v -> v -> v)
  -> Dictionary (PrimState m) ks k vs v
  -> Dictionary (PrimState m) ks k vs v
  -> m (Dictionary (PrimState m) ks k vs v)
unionWith f = unionWithKey (const f)

{-# INLINE unionWith #-}

-- | /O(min n m)/ in the best case, /O(min n m * max n m)/ in the worst case.
-- The union of two maps.
-- If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
unionWithKey
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => (k -> v -> v -> v)
  -> Dictionary (PrimState m) ks k vs v
  -> Dictionary (PrimState m) ks k vs v
  -> m (Dictionary (PrimState m) ks k vs v)
unionWithKey f t1 t2 = do
  l1 <- length t1
  l2 <- length t2
  let smallest = min l1 l2
      greatest = max l1 l2
      g k v1 v2 = if smallest == l1 then f k v1 v2 else f k v2 v1
      (tS, tG) = if smallest == l1 then (t1, t2) else (t2, t1)
  ht <- clone tG
  dictG <- readMutVar . getDRef $ tG
  dictS <- readMutVar . getDRef $ tS
  hcsS <- A.freeze . hashCode $ dictS
  let indices = catMaybes . zipWith collect [0 ..] . take smallest . A.primArrayToList $ hcsS
      collect i _ | hcsS !. i >= 0 = Just i
                  | otherwise       = Nothing

      go !i = do
        k <- key dictS !~ i
        v <- value dictS !~ i
        let
           hashCode' = hash k .&. mask
           !targetBucket = hashCode' `fastRem` remSize dictG

           onFound dict i = do
             d@Dictionary{..} <- readMutVar . getDRef $ dict
             vG <- value !~ i
             insertWithIndex targetBucket hashCode' k (g k v vG) (getDRef dict) d i

           onNothing dict = do
             d@Dictionary{..} <- readMutVar . getDRef $ dict
             insertWithIndex targetBucket hashCode' k v (getDRef dict) d
               =<< buckets ! targetBucket

        void $ atWithOrElse ht k onFound onNothing
  mapM_ go indices
  return ht

{-# INLINE unionWithKey #-}

-- * Difference and intersection

-- | /O(n)/ in the best case, /O(n * m)/ in the worst case.
-- Difference of two tables. Return elements of the first table
-- not existing in the second.
difference
  :: (MVector ks k, MVector vs v, MVector vs w, PrimMonad m, Hashable k, Eq k)
  => Dictionary (PrimState m) ks k vs v
  -> Dictionary (PrimState m) ks k vs w
  -> m (Dictionary (PrimState m) ks k vs v)
difference a b = do
  kvs <- toList a
  ht <- initialize 10
  mapM_ (go ht) kvs
  return ht
  where
    go ht (!k, !v) = do
      mv <- lookup b k
      case mv of
        Nothing -> insert ht k v
        _       -> return ()
{-# INLINE difference #-}

-- | /O(n)/ in the best case, /O(n * m)/ in the worst case.
-- Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWith
  :: (MVector ks k, MVector vs v, MVector vs w, PrimMonad m, Hashable k, Eq k)
  => (v -> w -> Maybe v)
  -> Dictionary (PrimState m) ks k vs v
  -> Dictionary (PrimState m) ks k vs w
  -> m (Dictionary (PrimState m) ks k vs v)
differenceWith f a b = do
  kvs <- toList a
  ht <- initialize 10
  mapM_ (go ht) kvs
  return ht
  where
    go ht (!k, !v) = do
      mv <- lookup b k
      case mv of
        Nothing -> insert ht k v
        Just w  -> maybe (return ()) (insert ht k) (f v w)
{-# INLINE differenceWith #-}

-- | /O(n)/ in the best case, /O(n * m)/ in the worst case.
-- Intersection of two maps. Return elements of the first
-- map for keys existing in the second.
intersection
  :: (MVector ks k, MVector vs v, MVector vs w, PrimMonad m, Hashable k, Eq k)
  => Dictionary (PrimState m) ks k vs v
  -> Dictionary (PrimState m) ks k vs w
  -> m (Dictionary (PrimState m) ks k vs v)
intersection a b = do
  kvs <- toList a
  ht <- initialize 10
  mapM_ (go ht) kvs
  return ht
  where
    go ht (!k, !v) = do
      mv <- lookup b k
      case mv of
        Nothing -> return ()
        Just _  -> insert ht k v
{-# INLINE intersection #-}

-- | Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWith
  :: (MVector ks k, MVector vs v1, MVector vs v2, MVector vs v3, PrimMonad m, Hashable k, Eq k)
  => (v1 -> v2 -> v3)
  -> Dictionary (PrimState m) ks k vs v1
  -> Dictionary (PrimState m) ks k vs v2
  -> m (Dictionary (PrimState m) ks k vs v3)
intersectionWith f a b = do
  kvs <- toList a
  ht <- initialize 10
  mapM_ (go ht) kvs
  return ht
  where
    go ht (!k, !v) = do
      mv <- lookup b k
      case mv of
        Nothing -> return ()
        Just w  -> insert ht k (f v w)
{-# INLINE intersectionWith #-}

-- | Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWithKey
  :: (MVector ks k, MVector vs v1, MVector vs v2, MVector vs v3, PrimMonad m, Hashable k, Eq k)
  => (k -> v1 -> v2 -> v3)
  -> Dictionary (PrimState m) ks k vs v1
  -> Dictionary (PrimState m) ks k vs v2
  -> m (Dictionary (PrimState m) ks k vs v3)
intersectionWithKey f a b = do
  kvs <- toList a
  ht <- initialize 10
  mapM_ (go ht) kvs
  return ht
  where
    go ht (!k, !v) = do
      mv <- lookup b k
      case mv of
        Nothing -> return ()
        Just w  -> insert ht k (f k v w)
{-# INLINE intersectionWithKey #-}

-- * List conversions

-- | /O(n)/ Convert list to a 'Dictionary'. 
fromList
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => [(k, v)] -> m (Dictionary (PrimState m) ks k vs v)
fromList kv = do
    ht <- initialize 1
    mapM_ (uncurry (insert ht)) kv
    return ht

{-# INLINE fromList #-}

-- | /O(n)/ Convert 'Dictionary' to a list.
toList
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => (Dictionary (PrimState m) ks k vs v) -> m [(k, v)]
toList DRef {..} = do
    Dictionary {..} <- readMutVar getDRef
    hcs <- A.freeze hashCode
    count <- refs ! getCount
    let go !i xs
          | i < 0 = return xs
          | hcs !. i < 0 = go (i - 1) xs
          | otherwise = do
              k <- key !~ i
              v <- value !~ i
              go (i - 1) ((k, v) : xs)
        {-# INLINE go #-}
    go (count - 1) []
{-# INLINE toList #-}

-- * Extras

-- | This data is auto-generated by GenPrimes.hs.
-- The vector contains tuples (p, m, s) such that p is prime
-- and (assuming 64-bit architecture) for every n >= 0
-- it holds that n \`'quot'\` p = (n * m) \`'shiftR'\` (64 + s),
-- enabling faster computation of remainders.
primesWithFastRem :: UI.Vector (Int, Int, Int)
primesWithFastRem = UI.fromList $
  if finiteBitSize (0 :: Int) == 32
  then [(5,1717986919,1),(11,780903145,1),(17,2021161081,3),(41,1676084799,4),(53,1296593901,4),(67,128207979,1),(83,827945503,4),(101,680390859,4),(131,1049152317,5),(163,210795941,3),(197,1395319325,6),(241,285143057,4),(311,883851791,6),(379,1450543045,7),(457,601483385,6),(557,123374285,4),(673,1633746847,8),(809,84943729,4),(977,562697865,7),(1187,1852589095,9),(1427,24078303,3),(1721,638879505,8),(2069,2125687053,10),(2503,1757110073,10),(3041,723125043,9),(3673,299349749,8),(4409,1995031305,11),(5297,103786259,7),(6367,1381512961,11),(7649,287491601,9),(9181,1916151405,12),(11047,1592485385,12),(13291,330904109,10),(15959,1102336365,12),(19157,57394771,8),(22993,382555257,11),(27611,637144111,12),(33149,1061400709,13),(39779,1768992287,14),(47741,736984397,13),(57301,1228054383,14),(68767,511646169,13),(82529,426327377,13),(99041,177625287,12),(118861,1184051021,15),(142657,1973089135,16),(171203,1644100727,16),(205477,684930617,15),(246577,285382433,14),(295901,951247129,16),(355087,792692993,16),(426131,1321072519,17),(511361,1100885585,17),(613637,917398973,17),(736369,1528988737,18),(883661,637065519,17),(1060421,2123496059,19),(1272539,1769533047,19),(1527061,184324645,16),(1832497,1228815007,19),(2199061,2047964849,20),(2638879,1706633623,20),(3166661,1422191901,20),(3800051,1185141891,20),(4560071,123452015,17),(5472109,411504927,19),(6566551,685839435,20),(7879897,285765133,19),(9455881,238137495,19),(11347079,793790125,21),(13616503,1322982745,22),(16339877,275620167,20),(19607893,229682997,20),(23529511,191402177,20),(28235483,1276011359,23),(33882593,2126684757,24),(40659149,1772235667,24),(48791009,738431071,23),(58549219,1230718279,24),(70259107,1025597921,24),(84310943,427332393,23),(101173139,712220603,24),(121407857,74189591,21),(145689433,1978389031,26),(174827333,206082175,23),(209792827,1373880987,26),(251751509,286225073,24),(302101841,1908166963,27),(362522213,1590139119,27),(435026701,662557897,26),(522032051,1104263141,27),(626438489,1840438487,28),(751726211,766849345,27),(902071483,1278082199,28)]
  else [(5,7378697629483820647,1),(7,5270498306774157605,1),(11,3353953467947191203,1),(17,8680820740569200761,3),(29,5088756985850910791,3),(37,3988485205126389539,3),(47,6279742663390485657,4),(67,8810385229234412713,5),(83,3555998857582564167,4),(107,1379195818595106663,3),(131,281629680514649643,1),(163,7242893378634425175,6),(197,1498212716646461045,4),(241,1224680104478642431,4),(293,503665367200260795,3),(353,1672226091667721393,5),(433,681634884940768651,4),(521,9064043153300662599,8),(631,7483940543375032035,8),(761,3102737505170594753,7),(919,642324059149842929,5),(1103,4281383937325154319,8),(1327,7117357170866081709,9),(1597,739255867700320165,6),(1949,2422968949650921095,8),(2339,4037936282915472607,9),(2833,6667654758728761333,10),(3407,5544310517017487777,10),(4093,4615066193862345677,10),(4919,7680205705012637063,11),(5903,6399954576140464461,11),(7103,2659364484228999135,10),(8527,8861013688977873041,12),(10243,7376536534795892163,12),(12301,6142416366629893783,12),(14767,2558334926725615339,11),(17729,4261823212020662385,12),(21277,887788030806907969,10),(25561,1477991153043979567,11),(30677,4926026907840683471,13),(36821,4104063644437376683,13),(44201,1709415255897249461,12),(53051,5696998264003643545,14),(63667,4747066060968119963,14),(76403,7911507529904775825,15),(91691,6592390854143968191,15),(110039,5493169783506889261,15),(132047,9155269105808001505,16),(158507,3813477700084630883,15),(190243,6354640221267690137,16),(228299,5295361870243098633,16),(273967,8825338961368552963,17),(328777,3677038903617434233,16),(394549,6128140330426026551,17),(473471,5106652021410515849,17),(568171,8510999819523553119,18),(681809,3546230160102401625,17),(818173,5910367707634591583,18),(981809,4925299399841024781,18),(1178173,8208817004732779819,19),(1413827,1710146743009758867,17),(1696601,5700460247823167261,19),(2035927,4750370006840634953,19),(2443151,7917158257444614269,20),(2931793,6597605326786054403,20),(3518209,5497914738389352877,20),(4221851,9163190796564855935,21),(5066231,7635977559583866901,21),(6079481,3181655327787695495,20),(7295381,1325689029389559421,19),(8754461,8837923026367501915,22),(10505377,7364919169996114103,22),(12606463,1534356870268374785,20),(15127831,5114497409135273073,22),(18153427,8524148355606494265,23),(21784129,7103451550010217731,23),(26140973,5919538837007808943,23),(31369243,2466468586931991543,22),(37643093,8221561650668425911,24),(45171733,856412266221181587,21),(54206099,5709413064779759723,24),(65047343,4757842450557051481,24),(78056833,247804226362015825,20),(93668203,6608112463123586747,25),(112401881,1376689638411589699,23),(134882263,9177930528088635901,26),(161858731,7648274712381010049,26),(194230481,3186781067811339753,25),(233076601,663912654667005953,23),(279691949,4426083924515754563,26),(335630353,7376806228758340429,27),(402756463,48026077520285669,20),(483307787,5122781269342057073,27),(579969349,4268984357259129137,27),(695963227,7114973844934943811,28),(835155913,5929144582541584783,28),(1002187163,617619185811393333,25),(1202624651,8234922098136039455,29),(1443149623,6862434883013541971,29),(1731779563,357418480311913773,25),(2078135531,4765579610448921293,29),(2493762643,7942632665608538591,30),(2992515199,3309430247035160405,29),(3591018241,5515717075012787271,30),(4309221899,9192861770781641709,31),(5171066297,7660718115355497017,31),(6205279567,6383931751891069761,31),(7446335483,1329985781179588333,29),(8935602619,2216642958858742015,30),(10722723161,3694404925160612267,31),(12867267797,6157341540115949263,32),(15440721377,1282779485812755651,30),(18528865703,8551863215397642261,33),(22234638851,890819084640777303,30),(26681566631,2969396947711947027,32),(32017880003,309312181610871977,29),(38421456013,4124162420469296211,33),(46105747229,6873604032117775405,34),(55326896741,1432000838311112129,32),(66392276177,1193334030347810553,32),(79670731433,3977780100130856927,34),(95604877727,6629633499704948593,35),(114725853301,172646705678443951,30),(137671023989,575489018811937629,32),(165205228889,3836593456372840199,35),(198246274687,6394322426636527325,36),(237895529659,5328602021422103605,36),(285474635629,4440501683924226201,36),(342569562761,7400836139739766199,37),(411083475323,3083681724818056003,36),(493300170481,1284867385097583665,35),(591960204599,8565782567001774073,38),(710352245527,7138152139085745311,38),(852422694637,5948460115872687585,38),(1022907233639,4957050096199058351,38),(1227488680427,4130875079963290535,38),(1472986416527,3442395899935288315,38),(1767583699907,5737326499650006151,39),(2121100439917,2390552708155269385,38),(2545320527903,996063628397011449,37),(3054384633659,6640424188932079035,40),(3665261560423,5533686824061451401,40),(4398313872521,9222811373407653915,41),(5277976647059,7685676144457159429,41),(6333571976483,6404730120368629123,41),(7600286371789,2668637550150294909,40),(9120343646191,8895458500457872893,42),(10944412375433,7412882083712320257,42),(13133294850551,6177401736412164183,42),(15759953820697,5147834780331776433,42),(18911944584839,1072465579235639315,40),(22694333501813,7149770528235642145,43),(27233200202177,2979071053431364413,42),(32679840242663,1241279605594479897,41),(39215808291301,8275197370607624801,44),(47058969949679,3447998904411212491,43),(56470763939783,1436666210167059381,42),(67764916727749,1197221841805716745,42),(81317900073323,7981478945369069699,45),(97581480088031,3325616227235633285,44),(117097776105689,5542693712056936913,45),(140517331326899,4618911426711740825,45),(168620797592327,3849092855592017097,45),(202344957110837,3207577379659307247,45),(242813948533111,5345962299429831765,46),(291376738239791,8909937165714618823,47),(349652085887761,3712473819047632555,46),(419582503065331,1546864091269781275,45),(503499003678427,5156213637565632409,47),(604198804414123,8593689395942569915,48),(725038565296949,7161407829952127767,48),(870046278356531,745979982286515183,45),(1044055534027841,4973199881910083119,48),(1252866640833481,259020827182801985,44),(1503439969000181,6907222058208035475,49),(1804127962800257,2878009190919951291,48),(2164953555360361,299792624054154309,45),(2597944266432433,999308746847181107,47),(3117533119718951,1665514578078618403,48),(3741039743662841,693964407532739155,47),(4489247692395509,4626429383551491517,50),(5387097230874631,7710715639252456949,51),(6464516677049609,1606399091510915659,49),(7757420012459563,5354663638369696637,51),(9308904014951479,2231109849320706117,50),(11170684817941799,7437032831069004279,52),(13404821781530213,3098763679612072587,51),(16085786137836413,5164606132686737109,52),(19302943365403697,8607676887811227891,53),(23163532038484451,7173064073176018721,53),(27796238446181363,2988776697156672123,52),(33355486135417657,4981294495261117009,53),(40026583362501191,8302157492101861143,54),(48031900035001457,1729616144187886737,52),(57638280042001759,2882693573646477365,53),(69165936050402159,1201122322352698065,52),(82999123260482599,8007482149017986309,55),(99598947912579133,6672901790848320973,55),(119518737495095009,347546968273349907,51),(143422484994114059,4633959576977997203,55),(172106981992936889,7723265961629994521,56),(206528378391524347,6436054968024992935,56),(247834054069829309,2681689570010412721,55),(297400864883795143,8938965233368043239,57),(356881037860554209,7449137694473368585,57),(428257245432665101,6207614745394473093,57),(513908694519198103,5173012287828727761,57),(616690433423037709,8621687146381213139,58),(740028520107645211,898092411081376417,55),(888034224129174191,2993641370271254933,57),(1065641068955008969,2494701141892712585,57),(1278769282746010901,4157835236487853859,58),(1534523139295213087,216553918567075721,54),(1841427767154255641,1443692790447171523,57),(2209713320585106689,2406154650745285959,58),(2651655984702128147,8020515502484286167,60),(3181987181642553871,1670940729684226235,58),(3818384617971064327,5569802432280754581,60),(4582061541565277261,1160375506725157187,58)]

getFastRem :: Int -> FastRem
getFastRem n =
  (\(p, m, s) -> FastRem p m s) $ fromJust $
    UI.find (\(p, _, _) -> p >= n) primesWithFastRem

-- | For 64-bit architectures
-- 'frmPrime' is a prime number such that for each @n@ >= 0
-- it holds that @n@ \`'quot'\` 'frmPrime' = (n * '_frmMulHi') \`'shiftR'\` (64 + s).
data FastRem = FastRem
  { frmPrime :: !Int
  , _frmMulHi :: !Int
  , _frmShift :: !Int
  } deriving (Eq, Ord, Show)

fastRem :: Int -> FastRem -> Int
#ifndef aarch64_HOST_ARCH
fastRem !i (FastRem !p !m !s) = i - p * q
  where
    q = fromIntegral (mulHi (fromIntegral i) (fromIntegral m)) `unsafeShiftR` s
    mulHi (Exts.W# x) (Exts.W# y) =
      let (# z, _ #) = Exts.timesWord2# x y in Exts.W# z
#else
-- At the moment GHC NCG does not make use of UMULH instruction,
-- so timesWord2# is painfully slow on ARM. This is being worked on
-- at https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10832,
-- but in the meantime we should resort to a usual division.
fastRem !i (FastRem !p _ _) = i `rem` p
#endif
