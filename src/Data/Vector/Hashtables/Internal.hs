{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
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

type IntArray s = S.MVector s Int

newtype Dictionary s ks k vs v = DRef { getDRef :: MutVar s (Dictionary_ s ks k vs v) }

data FrozenDictionary ks k vs v = FrozenDictionary {
    fhashCode,
    fnext,
    fbuckets :: SI.Vector Int,
    count, freeList, freeCount :: Int,
    fkey :: ks k,
    fvalue :: vs v
} deriving (Eq, Ord, Read, Show)

findElem :: (Vector ks k, Vector vs v, Hashable k, Eq k)
         => FrozenDictionary ks k vs v -> k -> Int
findElem FrozenDictionary{..} key' = go $ fbuckets !. (hashCode' `rem` VI.length fbuckets) where
    hashCode' = hash key' .&. 0x7FFFFFFFFFFFFFFF
    go i | i >= 0 =
            if fhashCode !. i == hashCode' && fkey !. i == key'
                then i else go $ fnext !. i
         | otherwise = -1
{-# INLINE findElem #-}

data Dictionary_ s ks k vs v = Dictionary {
    hashCode,
    next,
    buckets,
    refs :: IntArray s,
    key :: ks s k,
    value :: vs s v
}

getCount, getFreeList, getFreeCount :: Int
getCount = 0
getFreeList = 1
getFreeCount = 2

(!) :: (MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> m a
(!) = V.unsafeRead

(!.) :: (Vector v a) => v a -> Int -> a
(!.) = VI.unsafeIndex

(<~) :: (MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> a -> m ()
(<~) = V.unsafeWrite

initialize
    :: (MVector ks k, MVector vs v, PrimMonad m)
    => Int
    -> m (Dictionary (PrimState m) ks k vs v)
initialize capacity = do
    let size = getPrime capacity
    hashCode <- V.replicate size 0
    next     <- V.replicate size 0
    key      <- V.new size
    value    <- V.new size
    buckets  <- V.replicate size (-1)
    refs     <- V.replicate 3 0
    refs     <~ 1 $ -1
    dr       <- newMutVar Dictionary {..}
    return . DRef $ dr

clone
    :: (MVector ks k, MVector vs v, PrimMonad m)
    => Dictionary (PrimState m) ks k vs v
    -> m (Dictionary (PrimState m) ks k vs v)
clone DRef {..} = do
    Dictionary {..} <- readMutVar getDRef
    hashCode        <- V.clone hashCode
    next            <- V.clone next
    key             <- V.clone key
    value           <- V.clone value
    buckets         <- V.clone buckets
    refs            <- V.clone refs
    dr              <- newMutVar Dictionary {..}
    return . DRef $ dr


unsafeFreeze
    :: (VI.Vector ks k, VI.Vector vs v, PrimMonad m)
    => Dictionary (PrimState m) (Mutable ks) k (Mutable vs) v
    -> m (FrozenDictionary ks k vs v)
unsafeFreeze DRef {..} = do
    Dictionary {..} <- readMutVar getDRef
    fhashCode       <- VI.unsafeFreeze hashCode
    fnext           <- VI.unsafeFreeze next
    fbuckets        <- VI.unsafeFreeze buckets
    count           <- refs ! getCount
    freeList        <- refs ! getFreeList
    freeCount       <- refs ! getFreeCount
    fkey            <- VI.unsafeFreeze key
    fvalue          <- VI.unsafeFreeze value
    return FrozenDictionary {..}


unsafeThaw
    :: (Vector ks k, Vector vs v, PrimMonad m)
    => FrozenDictionary ks k vs v
    -> m (Dictionary (PrimState m) (Mutable ks) k (Mutable vs) v)
unsafeThaw FrozenDictionary {..} = do
    hashCode <- VI.unsafeThaw fhashCode
    next     <- VI.unsafeThaw fnext
    buckets  <- VI.unsafeThaw fbuckets
    refs     <- VI.unsafeThaw $ SI.fromListN 3 [count, freeList, freeCount]
    key      <- VI.unsafeThaw fkey
    value    <- VI.unsafeThaw fvalue
    dr       <- newMutVar Dictionary {..}
    return . DRef $ dr


values :: (Vector vs v, PrimMonad m)
       => Dictionary (PrimState m) ks k (Mutable vs) v -> m (vs v)
values DRef{..} = do
    Dictionary{..} <- readMutVar getDRef
    hcs <- SI.freeze hashCode
    vs <- VI.freeze value
    count <- refs ! getCount
    return . VI.ifilter (\i _ -> hcs VI.! i >= 0) . VI.take count $ vs

keys :: (Vector ks k, PrimMonad m)
     => Dictionary (PrimState m) (Mutable ks) k vs v -> m (ks k)
keys DRef{..} = do
    Dictionary{..} <- readMutVar getDRef
    hcs <- SI.freeze hashCode
    ks <- VI.freeze key
    count <- refs ! getCount
    return . VI.ifilter (\i _ -> hcs VI.! i >= 0) . VI.take count $ ks

at :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
   => Dictionary (PrimState m) ks k vs v -> k -> m v
at d k = do
    i <- findEntry d k
    if i >= 0
        then do
            Dictionary{..} <- readMutVar . getDRef $ d
            value ! i
        else error "KeyNotFoundException!"
{-# INLINE at #-}

at' :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
    => Dictionary (PrimState m) ks k vs v -> k -> m (Maybe v)
at' d k = do
  i <- findEntry d k
  if i >= 0
      then do
          Dictionary{..} <- readMutVar . getDRef $ d
          Just <$> value ! i
      else pure Nothing
{-# INLINE at' #-}

findEntry :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
          => Dictionary (PrimState m) ks k vs v -> k -> m Int
findEntry d key' = do
    Dictionary{..} <- readMutVar . getDRef $ d
    let hashCode' = hash key' .&. 0x7FFFFFFFFFFFFFFF
        go i | i >= 0 = do
                hc <- hashCode ! i
                if hc == hashCode'
                    then do
                        k <- key ! i
                        if k == key'
                            then return i
                            else go =<< next ! i
                    else go =<< next ! i
             | otherwise = return $ -1
    go =<< buckets ! (hashCode' `rem` V.length buckets)
{-# INLINE findEntry #-}

insert :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
       => Dictionary (PrimState m) ks k vs v -> k -> v -> m ()
insert DRef{..} key' value' = do
    d@Dictionary{..} <- readMutVar getDRef
    let
        hashCode' = hash key' .&. 0x7FFFFFFFFFFFFFFF
        targetBucket = hashCode' `rem` V.length buckets

        go i    | i >= 0 = do
                    hc <- hashCode ! i
                    if hc == hashCode'
                        then do
                            k  <- key ! i
                            if k == key'
                                then value <~ i $ value'
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
                    if count == V.length next
                        then do
                            nd <- resize d count hashCode' key' value'
                            writeMutVar getDRef nd
                        else add (fromIntegral count) (fromIntegral targetBucket)

        add !index !targetBucket = do
            hashCode <~ index $ hashCode'
            b <- buckets ! targetBucket
            next <~ index $ b
            key <~ index $ key'
            value <~ index $ value'
            buckets <~ targetBucket $ index

    go =<< buckets ! targetBucket

{-# INLINE insert #-}

resize Dictionary{..} index hashCode' key' value' = do
    let newSize = getPrime (index*2)
        delta = newSize - index

    buckets <- V.replicate newSize (-1)

    hashCode <- V.grow hashCode delta
    next <- V.grow next delta
    key <- V.grow key delta
    value <- V.grow value delta

    let go i | i < index = do
                hc <- hashCode ! i
                when (hc >= 0) $ do
                    let bucket = hc `rem` newSize
                    nx <- buckets ! bucket
                    next <~ i $ nx
                    buckets <~ bucket $ i
                go (i + 1)
             | otherwise = return ()
    go 0

    let targetBucket = hashCode' `rem` V.length buckets
    hashCode <~ index $ hashCode'
    b <- buckets ! targetBucket
    next <~ index $ b
    key <~ index $ key'
    value <~ index $ value'
    buckets <~ targetBucket $ index
    return Dictionary{..}

{-# INLINEABLE resize #-}

class DeleteEntry xs where
    deleteEntry :: (MVector xs x, PrimMonad m) => xs (PrimState m) x -> Int -> m ()

instance DeleteEntry S.MVector where
    deleteEntry _ _ = return ()

instance DeleteEntry U.MVector where
    deleteEntry _ _ = return ()

instance DeleteEntry B.MVector where
    deleteEntry v i = v <~ i $ undefined

delete :: (Eq k, MVector ks k, MVector vs v, Hashable k, PrimMonad m, DeleteEntry ks, DeleteEntry vs)
       => Dictionary (PrimState m) ks k vs v -> k -> m ()
delete DRef{..} key' = do
    Dictionary{..} <- readMutVar getDRef
    let hashCode' = hash key' .&. 0x7FFFFFFFFFFFFFFF
        bucket = hashCode' `rem` V.length buckets
        go !last !i | i >= 0 = do
            hc <- hashCode ! i
            k  <- key ! i
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

{-# INLINEABLE delete #-}

lookup :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
   => Dictionary (PrimState m) ks k vs v -> k -> m (Maybe v)
lookup = at'
{-# INLINE lookup #-}

lookup' :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
   => Dictionary (PrimState m) ks k vs v -> k -> m v
lookup' = at
{-# INLINE lookup' #-}

lookupIndex :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
   => Dictionary (PrimState m) ks k vs v -> k -> m (Maybe Int)
lookupIndex ht k = do
    let safeIndex i | i < 0 = Nothing
                    | otherwise = Just i
    return . safeIndex =<< findEntry ht k
{-# INLINE lookupIndex #-}

null
  :: (MVector ks k, PrimMonad m)
  => Dictionary (PrimState m) ks k vs v -> m Bool
null ht = return . (== 0) =<< length ht
{-# INLINE null #-}

length
  :: (MVector ks k, PrimMonad m)
  => Dictionary (PrimState m) ks k vs v -> m Int
length DRef {..} = do
    Dictionary {..} <- readMutVar getDRef
    count           <- refs ! getCount
    freeCount       <- refs ! getFreeCount
    return (count - freeCount)
{-# INLINE length #-}

size
  :: (MVector ks k, PrimMonad m)
  => Dictionary (PrimState m) ks k vs v -> m Int
size = length
{-# INLINE size #-}

member
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => Dictionary (PrimState m) ks k vs v -> k -> m Bool
member ht k = return . (>= 0) =<< findEntry ht k
{-# INLINE member #-}

findWithDefault
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => Dictionary (PrimState m) ks k vs v -> v -> k -> m v
findWithDefault ht v k = return . fromMaybe v =<< at' ht k
{-# INLINE findWithDefault #-}

alter
  :: ( MVector ks k, MVector vs v, DeleteEntry ks, DeleteEntry vs
     , PrimMonad m, Hashable k, Eq k
     )
  => Dictionary (PrimState m) ks k vs v -> (Maybe v -> Maybe v) -> k -> m ()
alter ht f k = do
  mv <- at' ht k
  case f mv of
    Nothing -> delete ht k
    Just v  -> insert ht k v
{-# INLINABLE alter #-}

-- * Combine

-- | The union of two maps.
-- If a key occurs in both maps,
-- the mapping from the first will be the mapping in the result.
union
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => Dictionary (PrimState m) ks k vs v
  -> Dictionary (PrimState m) ks k vs v
  -> m (Dictionary (PrimState m) ks k vs v)
union = unionWith const

-- | The union of two maps.
-- If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
unionWith
  :: (MVector ks k, MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => (v -> v -> v)
  -> Dictionary (PrimState m) ks k vs v
  -> Dictionary (PrimState m) ks k vs v
  -> m (Dictionary (PrimState m) ks k vs v)
unionWith f = unionWithKey (const f)

-- | The union of two maps.
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
  hcsS <- SI.freeze . hashCode $ dictS
  let indices = catMaybes . zipWith collect [0 ..] . VI.toList . VI.take smallest $ hcsS
      collect i _ | hcsS VI.! i >= 0 = Just i
                  | otherwise       = Nothing
      go i = do
        k <- key dictS ! i
        v <- value dictS ! i
        iS <- at' tG k -- FIXME: too many reads
        case iS of
          Nothing -> insert ht k v
          Just vG -> insert ht k (g k v vG)
  mapM_ go indices
  return ht

-- * Difference and intersection

-- | Difference of two maps. Return elements of the first map
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
    go ht (k, v) = do
      mv <- lookup b k
      case mv of
        Nothing -> insert ht k v
        _       -> return ()
{-# INLINABLE difference #-}

-- | Difference with a combining function. When two equal keys are
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
    go ht (k, v) = do
      mv <- lookup b k
      case mv of
        Nothing -> insert ht k v
        Just w  -> maybe (return ()) (insert ht k) (f v w)
{-# INLINABLE differenceWith #-}

-- | Intersection of two maps. Return elements of the first
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
    go ht (k, v) = do
      mv <- lookup b k
      case mv of
        Nothing -> return ()
        Just _  -> insert ht k v
{-# INLINABLE intersection #-}

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
    go ht (k, v) = do
      mv <- lookup b k
      case mv of
        Nothing -> return ()
        Just w  -> insert ht k (f v w)
{-# INLINABLE intersectionWith #-}

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
    go ht (k, v) = do
      mv <- lookup b k
      case mv of
        Nothing -> return ()
        Just w  -> insert ht k (f k v w)
{-# INLINABLE intersectionWithKey #-}

-- * List conversions

fromList
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => [(k, v)] -> m (Dictionary (PrimState m) ks k vs v)
fromList kv = do
    ht <- initialize 1
    mapM_ (uncurry (insert ht)) kv
    return ht

toList
  :: (MVector ks k, MVector vs v, PrimMonad m, Hashable k, Eq k)
  => (Dictionary (PrimState m) ks k vs v) -> m [(k, v)]
toList DRef {..} = do
    Dictionary {..} <- readMutVar getDRef
    hcs <- SI.freeze hashCode
    count <- refs ! getCount
    let indeces = catMaybes . zipWith collect [0..] . VI.toList . VI.take count $ hcs
        collect i _ | hcs VI.! i >= 0 = Just i
                    | otherwise       = Nothing
        go i = do
          k <- key ! i
          v <- value ! i
          return (k, v)
    mapM go indeces

-- * Extras

primes :: UI.Vector Int
primes = UI.fromList [
    3, 7, 11, 17, 23, 29, 37, 47, 59, 71, 89, 107, 131, 163, 197, 239, 293, 353, 431, 521, 631,
    761, 919, 1103, 1327, 1597, 1931, 2333, 2801, 3371, 4049, 4861, 5839, 7013, 8419, 10103, 12143,
    14591, 17519, 21023, 25229, 30293, 36353, 43627, 52361, 62851, 75431, 90523, 108631, 130363,
    156437, 187751, 225307, 270371, 324449, 389357, 467237, 560689, 672827, 807403, 968897,
    1162687, 1395263, 1674319, 2009191, 2411033, 2893249, 3471899, 4166287, 4999559, 5999471,
    7199369, 8639249, 10367101, 12440537, 14928671, 17914409, 21497293, 25796759, 30956117,
    37147349, 44576837, 53492207, 64190669, 77028803, 92434613, 110921543, 133105859, 159727031,
    191672443, 230006941, 276008387, 331210079, 397452101, 476942527, 572331049, 686797261,
    824156741, 988988137, 1186785773, 1424142949, 1708971541, 2050765853 ]

getPrime :: Int -> Int
getPrime n = fromJust $ UI.find (>= n) primes
