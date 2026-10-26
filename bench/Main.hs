{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import qualified Data.Vector.Storable            as V
import qualified Data.Vector.Storable.Mutable    as VM
import qualified Data.Vector.Mutable             as BV
import           Control.Monad.Primitive         (PrimMonad(PrimState))
import qualified Data.HashTable.IO               as H

import           Criterion.Main                  (bench, bgroup, defaultMain, nfIO)
import           Criterion                       (Benchmark)

import qualified Data.Vector.Hashtables.Internal as VH
import qualified Data.Vector                     as BVec
import           Control.DeepSeq                 (NFData (..))
import           Criterion.Main                  (perRunEnv)
import           Data.Bits                       (shiftR, xor)
import           Data.Hashable                   (Hashable (..))

vh :: Int -> IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
vh n = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0
    return ht

fvh :: Int -> IO (VH.FrozenDictionary V.Vector Int V.Vector Int)
fvh n = do
    h <- vh n
    c <- VH.clone h
    VH.unsafeFreeze c

bh :: Int -> IO (H.BasicHashTable Int Int)
bh n = do
    ht <- H.newSized n :: IO (H.BasicHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0
    return ht

fl :: Int -> [(Int, Int)]
fl n = mkPair <$> [0 .. n]
  where
    mkPair !x = (x, x)

vhfind :: Int -> VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int -> IO Int
vhfind n ht = do
    let go !i !s | i <= n = do
                                x <- VH.findEntry ht i
                                go (i + 1) (s + x)
                 | otherwise = return s
    go 0 0

fvhfind :: Int -> VH.FrozenDictionary V.Vector Int V.Vector Int -> IO Int
fvhfind n ht = return $ go 0 0 where
    go !i !s | i <= n = go (i + 1) (s + VH.findElem ht i)
             | otherwise = s

bhfind :: Int -> H.BasicHashTable Int Int -> IO Int
bhfind n ht = do
    let go !i !s | i <= n = do
                                Just x <- H.lookup ht i
                                go (i + 1) (s + x)
                 | otherwise = return s
    go 0 0

htb :: Int -> IO ()
htb n = do
    ht <- H.newSized n :: IO (H.BasicHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0


vht :: Int -> IO ()
vht n = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtd :: Int -> IO ()
vhtd n = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0
    let go1 !i | i <= n = VH.delete ht i >> go1 (i + 1)
               | otherwise = return ()
    go1 0

htbd :: Int -> IO ()
htbd n = do
    ht <- H.newSized n :: IO (H.BasicHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0
    let go1 !i | i <= n = H.delete ht i >> go1 (i + 1)
               | otherwise = return ()
    go1 0

vhtb :: Int -> IO ()
vhtb n = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) BV.MVector Int BV.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtk :: Int -> IO ()
vhtk n = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) VM.MVector Int BV.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

htbg :: Int -> IO ()
htbg n = do
    ht <- H.newSized 1 :: IO (H.BasicHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtg :: Int -> IO ()
vhtg n = do
    ht <- VH.initialize 1 :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtbg :: Int -> IO ()
vhtbg n = do
    ht <- VH.initialize 1 :: IO (VH.Dictionary (PrimState IO) BV.MVector Int BV.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtkg :: Int -> IO ()
vhtkg n = do
    ht <- VH.initialize 1 :: IO (VH.Dictionary (PrimState IO) VM.MVector Int BV.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

mvb :: Int -> IO ()
mvb n = do
    ht <- BV.new (n+1)
    let go !i | i <= n = BV.write ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

mv :: Int -> IO ()
mv n = do
    ht <- VM.new (n+1)
    let go !i | i <= n = VM.write ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

bhfromList l = do
  _bht <- H.fromList l :: IO (H.BasicHashTable Int Int)
  return ()

vhfromList l = do
  _ht <- VH.fromList l :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
  return ()

bhlookupIndex :: Int -> H.BasicHashTable Int Int -> IO Int
bhlookupIndex n ht = do
    let go !i !s | i <= n = do
                                Just x <- H.lookupIndex ht i
                                go (i + 1) (s + fromIntegral x)
                 | otherwise = return s
    go 0 0

vhlookupIndex :: Int -> VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int -> IO Int
vhlookupIndex n ht = do
    let go !i !s | i <= n = do
                                Just x <- VH.lookupIndex ht i
                                go (i + 1) (s + x)
                 | otherwise = return s
    go 0 0

bhtoList = H.toList

vhtoList = VH.toList

bgc :: Int -> IO Benchmark
bgc n = do
    h <- vh n
    h2 <- bh n
    fh <- fvh n
    let l = fl n
    return $ bgroup (show n)
        [ bgroup "insert" 
            [ bench "hashtables basic"  $ nfIO (htb n)
            , bench "vector-hashtables boxed" $ nfIO (vhtb n)
            , bench "vector-hashtables unboxed keys" $ nfIO (vhtk n)
            , bench "vector-hashtables" $ nfIO (vht n) 
            , bench "mutable vector boxed" $ nfIO (mvb n)
            , bench "mutable vector" $ nfIO (mv n) ]
        , bgroup "insert (resize)"
            [ bench "hashtables basic"  $ nfIO (htbg n)
            , bench "vector-hashtables boxed" $ nfIO (vhtbg n)
            , bench "vector-hashtables unboxed keys" $ nfIO (vhtkg n)
            , bench "vector-hashtables" $ nfIO (vhtg n) ]
        , bgroup "insert, delete"
            [ bench "hashtables basic"  $ nfIO (htbd n)
            , bench "vector-hashtables" $ nfIO (vhtd n) ]
        , bgroup "find"
            [ bench "hashtables basic" $ nfIO (bhfind n h2)
            , bench "vector-hashtables" $ nfIO (vhfind n h)
            , bench "vector-hashtables (frozen)" $ nfIO (fvhfind n fh) ]
        , bgroup "lookupIndex"
            [ bench "hashtables basic" $ nfIO (bhlookupIndex n h2)
            , bench "vector-hashtables" $ nfIO (vhlookupIndex n h) ]
        , bgroup "fromList"
            [ bench "hashtables basic" $ nfIO (bhfromList l)
            , bench "vector-hashtables" $ nfIO (vhfromList l) ]
        , bgroup "toList"
            [ bench "hashtables basic" $ nfIO (bhtoList h2)
            , bench "vector-hashtables" $ nfIO (vhtoList h) ]]

main :: IO ()
main = do
    let inputs = [1000,10000,100000,1000000]
    comparisonBench <- mapM bgc inputs
    utilitiesBench  <- mapM utilities inputs
    defaultMain
        [ bgroup "Comparison" comparisonBench
        , bgroup "Utilities"  utilitiesBench
        , bgroup "AlterDelete" alterDeleteBench ]

-- ** Utilities benchmark

utilities n = do
    -- utilities input data
    hAt' <- vh n
    hInsert <- vh n
    hDelete <- vh n
    hLookup <- vh n
    hLookup' <- vh n
    hLookupIndex <- vh n
    hNull <- vh n
    hLength <- vh n
    hSize <- vh n
    hMember <- vh n
    hFindWithDefault <- vh n
    hUpsert <- vh n
    hAlter <- vh n
    hAlterM <- vh n
    hUnion1 <- vh n
    hUnion2 <- vh n
    hDifference1 <- vh n
    hDifference2 <- vh n
    hIntersection1 <- vh n
    hIntersection2 <- vh n
    hFromList <- VH.toList =<< vh n
    hToList <- vh n

    return $ bgroup (show n)
        [ bench "at'" $ nfIO (bhuat' n hAt')
        , bench "insert" $ nfIO (bhuinsert n hInsert)
        , bench "delete" $ nfIO (bhudelete n hDelete)
        , bench "lookup" $ nfIO (bhulookup n hLookup)
        , bench "lookup'" $ nfIO (bhulookup' n hLookup')
        , bench "lookupIndex" $ nfIO (bhulookupIndex n hLookupIndex)
        , bench "null" $ nfIO (bhunull hNull)
        , bench "length" $ nfIO (bhulength hLength)
        , bench "size" $ nfIO (bhusize hSize)
        , bench "member" $ nfIO (bhumember n hMember)
        , bench "findWithDefault" $ nfIO (bhufindWithDefault n hFindWithDefault)
        , bench "upsert" $ nfIO (bhuupsert n hUpsert)
        , bench "alter" $ nfIO (bhualter n hAlter)
        , bench "alterM" $ nfIO (bhualterM n hAlterM)
        , bench "union" $ nfIO (bhuunion hUnion1 hUnion2)
        , bench "difference" $ nfIO (bhudifference hDifference1 hDifference2)
        , bench "intersection" $ nfIO (bhuintersection hIntersection1 hIntersection2)
        , bench "fromList" $ nfIO (bhufromList hFromList)
        , bench "toList" $ nfIO (VH.toList hToList) ]

bhuat' n ht = do
    let go !i | i <= n = VH.at' ht i >> go (i + 1)
              | otherwise = return ()
    go 0

bhuinsert n ht = do
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

bhudelete n ht = do
    let go !i | i <= n = VH.delete ht i >> go (i + 1)
              | otherwise = return ()
    go 0

bhulookup n ht = do
    let go !i | i <= n = VH.lookup ht i >> go (i + 1)
              | otherwise = return ()
    go 0

bhulookup' n ht = do
    let go !i | i <= n = VH.lookup' ht i >> go (i + 1)
              | otherwise = return ()
    go 0

bhulookupIndex n ht = do
    let go !i | i <= n = VH.lookupIndex ht i >> go (i + 1)
              | otherwise = return ()
    go 0

bhunull = VH.null

bhulength = VH.length

bhusize = VH.size

bhumember n ht = do
    let go !i | i <= n = VH.member ht i >> go (i + 1)
              | otherwise = return ()
    go 0

bhufindWithDefault n ht = do
    let go !i | i <= n = VH.findWithDefault ht 0 i >> go (i + 1)
              | otherwise = return ()
    go 0

bhuupsert n ht = do
    let go !i | i <= n = VH.upsert ht (maybe minBound succ) i >> go (i + 1)
              | otherwise = return ()
    go 0

bhualter n ht = do
    let go !i | i <= n = VH.alter ht (fmap succ) i >> go (i + 1)
              | otherwise = return ()
    go 0

bhualterM :: Int -> VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int -> IO ()
bhualterM n ht = do
    let f = return . fmap succ
        go !i | i <= n = VH.alterM ht f i >> go (i + 1)
              | otherwise = return ()
    go 0

bhuunion ht1 ht2 = VH.union ht1 ht2 >> return ()

bhudifference ht1 ht2 = VH.difference ht1 ht2 >> return ()

bhuintersection ht1 ht2 = VH.intersection ht1 ht2 >> return ()

bhufromList htlist = do
  ht <- VH.fromList htlist :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
  return ()

bhutoList ht = VH.toList ht >> return ()

-- ** Deleting through alter/alterM, against delete
--
-- Only the deletes are timed: every run gets a freshly filled table
-- (perRunEnv), and deletes every key in insertion order.

-- | Every key in one bucket chain.
newtype Collide = Collide Int deriving (Eq, Show)

instance Hashable Collide where
  hashWithSalt _ _ = 0
  hash _ = 0

-- | Lets perRunEnv hold a mutable table.
newtype Env a = Env a

instance NFData (Env a) where
  rnf (Env a) = a `seq` ()

type IntTable = VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int
type CollideTable = VH.Dictionary (PrimState IO) BV.MVector Collide VM.MVector Int

-- | Distinct pseudo-random keys (SplitMix's finaliser is a bijection), so
-- buckets collide the way real keys do; sequential Int keys never collide.
randomKeys :: Int -> [Int]
randomKeys n = map mix [1 .. n + 1]
  where
    mix z0 = let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
                 z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
             in z2 `xor` (z2 `shiftR` 31)

fillInt :: BVec.Vector Int -> IO (Env IntTable)
fillInt ks = do
    ht <- VH.initialize (BVec.length ks)
    BVec.mapM_ (\k -> VH.insert ht k k) ks
    return (Env ht)

fillCollide :: BVec.Vector Collide -> IO (Env CollideTable)
fillCollide ks = do
    ht <- VH.initialize (BVec.length ks)
    BVec.mapM_ (\k@(Collide i) -> VH.insert ht k i) ks
    return (Env ht)

deleteGroup
  :: (BVec.Vector k -> IO (Env (VH.Dictionary (PrimState IO) ks k vs Int)))
  -> (VH.Dictionary (PrimState IO) ks k vs Int -> k -> IO ())
  -> (VH.Dictionary (PrimState IO) ks k vs Int -> k -> IO ())
  -> (VH.Dictionary (PrimState IO) ks k vs Int -> k -> IO ())
  -> String -> BVec.Vector k -> Benchmark
deleteGroup fill del alt altM name ks = bgroup name
    [ bench "delete" $ perRunEnv (fill ks) (\(Env ht) -> BVec.mapM_ (del ht) ks)
    , bench "alter"  $ perRunEnv (fill ks) (\(Env ht) -> BVec.mapM_ (alt ht) ks)
    , bench "alterM" $ perRunEnv (fill ks) (\(Env ht) -> BVec.mapM_ (altM ht) ks) ]
{-# INLINE deleteGroup #-}

intDeletes :: String -> BVec.Vector Int -> Benchmark
intDeletes = deleteGroup fillInt VH.delete
    (\ht -> VH.alter ht (const Nothing)) (\ht -> VH.alterM ht (const (return Nothing)))

collideDeletes :: String -> BVec.Vector Collide -> Benchmark
collideDeletes = deleteGroup fillCollide VH.delete
    (\ht -> VH.alter ht (const Nothing)) (\ht -> VH.alterM ht (const (return Nothing)))

alterDeleteBench :: [Benchmark]
alterDeleteBench =
    [ bgroup (show n)
        [ intDeletes "sequential" (BVec.fromList [0 .. n])
        , intDeletes "random" (BVec.fromList (randomKeys n)) ]
    | n <- [10000, 1000000] ] ++
    [ bgroup (show n) [ collideDeletes "one chain" (BVec.fromList (map Collide [0 .. n])) ]
    | n <- [1000] ]
