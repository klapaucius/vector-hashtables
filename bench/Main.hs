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


bgc :: Int -> IO Benchmark
bgc n = do
    h <- vh n
    h2 <- bh n
    fh <- fvh n
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
            , bench "vector-hashtables (frozen)" $ nfIO (fvhfind n fh) ]]

main :: IO ()
main = defaultMain =<< mapM bgc [1000,10000,100000,1000000]
