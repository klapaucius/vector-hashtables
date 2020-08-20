{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Criterion.Main                  (bench, defaultMain, nf, nfIO)

import qualified Data.Vector.Hashtables.Internal as VH
import qualified Data.Vector.Storable            as V
import qualified Data.Vector.Storable.Mutable    as VM

import qualified Data.Vector.Mutable             as BV

import qualified Data.HashTable.IO               as H

-- import qualified Data.HashMap.Strict as Map

import           Control.Monad
import           Control.Monad.Primitive
import           Data.IORef

n = 100000 :: Int

vh :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
vh = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0
    return ht

fvh :: IO (VH.FrozenDictionary V.Vector Int V.Vector Int)
fvh = do
    h <- vh
    c <- VH.clone h
    VH.unsafeFreeze c

bh :: IO (H.BasicHashTable Int Int)
bh = do
    ht <- H.newSized n :: IO (H.BasicHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0
    return ht

vhfind :: VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int -> IO Int
vhfind ht = do
    let go !i !s | i <= n = do
                                x <- VH.findEntry ht i
                                go (i + 1) (s + x)
                 | otherwise = return s
    go 0 0

fvhfind :: VH.FrozenDictionary V.Vector Int V.Vector Int -> IO Int
fvhfind ht = return $ go 0 0 where
    go !i !s | i <= n = go (i + 1) (s + VH.findElem ht i)
             | otherwise = s

bhfind :: H.BasicHashTable Int Int -> IO Int
bhfind ht = do
    let go !i !s | i <= n = do
                                Just x <- H.lookup ht i
                                go (i + 1) (s + x)
                 | otherwise = return s
    go 0 0

htb = do
    ht <- H.newSized n :: IO (H.BasicHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

htc = do
    ht <- H.newSized n :: IO (H.CuckooHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

htl = do
    ht <- H.newSized n :: IO (H.LinearHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vht = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtd = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0
    let go1 !i | i <= n = VH.delete ht i >> go1 (i + 1)
               | otherwise = return ()
    go1 0

htbd = do
    ht <- H.newSized n :: IO (H.BasicHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0
    let go1 !i | i <= n = H.delete ht i >> go1 (i + 1)
               | otherwise = return ()
    go1 0

vhtb = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) BV.MVector Int BV.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtk = do
    ht <- VH.initialize n :: IO (VH.Dictionary (PrimState IO) VM.MVector Int BV.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

htbg = do
    ht <- H.newSized 1 :: IO (H.BasicHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

htcg = do
    ht <- H.newSized 1 :: IO (H.CuckooHashTable Int Int)
    let go !i | i <= n = H.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

-- htl = do
--     ht <- H.newSized n :: IO (H.LinearHashTable Int Int)

vhtg = do
    ht <- VH.initialize 1 :: IO (VH.Dictionary (PrimState IO) VM.MVector Int VM.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtbg = do
    ht <- VH.initialize 1 :: IO (VH.Dictionary (PrimState IO) BV.MVector Int BV.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

vhtkg = do
    ht <- VH.initialize 1 :: IO (VH.Dictionary (PrimState IO) VM.MVector Int BV.MVector Int)
    let go !i | i <= n = VH.insert ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

mvb = do
    ht <- BV.new (n+1)
    let go !i | i <= n = BV.write ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

mv = do
    ht <- VM.new (n+1)
    let go !i | i <= n = VM.write ht i i >> go (i + 1)
              | otherwise = return ()
    go 0

main :: IO ()
main =  do
    h <- vh
    h2 <- bh
    fh <- fvh
    defaultMain
        [ bench "insert hashtables basic"  $ nfIO htb
        , bench "insert hashtables basic (resize)"  $ nfIO htbg
        , bench "insert hashtables basic (delete)"  $ nfIO htbd
        , bench "insert vector-hashtables boxed" $ nfIO vhtb
        , bench "insert vector-hashtables unboxed keys" $ nfIO vhtk
        , bench "insert vector-hashtables (resize)" $ nfIO vhtg
        , bench "insert vector-hashtables (delete)" $ nfIO vhtd
        , bench "insert vector-hashtables" $ nfIO vht
        , bench "insert mutable vector boxed" $ nfIO mvb
        , bench "insert mutable vector" $ nfIO mv
        , bench "find hashtables basic" $ nfIO (bhfind h2)
        , bench "find vector-hashtables" $ nfIO (vhfind h)
        , bench "find vector-hashtables (frozen)" $ nfIO (fvhfind fh) ]
