{-# LANGUAGE BangPatterns, TypeFamilies #-}

module Main where

import qualified Data.Vector.Hashtables.Internal as VH
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable as V

import qualified Data.Vector.Mutable as BV

import qualified Data.HashTable.IO as H

-- import qualified Data.HashMap.Strict as Map

import Control.Monad
import Control.Monad.Primitive
import Data.IORef

n = 10000000

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

main = do 
    ht <- fvh
    s <- replicateM 10 $ fvhfind ht 
    print $ s