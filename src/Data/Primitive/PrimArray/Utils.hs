{-|
Module      : Data.Primitive.PrimArray.Utils
Description : Provides useful utilities for operating with mutable primitive arrays.
Copyright   : (c) klapaucius, swamp_agr, 2016-2021
License     : BSD3
-}
module Data.Primitive.PrimArray.Utils where

import Data.Primitive.PrimArray
import Control.Monad.Primitive
import Data.Primitive

replicate :: (PrimMonad m, Prim a) 
          => Int -> a -> m (MutablePrimArray (PrimState m) a)
replicate n x = do
    xs <- newPrimArray n
    sz <- getSizeofMutablePrimArray xs
    setPrimArray xs 0 sz x
    return xs

{-# INLINE replicate #-}

clone :: (PrimMonad m, Prim a) 
      => MutablePrimArray (PrimState m) a -> m (MutablePrimArray (PrimState m) a)
clone xs = do
    sz <- getSizeofMutablePrimArray xs
    cloneMutablePrimArray xs 0 sz

{-# INLINE clone #-}

unsafeFreeze :: PrimMonad m 
             => MutablePrimArray (PrimState m) a -> m (PrimArray a)
unsafeFreeze = unsafeFreezePrimArray

{-# INLINE unsafeFreeze #-}

unsafeThaw :: PrimMonad m 
           => PrimArray a -> m (MutablePrimArray (PrimState m) a)
unsafeThaw = unsafeThawPrimArray

{-# INLINE unsafeThaw #-}

growWith :: (PrimMonad m, Prim a) 
     => a -> MutablePrimArray (PrimState m) a -> Int -> m (MutablePrimArray (PrimState m) a)
growWith a xs delta = do 
    r <- growNoZ xs delta
    sz <- getSizeofMutablePrimArray xs
    setPrimArray r sz delta a
    return r

{-# INLINE growWith #-}

growNoZ :: (PrimMonad m, Prim a) 
     => MutablePrimArray (PrimState m) a -> Int -> m (MutablePrimArray (PrimState m) a)
growNoZ xs delta = do
    sz <- getSizeofMutablePrimArray xs
    resizeMutablePrimArray xs (sz + delta)

{-# INLINE growNoZ #-}

freeze :: (PrimMonad m, Prim a) 
       => MutablePrimArray (PrimState m) a -> m (PrimArray a)
freeze xs = do 
    r <- unsafeFreezePrimArray xs
    return $ clonePrimArray r 0 (sizeofPrimArray r)

{-# INLINE freeze #-}

length :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> m Int
length = getSizeofMutablePrimArray

{-# INLINE length #-}
