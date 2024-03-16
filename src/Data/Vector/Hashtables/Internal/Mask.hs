{-|
Module      : Data.Vector.Hashtables.Internal.Mask
Description : Provides arch-dependent mask for hashtables.
Copyright   : (c) klapaucius, swamp_agr, 2016-2021
License     : BSD3
-}
module Data.Vector.Hashtables.Internal.Mask where

-- | 'Int' mask. For 32-bit it is equal to @0x7FFFFFFF@. Otherwise, @0x7FFFFFFFFFFFFFFF@.
mask = maxBound :: Int
{-# INLINE mask #-}
