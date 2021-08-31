{-|
Module      : Data.Vector.Hashtables
Description : Provides hashtables, basic interface and set of utilities.
Copyright   : (c) klapaucius, 2016-2021
License     : BSD3
-}
module Data.Vector.Hashtables
    ( -- * Documentation
      -- $doc

      -- ** Usage
      -- $usage

      -- ** Types
      Dictionary (..)
    , FrozenDictionary (..)
    , findElem
    , Dictionary_ (..)
    , findEntry

    -- ** Construction
    , initialize
    , clone

    -- ** Basic interface
    , null
    , size
    , keys
    , values
    , lookup
    , lookup'
    , insert
    , delete
    , alterM

    -- ** Combine

    -- *** Union
    , union
    , unionWith
    , unionWithKey
    
    -- *** Difference
    , difference
    , differenceWith

    -- *** Intersection
    , intersection
    , intersectionWith
    , intersectionWithKey

    -- ** Conversions
    
    -- *** Mutable
    , unsafeFreeze
    , unsafeThaw

    -- *** List
    , fromList
    , toList

    , module Control.Monad.Primitive
    ) where

import Prelude hiding (null, lookup)
import Control.Monad.Primitive
import Data.Vector.Hashtables.Internal

-- $doc
--
-- This package provides hashtable implementation similar to .NET. It was originated as response to <https://comp.lang.functional.narkive.com/uYVjkKfl/f-vs-ocaml-vs-python-vs-haskell-hash-table-performance#post5>.
-- 

-- $usage
--
-- >>> import qualified Data.Vector.Storable.Mutable as VM
-- >>> import qualified Data.Vector.Unboxed.Mutable  as UM
-- >>> import Data.Vector.Hashtables
-- >>> type HashTable k v = Dictionary (PrimState IO) VM.MVector k UM.MVector v
-- >>> ht <- initialize 0 :: IO (HashTable Int Int)
-- >>> insert ht 0 1
--
