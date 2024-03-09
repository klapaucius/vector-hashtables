{-|
Module      : Data.Vector.Hashtables
Description : Provides hashtables, basic interface and set of utilities.
Copyright   : (c) klapaucius, swamp_agr, 2016-2021
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
    , upsert
    , alter
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
-- - This package provides hashtable implementation similar to .NET Generic Dictionary implementation (at the time of 2015) <https://github.com/dotnet/coreclr/blob/3a0d638843472056c0cbb723beaed0b1152ca36d/src/mscorlib/src/System/Collections/Generic/Dictionary.cs>.
-- 
-- - It was originated as response to <https://comp.lang.functional.narkive.com/uYVjkKfl/f-vs-ocaml-vs-python-vs-haskell-hash-table-performance#post5>.
--
-- - For more hashtables implementations see <https://rcoh.me/posts/hash-map-analysis/>.

-- $usage
--
-- >>> import qualified Data.Vector.Storable.Mutable as VM
-- >>> import qualified Data.Vector.Unboxed.Mutable  as UM
-- >>> import Data.Vector.Hashtables
-- >>> type HashTable k v = Dictionary (PrimState IO) VM.MVector k UM.MVector v
-- >>> ht <- initialize 0 :: IO (HashTable Int Int)
-- >>> insert ht 0 1
--
