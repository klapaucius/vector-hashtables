#!/usr/bin/env cabal
{- cabal:
build-depends: arithmoi, base, quote-quot
-}

import Data.Bits (FiniteBits)
import Data.Int (Int32, Int64)
import Math.NumberTheory.Primes (nextPrime, unPrime)
import Numeric.QuoteQuot (AST (..), assumeNonNegArg, astQuot)

-- | For a given bitness, expressed as a ~ Int32 or a ~ Int64,
-- generate a list of primes such that each prime is at least 20% larger
-- than the previous. Additionally, for each of selected primes
-- there exist numbers m and s such that (assuming Int64) for every n >= 0
--
-- n `quot` p = (n * m) `shiftR` (64 + s)
--
-- The function returns a list of tuples (p, m, s).
--
genPrimes :: (FiniteBits a, Integral a, Show a, Bounded a) => [(a, a, Int)]
genPrimes = go 3
  where
    go n
      | n < 0 = []
      | n >= maxBound `quot` 2 = []
      | p < n = []
      | otherwise = case assumeNonNegArg (astQuot p) of
        Shr (MulHi Arg mul) shft -> (p, mul, shft) : go p'
        _ -> go (p + 1)
      where
        p = fromInteger (unPrime (nextPrime (toInteger n)))
        p' = ceiling (fromIntegral p * 1.2 :: Double)

main :: IO ()
main = do
  putStrLn "-- | This data is auto-generated by GenPrimes.hs."
  putStrLn "-- The vector contains tuples (p, m, s) such that p is prime"
  putStrLn "-- and (assuming 64-bit architecture) for every n >= 0"
  putStrLn "-- it holds that n `quot` p = (n * m) `shiftR` (64 + s),"
  putStrLn "-- enabling faster computation of remainders."
  putStrLn "primesWithFastRem :: UI.Vector (Int, Int, Int)"
  putStrLn "primesWithFastRem = UI.fromList $"
  putStrLn "  if finiteBitSize (0 :: Int) == 32"
  putStrLn $ "  then " ++ show (genPrimes :: [(Int32, Int32, Int)])
  putStrLn $ "  else " ++ show (genPrimes :: [(Int64, Int64, Int)])
