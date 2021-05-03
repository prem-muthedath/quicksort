{-# LANGUAGE ScopedTypeVariables #-}

-- | Benchmark haskell quicksort implementations defined in Quicksort module.
-- author: Prem Muthedath.

module Benchmark (defaultMain) where

import System.Random                        -- for randomRs, mkStdGen
import qualified Criterion.Main as CM       -- for running benchmarks
import Criterion.Measurement.Types (Benchmark)

import Types (Qsort, Name, Implementation(..))
import Quicksort (qsortImplementations, qsortSplits)

-- criterion reference (hackage): https://tinyurl.com/y6jtau3k

-- | `List` -- specifies samples for quicksort benchmarking.
-- example: `Random` => list with items in random order.
data List = Simple | Random | Descending | Ascending | BigDescending deriving (Eq, Show)

-- | sample for benchmarking.
type Sample = [Int]

-- | generate benchmark sample for a `List`.
generate :: List -> Sample
generate Simple        = [19, 3, 78, 5, 4, 33, 77, 21, 7, 58]
generate Random        = take 1000000 . randomRs (2 :: Int, 10000000 :: Int) . mkStdGen $ 0
generate Descending    = take 10000 [100000,99999..1]  -- worst case
generate Ascending     = take 10000 [1..] -- worst case
generate BigDescending = take 1000000 [10000000,9999999..1] -- very bad, may hang

-- | benchmark all quicksort implementations on a sample using criteriion.
-- criterion tutorial @ http://www.serpentine.com/criterion/tutorial.html
benchQuicksort :: Sample -> Benchmark
benchQuicksort sample =
   CM.bgroup "quicksort" $
      map (\(qsort :: Qsort, Implementation f) ->
          CM.bench (show qsort <> ":") $ CM.nf f sample
      ) qsortImplementations

-- | benchmark quicksort `splits` on a sample using criteriion package.
-- modeled after code from https://goo.gl/x5tMH9 (aweinstock @ github).
benchSplits :: Sample -> Benchmark
benchSplits sample =
   CM.bgroup "quicksort-splits" $
      map (\(name :: Name, f) ->
          let pivot = head sample
              g     = f pivot
              rest  = tail sample
          in CM.bench (name <> ":") $ CM.nf g rest
      ) qsortSplits

-- | run benchmarks.
defaultMain :: IO ()
defaultMain =
  let list :: List     = Simple
      name :: Name     = show list
      sample :: Sample = generate list
      size :: String   = show . length $ sample
  in do putStrLn $ "\n***** benchmark sample: " <> name <> ", size: " <> size <> " elements. *****"
        let benches = [benchQuicksort, benchSplits]
        CM.defaultMain $ map (\x -> x sample) benches


