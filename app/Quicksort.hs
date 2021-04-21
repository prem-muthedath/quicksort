{-# LANGUAGE ScopedTypeVariables #-}

-- | Benchmark haskell quicksort implementations: classic, Diller, Leal, Bird.
-- author: Prem Muthedath.

module Quicksort (defaultMain, qsortFunctions) where

import System.Random hiding (split)         -- for randomRs, mkStdGen
import qualified Criterion.Main as CM       -- for running benchmarks

-- | benchmark haskell quicksort implementations -- Classic, Diller, Leal, Bird.
--   here, we study/understand performance differences in implementations.
--   all non-classic implementations (supposedly!!) more efficient.
--   more efficient because they split list in a single pass, rather than 2.
--   see Enrique Santos Leal's comment on Lennart's blog:
--   https://augustss.blogspot.com/search?q=quicksort

--   findings:
--   (a) best: Leal, Bird (they are almost same)
--   (b) LealM ~ Leal
--   (c) Classic ~ Diller (Classic seems to fare somewhat better)

-- classic haskell quicksort implementation.
-- traverses list twice in each recursive call -- perhaps why it is slower.
-- performance ~ `qsortDiller`
-- REF: richard bird,  chapter 7 -- thinking functionally in haskell.
-- NOTE: GHC requires explicit `forall` at top level for compilation.
qsortClassic :: (Ord a) => [a] -> [a]
qsortClassic []     = []
qsortClassic (x:xs) = qsortClassic [y | y <- xs, y < x] ++ [x] ++
                      qsortClassic [y | y <- xs, x <= y]

-- http://www.cantab.net/users/antoni.diller/haskell/units/unit07.html
-- performance ~ `qsortClassic`
-- worst-case inputs:
-- take 10000 [100000,99999..1], ~ 16 secs (slowest);
-- take 100000 [1000000,999999..1] hangs
qsortDiller :: Ord a => [a] -> [a]
qsortDiller []      = []
qsortDiller (x:xs)  = qsortDiller ls ++ [x] ++ qsortDiller gs
  where (ls, gs) = split x xs

split :: Ord a => a -> [a] -> ([a],[a])
split _ [] = ([], [])
split x (y:ys)
  | y < x     = (y:less, greater)
  | otherwise = (less, y:greater)
  where (less, greater) = split x ys

-- https://augustss.blogspot.com/search?q=quicksort
-- Enrique Santos Leal posted this code in comments to Lennart's quicksort blog.
-- performance ~ `qsortBird`
-- worst-case inputs:
-- take 10000 [100000,99999..1], ~ 1.7 secs (best);
-- take 100000 [1000000,999999..1] ~ 11 min
qsortLeal    :: Ord a => [a] -> [a]
qsortLeal []      = []
qsortLeal (x:xs)  = qsortLeal ls ++ [x] ++ qsortLeal gs
  where (ls, gs)  = split' (< x) xs

-- NOTE: GHC requires explicit `forall` at top level for compilation.
split' :: forall a. Ord a => (a -> Bool) -> [a] -> ([a], [a])
split' p xs = sep xs [] []
  where sep :: [a] -> [a] -> [a] -> ([a], [a])
        sep [] ps qs = (ps, qs)
        sep (o:os) ps qs
          | p o = sep os (o:ps) qs
          | otherwise = sep os ps (o:qs)

-- Leal's code modified so `split'` has ~ type signature as `split`. done to 
-- understand why Leal's code is quicker than `qsortDiller`. with this change, 
-- `qsortLealM` code ~ `qsortDiller`, except for `split''`. yet, `qsortLealM` 
-- has same timings as `qsortLeal`, => `qsortLeal`/`qsortLealM` faster than 
-- `qsortDiller` due to differences between `split` & `split'`/`split''`.
-- so what are these differences?
-- 1. `split` is recursive, while `split'`/`split''` is not => GHC inlines 
--    `split'`/`split''` but not `split`.
-- 2. `split` allocates a tuple for each recursive call; `split'`/`split''` only 
--    does 1 at end => GHC optimizes `split'`/`split''` better.
qsortLealM :: Ord a => [a] -> [a]
qsortLealM []     = []
qsortLealM (x:xs) = qsortLealM ls ++ [x] ++ qsortLealM gs
  where (ls, gs)  = split'' x xs

-- NOTE: GHC requires explicit `forall` at top level for compilation.
split'' :: forall a. Ord a => a -> [a] -> ([a], [a])
split'' p xs = sep xs [] []
  where sep :: [a] -> [a] -> [a] -> ([a], [a])
        sep [] ps qs = (ps, qs)
        sep (o:os) ps qs
          | o < p = sep os (o:ps) qs
          | otherwise = sep os ps (o:qs)

-- richard bird -- chapter 7 -- thinking functionally in haskell.
-- `sortp` (almost) like `split'`/`split''`; allocates just once (at the end).
-- `sortp` also non-recursive, so GHC (likely) inlines `sortp`.
-- performance ~ `qsortLeal`/`qsortLealM`.
-- NOTE: GHC requires explicit `forall` at top level for compilation.
qsortBird :: forall a. Ord a => [a] -> [a]
qsortBird []     = []
qsortBird (x:xs) = sortp xs [] []
  where sortp :: [a] -> [a] -> [a] -> [a]
        sortp [] us vs     = qsortBird us ++ [x] ++ qsortBird vs
        sortp (y:ys) us vs = if y < x
          then sortp ys (y:us) vs
          else sortp ys us (y:vs)

data Qsort = Classic | Diller | Leal | LealM | Bird deriving (Eq, Show, Enum)

qsortFunctions :: Ord a => [(Qsort, [a] -> [a])]
qsortFunctions = map (\x -> (x, qsortFunction x)) [toEnum 0 :: Qsort ..]
  where qsortFunction :: Ord a => Qsort -> ([a] -> [a])
        qsortFunction Classic = qsortClassic
        qsortFunction Diller  = qsortDiller
        qsortFunction Leal    = qsortLeal
        qsortFunction LealM   = qsortLealM
        qsortFunction Bird    = qsortBird

data List = Simple | Random | Descending | Ascending | BigDescending deriving (Eq, Show)

generate :: List -> [Int]
generate Simple        = [19, 3, 78, 5, 4, 33, 77, 21, 7, 58]
generate Random        = take 1000000 . randomRs (2 :: Int, 10000000 :: Int) . mkStdGen $ 0
generate Descending    = take 10000 [100000,99999..1]  -- worst case
generate Ascending     = take 10000 [1..] -- worst case
generate BigDescending = take 1000000 [10000000,9999999..1] -- very bad, may hang

-- benchmark using criterion package.
-- criterion tutorial @ http://www.serpentine.com/criterion/tutorial.html
-- modeled after code from https://goo.gl/x5tMH9 (aweinstock @ github).
runBenchmarks :: List -> IO ()
runBenchmarks list = do
   putStrLn $ "Benchmark input type: " <> show list
   CM.defaultMain . return $ CM.bgroup "quicksort" [
       CM.bench "split:"    $ CM.nf _split sample,      -- nf means normal form
       CM.bench "split'':"  $ CM.nf _split'' sample,
       CM.bench "qsortClassic:"  $ CM.nf qsortClassic sample,
       CM.bench "qsortDiller (using split):"  $ CM.nf qsortDiller sample,
       CM.bench "qsortLeal (using split'):"   $ CM.nf qsortLeal sample,
       CM.bench "qsortLealM (using split''):" $ CM.nf qsortLealM sample,
       CM.bench "qsortBird:" $ CM.nf qsortBird sample
      ] where sample = generate list
              (_split, _split'') = let pivot = head sample
                                   in (split pivot, split'' pivot)
defaultMain :: IO ()
defaultMain = runBenchmarks Simple

