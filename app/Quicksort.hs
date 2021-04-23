{-# LANGUAGE ScopedTypeVariables #-}

-- | haskell quicksort implementations: classic, Diller, Leal, Bird.
-- author: Prem Muthedath.

module Quicksort (qsortImplementations, qsortSplits) where

import Types

-- | haskell quicksort implementations -- Classic, Diller, Leal, Bird.
-- purpose: study/understand performance differences in implementations.
-- all non-classic implementations (supposedly!!) more efficient.
-- more efficient because they split list in a single pass, rather than 2.
-- see Enrique Santos Leal's comment on Lennart's blog:
-- https://augustss.blogspot.com/search?q=quicksort

-- Benchmark module (app/Benchmark.hs) benchmarks these implementations.

-- findings:
--   (a) best: Leal, Bird (they are almost same)
--   (b) LealM ~ Leal
--   (c) Classic ~ Diller (Classic fares better mostly)

-- | classic haskell quicksort implementation.
-- traverses list twice in each recursive call -- perhaps why it is slower.
-- performance ~ `qsortDiller`
-- REF: richard bird,  chapter 7 -- thinking functionally in haskell.
-- NOTE: GHC requires explicit `forall` at top level for compilation.
qsortClassic :: (Ord a) => [a] -> [a]
qsortClassic []     = []
qsortClassic (x:xs) = qsortClassic [y | y <- xs, y < x] ++ [x] ++
                      qsortClassic [y | y <- xs, x <= y]

-- | antonii diller's quicksort.
-- http://www.cantab.net/users/antoni.diller/haskell/units/unit07.html
-- performance ~ `qsortClassic`
-- worst-case inputs:
--  1. take 10000 [100000,99999..1], ~ 16 secs (slowest);
--  2. take 100000 [1000000,999999..1] hangs
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

-- | Enrique Leal's quicksort.
-- https://augustss.blogspot.com/search?q=quicksort
-- Enrique Santos Leal posted this code in comments to Lennart's quicksort blog.
-- performance ~ `qsortBird`
-- worst-case inputs:
--  1. take 10000 [100000,99999..1], ~ 1.7 secs (best);
--  2. take 100000 [1000000,999999..1] ~ 11 min
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

-- | Leal's quicksort with `split''` that has ~ type signature as `split`.
-- done to understand why Leal's code is quicker than `qsortDiller`. with this 
-- change, `qsortLealM` code ~ `qsortDiller`, except for `split''`. yet, 
-- `qsortLealM` has ~ timings as `qsortLeal`, => `qsortLeal`/`qsortLealM` faster 
-- than `qsortDiller` due to differences between `split` & `split'`/`split''`.
-- so what are these differences?
--  1. `split` is recursive, while `split'`/`split''` is not => GHC inlines 
--     `split'`/`split''` but not `split`.
--  2. `split` allocates a tuple for each recursive call; `split'`/`split''` 
--     only does 1 at end => GHC optimizes `split'`/`split''` better.
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

-- | richard bird's quicksort.
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

-- | quicksort implementations specified by `Qsort`.
qsortImplementations :: Ord a => [(Qsort, [a] -> [a])]
qsortImplementations = map (\qsort -> (qsort, qsortImplementation qsort)) [toEnum 0 :: Qsort ..]
  where qsortImplementation :: Ord a => Qsort -> ([a] -> [a])
        qsortImplementation Classic = qsortClassic
        qsortImplementation Diller  = qsortDiller
        qsortImplementation Leal    = qsortLeal
        qsortImplementation LealM   = qsortLealM
        qsortImplementation Bird    = qsortBird

-- | splits used in quicksort implementations.
qsortSplits :: Ord a => [(Name, a -> [a] -> ([a], [a]))]
qsortSplits = [("split", split), ("split''", split'')]


