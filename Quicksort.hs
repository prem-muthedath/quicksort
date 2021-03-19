module Quicksort (defaultMain) where

import System.Random hiding (split)         -- for randomRs, mkStdGen
import qualified Criterion.Main as M        -- for running benchmarks

-- | efficient quicksort implementations in haskell
--   used to study/understand performance differences in implementations
--   all implementations (supposedly!!) > efficient than the one at haskell wiki
--   more efficient because they split the list in a single pass, rather than 2
--   (see Enrique Santos Leal's comment on Lennart's blog)


-- http://www.cantab.net/users/antoni.diller/haskell/units/unit07.html
-- worst-case inputs:
-- take 10000 [100000,99999..1], ~ 16 secs (slowest);
-- take 100000 [1000000,999999..1] hangs
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort gs
  where (ls, gs) = split x xs

split :: Ord a => a -> [a] -> ([a],[a])
split _ [] = ([], [])
split x (y:ys)
  | y < x     = (y:less, greater)
  | otherwise = (less, y:greater)
  where (less, greater) = split x ys


-- https://augustss.blogspot.com/search?q=quicksort
-- Enrique Santos Leal posted this code in comments to Lennart's quicksort blog
-- worst-case inputs:
-- take 10000 [100000,99999..1], ~ 1.7 secs (best);
-- take 100000 [1000000,999999..1] ~ 11 min
qsort'    :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' ls ++ [x] ++ qsort' gs
  where (ls, gs) = split' (< x) xs

split' :: (a -> Bool) -> [a] -> ([a], [a])
split' p xs = sep xs [] []
  where sep [] ps qs = (ps, qs)
        sep (o:os) ps qs
          | p o = sep os (o:ps) qs
          | otherwise = sep os ps (o:qs)



-- Enrique Santos Leal's code modified so split' has ~ type signature as split
-- this is done to understand why Enrique's code is quicker than Antoni Diller's (qsort)
-- with this modification, qsort'' code identical to qsort, except for split''
-- yet, qsort'' has same timings as qsort', which implies --
-- qsort'/qsort'' faster than qsort because of differences between split & split'/split''
-- so what are these differences?
-- 1. split is recursive, while split'/split'' is not => GHC inlines split'/split''
--    but not split
-- 2. split allocates a tuple for each recursive call, while split'/split''
--    only does 1 at the end  => GHC optimizes split'/split'' better
-- NOTE: split ~ split'' when timed individually => only when used
--       within qsort/qsort'', do these differences matter

qsort'' :: Ord a => [a] -> [a]
qsort'' []     = []
qsort'' (x:xs) = qsort'' ls ++ [x] ++ qsort'' gs
  where (ls, gs) = split'' x xs

split'' :: Ord a => a -> [a] -> ([a], [a])
split'' p xs = sep xs [] []
     where sep [] ps qs = (ps, qs)
           sep (o:os) ps qs
                        | o < p = sep os (o:ps) qs
                        | otherwise = sep os ps (o:qs)



-- richard bird -- chapter 7 -- thinking functionally in haskell
-- traverses the list twice in each recursive call -- perhaps why it is slower
-- performance ~ qsort
qsort1 :: (Ord a) => [a] -> [a]
qsort1 []     = []
qsort1 (x:xs) = qsort1 [y | y <- xs, y < x] ++ [x] ++
                qsort1 [y | y <- xs, x <= y]



-- richard bird -- chapter 7 -- thinking functionally in haskell
-- sortp (almost) like split'/split''; allocates just once (at the end)
-- sortp also non-recursive, so GHC (likely) inlines sortp
-- performance ~ qsort'/qsort''
qsort2 :: (Ord a) => [a] -> [a]
qsort2 []     = []
qsort2 (x:xs) = sortp xs [] []
     where
         sortp [] us vs     = qsort2 us ++ [x] ++ qsort2 vs
         sortp (y:ys) us vs = if y < x
           then sortp ys (y:us) vs
           else sortp ys us (y:vs)



-- benchmarking using criterion
-- criterion tutorial @ http://www.serpentine.com/criterion/tutorial.html

data List = Simple | Random | Descending | Ascending | BigDescending deriving (Eq, Show)

generate :: List -> [Int]
generate Simple        = [19, 3, 78, 5, 4, 33, 77, 21, 7, 58]
generate Random        = take 1000000 . randomRs (2 :: Int, 10000000 :: Int) . mkStdGen $ 0
generate Descending    = take 10000 [100000,99999..1]  -- worst case
generate Ascending     = take 10000 [1..] -- worst case
generate BigDescending = take 1000000 [10000000,9999999..1] -- very bad, may hang


-- modeled after code from https://goo.gl/x5tMH9 (aweinstock @ github)
runBenchmarks :: List -> IO ()
runBenchmarks list = do
   M.defaultMain . return $ M.bgroup "quicksort" [
       M.bench "split:"    $ M.nf _split sample,      -- nf means normal form
       M.bench "split'':"  $ M.nf _split'' sample,
       M.bench "qsort -- Diller (using split):"   $ M.nf qsort sample,
       M.bench "qsort' -- Leal (using split'):"   $ M.nf qsort' sample,
       M.bench "qsort'' -- Leal (using split''):" $ M.nf qsort'' sample,
       M.bench "qsort1 -- Bird 1:"  $ M.nf qsort1 sample,
       M.bench "qsort2 -- Bird 2:"  $ M.nf qsort2 sample
      ] where sample = generate list
              (_split, _split'') = let pivot = head sample
                                   in (split pivot, split'' pivot)

defaultMain :: IO ()
defaultMain = runBenchmarks Simple
