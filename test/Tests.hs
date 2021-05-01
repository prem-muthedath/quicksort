{-# LANGUAGE ScopedTypeVariables #-}

-- | QuickCheck tests for haskell quicksort implementations.
-- author: Prem Muthedath.

module Tests (defaultMain) where

import Test.QuickCheck

import Data.List (nub, sort)

import Types (Qsort, Implementation(..))
import Quicksort (qsortImplementations)
import Terminal

-- | `TestCase` -- specifies test cases for quickcheck testing.
-- each value refers to a specific quickcheck property; that is,
-- a quickcheck property is a function to test a specific test case.
data TestCase = Ordering | Invariance | Model | Min | Max deriving (Eq, Enum)

-- | `Show` instance for `TestCase`.
instance Show TestCase where
  show Ordering    = "*** property: ordered ***"
  show Invariance  = "*** property: invariance ***"
  show Model       = "*** property: model sort equivalence ***"
  show Min         = "*** property: minimum ***"
  show Max         = "*** property: maximum ***"

-- | all test cases.
testCases :: [TestCase]
testCases = [toEnum 0 :: TestCase ..]

-- | `True` if list has duplicates.
-- REF: /u/ Wong Jia Hau @ https://tinyurl.com/un79tvk (so)
hasDups :: Ord a => [a] -> Bool
hasDups xs = length (nub xs) /= length xs

-- | `True` if list is ordered.
--  REF: chapter 11, real world haskell.
ordered :: Ord a => [a] -> Bool
ordered []       = True
ordered [_]      = True
ordered (x:y:ys) = x <= y && ordered (y:ys)

-- | quickcheck `classifications` for a property.
classifys :: (Ord a, Testable prop) => [a] -> prop -> Property
classifys xs = classify (xs==[]) "empty" .
               classify (length xs > 10) "has > 10 elements" .
               classify (ordered xs) "pre-ordered" .
               classify (hasDups xs) "has duplicates"

-- | quickcheck test for a given quicksort implementation.
-- NOTE: using junit terminology, a test is actually a collection of test cases.
-- when we run quickcheck, we feed it a test case; i.e., a `prop_xyz` function.
qcTest :: forall a. (Ord a, Show a, Arbitrary a)
       => ([a] -> [a])
       -> [(TestCase, [a] -> Property)]
qcTest f = map (\tc -> (tc, qcProperty tc)) testCases
  where qcProperty :: TestCase -> [a] -> Property
        qcProperty testCase = case testCase of
           Ordering   -> \xs -> classifys xs $ ordered (f xs)
           Invariance -> \xs -> classifys xs $ f xs == f (f xs)
           Model      -> \xs -> classifys xs $ f xs == sort xs
           Min        -> \_ -> forAll (listOf1 arbitrary) $
                         \xs -> classifys xs $ head (f xs) == minimum xs
           Max        -> \_ -> forAll (listOf1 arbitrary) $
                         \xs -> classifys xs $ last (f xs) == maximum xs

-- | run quickcheck on a specific haskell quicksort implementation.
runQC :: (Ord a, Show a, Arbitrary a)
      => Qsort
      -> Option
      -> ([a] -> [a])
      -> IO ()
runQC qsort opt f = do
  putStrLn $ "\n--- " ++ show qsort ++ " ---"
  putStrLn $ "testing with: " <> list opt
  mapM_ (\(testCase, prop) ->
       do putStrLn $ show testCase
          quickCheck prop
    ) $ qcTest f

-- | run quickcheck on all haskell quicksort implementations.
-- offers commandline option for specifying input list type for quickcheck 
-- tests. if none specified, uses `[Int]` as default.
--
-- NOTE: this commandline option is only provided as a convenience for users, 
-- and not as a way to verify validity of quicksort tests for different list 
-- types. for we know that as long as our sort functions are polymorphic with 
-- `Ord` class constraint, we can be sure that if quickcheck tests pass for one 
-- type, say `[Int]`, the sort function will work for all other types. this is 
-- one of the "free" theorems.
--
-- NOTE: to do different type annotations of a polymorphic function, we:
--    1. examined the problem: GHC complains if we do multiple type annotations 
--       of a polymorphic function (as done in code below). for an exact 
--       simulation of the problem, see ../notes/problem.hs
--    2. /u/ joel @ haskell irc solved this issue using `RankNTypes` extension; 
--       see /u/ joel's code @ https://paste.tomsmeding.com/qkTRpcFx; see also 
--       /u/ jon purdy @ https://tinyurl.com/5rs2nn2w (so)
--    3. Prem refined solution from (2) -- see ../notes/solution.hs.
--    4. based on 1-3, defined a newtype `Implementation` (polymorphic) in 
--       app/Types.hs, using `RankNTypes` extension.
--    5. modified `qsortImplementions` type signature in app/Quicksort.hs to 
--       include `Implementation`.
--    6. related changes made in app/Benchmark.hs (for compilation).
--    7. in code below, `runQC qsort opt` is repeated, but factoring out common 
--       items outside the `case` (i.e., `runQC qsort opt $ case opt of`) will 
--       lead to same problem in (1), b'cause `runQC` is polymorphic. the 
--       solution is same as in (2) - (3); that is: create a newtype, say `Foo`, 
--       to pack ` (i.e., generalize) `runQC qsort opt`, then unpack it (i.e., 
--       specialize) where needed. this is what /u/ jon prudy (see 2) has 
--       outlined, but it is not worth the effort here.
defaultMain :: IO ()
defaultMain = do
  opt :: Option <- option
  mapM_ (\(qsort :: Qsort, Implementation f) ->
       case opt of
            Default     -> runQC qsort opt (f :: [Int] -> [Int])
            Letter      -> runQC qsort opt (f :: [Char] -> [Char])
            MaybeInt    -> runQC qsort opt (f :: [Maybe Int] -> [Maybe Int])
            MaybeChar   -> runQC qsort opt (f :: [Maybe Char] -> [Maybe Char])
            EitherInt   -> runQC qsort opt (f :: [Either String Int] -> [Either String Int])
            EitherChar  -> runQC qsort opt (f :: [Either String Char] -> [Either String Char])
    ) qsortImplementations



