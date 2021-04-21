-- | QuickCheck tests for haskell quicksort implementations.
-- author: Prem Muthedath.

module Tests (defaultMain) where

import Test.QuickCheck
import Data.List (nub, sort)

import Quicksort (qsortImplementations)

-- | `TestCase` type definition.
-- each `TestCase` instance refers to a specific quickcheck property; that is,
-- a quickcheck property is a function to test a given `TestCase` instance.
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
hasDups :: (Ord a) => [a] -> Bool
hasDups xs = length (nub xs) /= length xs

-- | `True` if list is ordered.
--  REF: chapter 11, real world haskell.
ordered :: [Int] -> Bool
ordered []       = True
ordered [_]      = True
ordered (x:y:ys) = x <= y && ordered (y:ys)

-- | quickcheck `classifications` for a property.
classifys :: Testable prop => [Int] -> prop -> Property
classifys xs = classify (xs==[]) "empty" .
               classify (length xs > 10) "has > 10 elements" .
               classify (ordered xs) "pre-ordered" .
               classify (hasDups xs) "has duplicates"

-- | some type synonyms.
type QsortImplementation = ([Int] -> [Int])
type QCProperty = [Int] -> Property

-- | quickcheck properties of all test cases.
qcProperties :: QsortImplementation -> [(TestCase, QCProperty)]
qcProperties f = map (\tc -> (tc, qcProperty tc)) testCases
  where qcProperty :: TestCase -> QCProperty
        qcProperty testCase = case testCase of
           Ordering   -> \xs -> classifys xs $ ordered (f xs)
           Invariance -> \xs -> classifys xs $ f xs == f (f xs)
           Model      -> \xs -> classifys xs $ f xs == sort xs
           Min        -> \_ -> forAll (listOf1 arbitrary) $
                         \xs -> classifys xs $ head (f xs) == minimum xs
           Max        -> \_ -> forAll (listOf1 arbitrary) $
                         \xs -> classifys xs $ last (f xs) == maximum xs

-- | run quickcheck tests for a specific haskell quicksort implementation.
runQC :: QsortImplementation -> IO ()
runQC f = mapM_(\(testCase, prop) ->
              do putStrLn $ show testCase
                 quickCheck prop
              ) $ qcProperties f

-- | run quickcheck tests for all haskell quicksort implementations.
defaultMain :: IO ()
defaultMain = mapM_ (\(name, implementation) ->
    do putStrLn $ "\n--- " ++ show name ++ " ---"
       runQC implementation
    ) qsortImplementations


