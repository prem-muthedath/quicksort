module Tests (defaultMain) where

import Test.QuickCheck
import Data.List (nub, sort)

import Quicksort (qsortFunctions)

type QCSortFunction = ([Int] -> [Int])
type QCProperty = [Int] -> Property

data TestCase = Ordering | Invariance | Model | Min | Max deriving (Eq, Enum)

instance Show TestCase where
  show Ordering    = "*** property: ordered ***"
  show Invariance  = "*** property: invariance ***"
  show Model       = "*** property: model sort equivalence ***"
  show Min         = "*** property: minimum ***"
  show Max         = "*** property: maximum ***"

testCases :: [TestCase]
testCases = [toEnum 0 :: TestCase ..]

qcProperties :: QCSortFunction -> [(TestCase, QCProperty)]
qcProperties f = map (\tc -> (tc, qcProperty tc)) testCases
  where qcProperty :: TestCase -> QCProperty
        qcProperty testCase xs = let result = f xs in
            case testCase of
                 Ordering   -> classifys xs $ ordered result
                 Invariance -> classifys xs $ result == f result
                 Model      -> classifys xs $ result == sort xs
                 Min        -> forAll (listOf1 arbitrary) $
                               \ys -> classifys ys $ head (f ys) == minimum ys
                 Max        -> forAll (listOf1 arbitrary) $
                               \ys -> classifys ys $ last (f ys) == maximum ys

classifys :: Testable prop => [Int] -> prop -> Property
classifys xs = classify (xs==[]) "empty" .
               classify (length xs > 10) "has > 10 elements" .
               classify (ordered xs) "pre-ordered" .
               classify (hasDups xs) "has duplicates"

hasDups :: (Ord a) => [a] -> Bool
hasDups xs = length (nub xs) /= length xs

ordered :: [Int] -> Bool
ordered []       = True
ordered [_]      = True
ordered (x:y:ys) = x <= y && ordered (y:ys)

runQC :: QCSortFunction -> IO ()
runQC f = mapM_(\(testCase, prop) ->
              do putStrLn $ show testCase
                 quickCheck prop
              ) $ qcProperties f

defaultMain :: IO ()
defaultMain = mapM_ (\(a, f) ->
    do putStrLn $ "\n--- " ++ show a ++ " ---"
       runQC f
    ) qsortFunctions


