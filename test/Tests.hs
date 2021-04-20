module Tests (defaultMain) where

import Test.QuickCheck
import Data.List (nub, sort)

import Quicksort (qsortFunctions)

type QCSortFunction = ([Int] -> [Int])
type QCProperty = [Int] -> Property

data TestCase = Ordering | Invariance | Library | Min deriving (Eq, Enum)

instance Show TestCase where
  show Ordering    = "*** property: ordered ***"
  show Invariance  = "*** property: invariance ***"
  show Library     = "*** property: library sort ***"
  show Min         = "*** property: minimum ***"

testCases :: [TestCase]
testCases = [toEnum 0 :: TestCase ..]

qcProperties :: QCSortFunction -> [(TestCase, QCProperty)]
qcProperties f = map (\tc -> (tc, qcProperty tc)) testCases
  where qcProperty :: TestCase -> QCProperty
        qcProperty testCase xs = let result = f xs in
            case testCase of
                 Ordering   -> classify_ xs $ ordered result
                 Invariance -> classify_ xs $ result == f result
                 Library    -> classify_ xs $ result == sort xs
                 Min        -> not (null xs) ==> classify_ xs $ head result == minimum xs

classify_ :: Testable prop => [Int] -> prop -> Property
classify_ xs = classify (xs==[]) "empty" .
               classify (length xs > 10) "has > 10 elements" .
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
defaultMain = mapM_ (\(a,f) ->
    do putStrLn $ "\n--- " ++ show a ++ " ---"
       runQC f
    ) qsortFunctions


