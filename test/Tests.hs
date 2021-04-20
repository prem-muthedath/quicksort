module Tests (defaultMain) where

import Test.QuickCheck
import Data.List (nub, sort)

import Quicksort hiding (defaultMain)


data Qsort = Diller | Leal | LealM | Bird | BirdM deriving (Eq, Show, Enum)

qsort :: Ord a => Qsort -> ([a] -> [a])
qsort Diller  = qsortDiller
qsort Leal    = qsortLeal
qsort LealM   = qsortLealM
qsort Bird    = qsortBird
qsort BirdM   = qsortBirdM

data TestCase = Ordering | Invariance | Library deriving (Eq, Enum)

instance Show TestCase where
  show Ordering    = "*** property: ordered ***"
  show Invariance  = "*** property: invariance ***"
  show Library     = "*** property: library sort ***"

type SortFunction = ([Int] -> [Int])
type QCProperty = [Int] -> Property

properties :: SortFunction -> [(TestCase, QCProperty)]
properties f = map (\tc -> (tc, qcProperty tc)) testCases
  where testCases :: [TestCase]
        testCases = [toEnum 0 :: TestCase ..]
        qcProperty :: TestCase -> QCProperty
        qcProperty testCase xs = let result = f xs
                                 in classify_ xs $
                                    case testCase of
                                         Ordering   -> ordered result
                                         Invariance -> result == f result
                                         Library    -> result == sort xs

hasDups :: (Ord a) => [a] -> Bool
hasDups xs = length (nub xs) /= length xs

ordered :: [Int] -> Bool
ordered []       = True
ordered [_]      = True
ordered (x:y:ys) = x <= y && ordered (y:ys)

classify_ :: Testable prop => [Int] -> prop -> Property
classify_ xs = classify (xs==[]) "empty" .
               classify (length xs > 10) "has > 10 elements" .
               classify (hasDups xs) "has duplicates"

runQC :: Qsort -> IO ()
runQC x = do let sortFunction = qsort x
             putStrLn $ "\n--- " ++ show x ++ " ---"
             mapM_(\(testCase, prop) ->
              do putStrLn $ show testCase
                 quickCheck prop
              ) $ properties sortFunction

defaultMain :: IO ()
defaultMain = mapM_ (\f -> runQC f) [toEnum 0 :: Qsort ..]


