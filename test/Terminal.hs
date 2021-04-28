{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines & parses terminal commandline options for running quickcheck tests.
-- author: Prem Muthedath.

-- NOTE: code modeled after example at haskell wiki @ 
-- https://tinyurl.com/5d7pkh9k

module Terminal (option, Option(..)) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.List (intercalate)

-- | cabal commandline usage information (for display to user).
-- for passing commandline option to a program run using `cabal v2-run`, see /u/ 
-- hvr @ https://github.com/haskell/cabal/issues/6074
usage :: String
usage = intercalate "\n" [
    "Usage: cabal v2-run :quicksort-test -- " <> options,
    "   --int            test with: [Int].  This is also the default.",
    "   --char           test with: [Char].",
    "   --maybe-int      test with: [Maybe Int].",
    "   --maybe-char     test with: [Maybe Char].",
    "   --either-int     test with: [Either String Int].",
    "   --either-char    test with: [Either String Char].",
    "   --help           print this help message and exit."
  ] where options :: String
          options = intercalate " | " [
              "[--help",
              "--int",
              "--char",
              "--maybe-int",
              "--maybe-char",
              "--either-int",
              "--either-char]"
            ]

-- | specifies list data type options for quickcheck testing.
data Option
    = Default       -- [Int]
    | Letter        -- [Char]
    | MaybeInt      -- [Maybe Int]
    | MaybeChar     -- [Maybe Char]
    | EitherInt     -- [Either String Int]
    | EitherChar    -- [Either String Char]
    deriving (Eq, Enum)

-- | `Show` instance for `Option`.
instance Show Option where
  show Default    = "testing with: [Int]"
  show Letter     = "testing with: [Char]"
  show MaybeInt   = "testing with: [Maybe Int]"
  show MaybeChar  = "testing with: [Maybe Char]"
  show EitherInt  = "testing with: [Either String Int]"
  show EitherChar = "testing with: [Either String Char]"

-- | returns user-specified commandline option (i.e., the input list data type) 
-- for quickcheck tests. if none specified, returns `Default`.
option :: IO Option
option = do
  opt :: [String] <- getArgs
  case opt of
       []                 -> return Default
       ["--int"]          -> return Default
       ["--char"]         -> return Letter
       ["--maybe-int"]    -> return MaybeInt
       ["--maybe-char"]   -> return MaybeChar
       ["--either-int"]   -> return EitherInt
       ["--either-char"]  -> return EitherChar
       ["--help"]       -> putStrLn (usage) >> exitSuccess
       x                -> putStrLn (bad x) >> exitFailure
  where bad :: [String] -> String
        bad y = "Unrecognized option: " <> intercalate " " y <> "\n" <> usage


