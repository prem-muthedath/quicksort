{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines & parses terminal commandline options for running quickcheck tests.
-- author: Prem Muthedath.

-- NOTE: design uses ideas from haskell wiki @ https://tinyurl.com/5d7pkh9k

module Terminal (option, Option(..)) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.List (intercalate)

-- | commandline flags. includes `help` flag.
flags :: [String]
flags = map (\x -> flag x) options  <> [help]

-- | cabal commandline usage information (for display to user).
-- for passing commandline option to a program run using `cabal v2-run`, see /u/ 
-- hvr @ https://github.com/haskell/cabal/issues/6074
usage :: IO ()
usage =
  do putStr $ "Usage: cabal v2-run :quicksort-test -- "
     putStrLn $ "[" <> intercalate " | " flags <> "]"
     mapM_ (\x -> putStrLn $ optionline x) options
     putStrLn $ pad help <> "print this help message and exit."
  where optionline :: Option -> String
        optionline x = pad (flag x) <> "test with: " <> show x <> ending x
        ending :: Option -> String
        ending x = if x == Default then ". This is the default." else "."
        pad :: String -> String
        pad flag' = replicate 3 ' ' <> flag' <> replicate (width - length flag') ' '
        width :: Int
        width = let offset = 5 in (maximum . map (\x -> length x) $ flags) + offset

-- | commandline `help` flag.
help :: String
help = "--help"

-- | commandline flag associated with an `Option`.
flag :: Option -> String
flag Default     =  "--int"
flag Letter      =  "--char"
flag MaybeInt    =  "--maybe-int"
flag MaybeChar   =  "--maybe-char"
flag EitherInt   =  "--either-int"
flag EitherChar  =  "--either-char"

-- | all `Option` values.
options :: [Option]
options = [toEnum 0 :: Option ..]

-- | `Show` instance of `Option`.
instance Show Option where
  show Default     =  "[Int]"
  show Letter      =  "[Char]"
  show MaybeInt    =  "[Maybe Int]"
  show MaybeChar   =  "[Maybe Char]"
  show EitherInt   =  "[Either String Int]"
  show EitherChar  =  "[Either String Char]"

-- | specifies list data types as options for quickcheck testing.
data Option
  = Default       -- [Int]
  | Letter        -- [Char]
  | MaybeInt      -- [Maybe Int]
  | MaybeChar     -- [Maybe Char]
  | EitherInt     -- [Either String Int]
  | EitherChar    -- [Either String Char]
  deriving (Eq, Enum)

-- | returns user-specified commandline option (i.e., the input list data type) 
-- for quickcheck tests. if none specified, returns `Default`.
option :: IO Option
option = do
  opt :: [String] <- getArgs
  let match :: [Option] = filter (\x -> opt == [flag x]) options
  if length match == 1 then return $ head match
  else case () of
        _ | opt == []     -> return Default
          | opt == [help] -> usage >> exitSuccess
          | otherwise     -> bad opt >> exitFailure
  where bad :: [String] -> IO ()
        bad y = do putStrLn $ "Unrecognized option: " <> intercalate " " y
                   usage


