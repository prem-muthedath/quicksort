{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines & parses terminal commandline options for running quickcheck tests.
-- author: Prem Muthedath.

-- NOTE: code modeled after example at haskell wiki @ 
-- https://tinyurl.com/5d7pkh9k

module Terminal (option, Option(..), list) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.List (intercalate)

-- | cabal commandline usage information (for display to user).
-- for passing commandline option to a program run using `cabal v2-run`, see /u/ 
-- hvr @ https://github.com/haskell/cabal/issues/6074
usage :: String
usage = intercalate "\n" (header : body)
  where header :: String
        header = "Usage: cabal v2-run :quicksort-test -- " <> "[" <> intercalate " | " clFlags <> "]"
        body :: [String]
        body = map (\x -> optionline x) options <> [helpline]
        optionline :: Option -> String
        optionline x = pad (flag x) <> "test with: " <> list x <> ending x
        ending :: Option -> String
        ending x = if x == Default then ". This is the default." else "."
        helpline :: String
        helpline = pad help <> "print this help message and exit."
        pad :: String -> String
        pad flag' = replicate 3 ' ' <> flag' <> replicate (width - length flag') ' '
        width :: Int
        width = let offset = 5 in (maximum . map (\x -> length x) $ clFlags) + offset

-- | commandline flags. includes `help` flag.
clFlags :: [String]
clFlags = map (\x -> flag x) options  <> [help]

-- | list type string representation associated with an `Option`.
list :: Option -> String
list Default     =  "[Int]"
list Letter      =  "[Char]"
list MaybeInt    =  "[Maybe Int]"
list MaybeChar   =  "[Maybe Char]"
list EitherInt   =  "[Either String Int]"
list EitherChar  =  "[Either String Char]"

-- | commandline flag associated with an `Option`.
flag :: Option -> String
flag Default     =  "--int"
flag Letter      =  "--char"
flag MaybeInt    =  "--maybe-int"
flag MaybeChar   =  "--maybe-char"
flag EitherInt   =  "--either-int"
flag EitherChar  =  "--either-char"

-- | commandline `help` flag.
help :: String
help = "--help"

-- | all `Option` values.
options :: [Option]
options = [toEnum 0 :: Option ..]

-- | specifies list data types as options for quickcheck testing.
data Option
  = Default       -- [Int]
  | Letter        -- [Char]
  | MaybeInt      -- [Maybe Int]
  | MaybeChar     -- [Maybe Char]
  | EitherInt     -- [Either String Int]
  | EitherChar    -- [Either String Char]
  deriving (Eq, Show, Ord, Enum)

-- | returns user-specified commandline option (i.e., the input list data type) 
-- for quickcheck tests. if none specified, returns `Default`.
option :: IO Option
option = do
  opt :: [String] <- getArgs
  let match :: [Option] = filter (\x -> opt == [flag x]) options
  if length match == 1 then return $ head match
  else case () of
        _ | opt == []     -> return Default
          | opt == [help] -> putStrLn (usage) >> exitSuccess
          | otherwise     -> putStrLn (bad opt) >> exitFailure
  where bad :: [String] -> String
        bad y = "Unrecognized option: " <> intercalate " " y <> "\n" <> usage


