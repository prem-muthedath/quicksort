{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines & parses terminal commandline options for running quickcheck tests.
-- author: Prem Muthedath.

-- NOTE: code modeled after example at haskell wiki @ 
-- https://tinyurl.com/5d7pkh9k

module Terminal (option, Option(..), list) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.List (intercalate)
import qualified Data.Map as M

-- | cabal commandline usage information (for display to user).
-- for passing commandline option to a program run using `cabal v2-run`, see /u/ 
-- hvr @ https://github.com/haskell/cabal/issues/6074
usage :: String
usage = intercalate "\n" (header : body)
  where header :: String
        header = command <> "[" <> intercalate " | " uflags <> "]"
        command :: String
        command = "Usage: cabal v2-run :quicksort-test -- "
        uflags :: [String]
        uflags = M.elems flags <> [help]
        body :: [String]
        body = map (\x -> optionline x) options <> [helpline]
        optionline :: Option -> String
        optionline x = pad (flag x) <> "test with: " <> list x <> ending x
        ending :: Option -> String
        ending x = if x == Default then ". This is the default." else "."
        helpline :: String
        helpline = pad help <> "print this help message and exit."
        pad :: String -> String
        pad str = replicate 3 ' ' <> str <> replicate (fsize - length str) ' '
        fsize :: Int
        fsize = let offset = 5 in (maximum . map (\x -> length x) $ uflags) + offset

-- | all `Option` values.
options :: [Option]
options = [toEnum 0 :: Option ..]

-- | maps `Option` values to string representation of their list types.
lists :: M.Map Option String
lists = M.fromList [
    (Default,    "[Int]"),
    (Letter,     "[Char]"),
    (MaybeInt,   "[Maybe Int]"),
    (MaybeChar,  "[Maybe Char]"),
    (EitherInt,  "[Either String Int]"),
    (EitherChar, "[Either String Char]")
  ]

-- | returns string representation of list type mapped to an `Option`.
list :: Option -> String
list opt = get opt lists

-- | returns value mapped to an `Option`.
get :: Option -> M.Map Option String -> String
get key map_ = case (M.lookup key map_) of
  Just x  -> x
  Nothing -> error $ "no such key: " <> show key

-- | maps `Option` values to commandline flags.
flags :: M.Map Option String
flags = M.fromList [
    (Default,    "--int"),
    (Letter,     "--char"),
    (MaybeInt,   "--maybe-int"),
    (MaybeChar,  "--maybe-char"),
    (EitherInt,  "--either-int"),
    (EitherChar, "--either-char")
  ]

-- | commandline `help` flag.
help :: String
help = "--help"

-- | returns commandline flag mapped to an `Option`.
flag :: Option -> String
flag opt = get opt flags

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
  case () of
    _ | opt == []                 -> return Default
      | opt == [flag Default]     -> return Default
      | opt == [flag Letter]      -> return Letter
      | opt == [flag MaybeInt]    -> return MaybeInt
      | opt == [flag MaybeChar]   -> return MaybeChar
      | opt == [flag EitherInt]   -> return EitherInt
      | opt == [flag EitherChar]  -> return EitherChar
      | opt == [help]             -> putStrLn (usage) >> exitSuccess
      | otherwise                 -> putStrLn (bad opt) >> exitFailure
  where bad :: [String] -> String
        bad y = "Unrecognized option: " <> intercalate " " y <> "\n" <> usage


