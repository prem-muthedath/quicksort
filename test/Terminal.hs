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
clFlags = M.elems flags <> [help]

-- | list type string representations associated with `Option` values.
lists :: M.Map Option String
lists = M.fromList $
  map (\x -> case x of
      Default     -> (x, "[Int]")
      Letter      -> (x, "[Char]")
      MaybeInt    -> (x, "[Maybe Int]")
      MaybeChar   -> (x, "[Maybe Char]")
      EitherInt   -> (x, "[Either String Int]")
      EitherChar  -> (x, "[Either String Char]")
  ) options

-- | list type string representation associated with an `Option`.
list :: Option -> String
list opt = get opt lists

-- | returns value associated with an `Option`.
get :: Option -> M.Map Option String -> String
get key map_ =
  case (M.lookup key map_) of
       Just x  -> x
       Nothing -> error $ "no such key: " <> show key

-- | commandline flags associated with `Option` values. `help` flag not in.
flags :: M.Map Option String
flags = M.fromList $
  map (\x -> case x of
      Default     -> (x, "--int")
      Letter      -> (x, "--char")
      MaybeInt    -> (x, "--maybe-int")
      MaybeChar   -> (x, "--maybe-char")
      EitherInt   -> (x, "--either-int")
      EitherChar  -> (x, "--either-char")
  ) options

-- | commandline `help` flag.
help :: String
help = "--help"

-- | commandline flag associated with an `Option`.
flag :: Option -> String
flag opt = get opt flags

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


