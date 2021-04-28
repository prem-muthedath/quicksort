{-# LANGUAGE RankNTypes #-}

-- | demo of how to do multiple type annoations of polymorphic functions using 
-- `RankNTypes` extension & polymorphic newtype.
--  1. see ./problem.hs for an exact simulation of the problem.
--  2. /u/ joel @ haskell irc solved this problem first, and this code is pretty 
--     much a copy of his solution with some minor refinements. see 
--     https://paste.tomsmeding.com/qkTRpcFx for /u/ joel's code.
--  3. see also /u/ jon purdy @ https://tinyurl.com/5rs2nn2w (so) for an 
--     explanation of this solution.
--  4. Prem Muthedath: authored all refinements of joel's code.

f :: Ord a => [a] -> [a]
f x = reverse x

h :: Ord a => [a] -> [a]
h x = tail x

g :: (Show a, Ord a) => ([a] -> [a]) -> [a] -> IO ()
g x y = putStrLn . show $ x y

newtype Lemma = Lemma (forall a. Ord a => [a] -> [a])

main :: IO ()
main = mapM_ (\(i, Lemma j) ->
  case i of
    0 -> g (j  :: [Int] -> [Int]) [1..5]
    1 -> g (j :: [Char] -> [Char]) "hello"
    _ -> error "bad"
  ) $ zipWith (\x y -> (x, y)) [0 :: Int ..] [Lemma f, Lemma h]

