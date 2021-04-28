-- | simulate problem of multiple type annotations of polymorphic functions.
-- author: Prem Muthedath.

-- NOTE: this code does NOT compilie! reason: after the first type annotation of 
-- a polymorphic function, the compiler fixes the type, and if you do a second 
-- type annotation (see case expression in `main` below), the compiler reports a 
-- type mismatch!!
-- see ./solution.hs on how this problem can be solved.

f :: Ord a => [a] -> [a]
f x = reverse x

h :: Ord a => [a] -> [a]
h x = tail x

g :: Ord a => ([a] -> [a]) -> [a] -> IO ()
g x y = putStrLn . show $ x y

main :: IO ()
main = mapM_ (\(i, j) ->
  case i of
    0 -> g (j :: [Int] -> [Int]) [1..5]     -- this line compiles fine.
    1 -> g (j :: [Char] -> [Char]) "hello"  -- this line does not compile.
  ) [(0,f), (1, h)]

