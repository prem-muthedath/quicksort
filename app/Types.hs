{-# LANGUAGE RankNTypes #-}

-- | Types for quicksort.
-- author: Prem Muthedath.

module Types where

-- | `Qsort` -- specifies haskell quicksort implementations.
-- each value refers to a specfic haskell quicksort implementation.
-- except `Classic`, values named after implementation authors.
data Qsort = Classic | Diller | Leal | LealM | Bird deriving (Eq, Show, Enum)

-- | all `Qsort` values.
qsorts :: [Qsort]
qsorts = [toEnum 0 :: Qsort ..]

-- | type synonym for a string that represents a name.
type Name = String

-- | specifies a quicksort function.
-- NOTE: `forall` requires `RankNTypes` language extension.
-- this type introduced to solve the problem described in ../notes/problem.hs
newtype Implementation = Implementation (forall a. Ord a => [a] -> [a])


