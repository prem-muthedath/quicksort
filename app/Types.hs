-- | Types for quicksort.
-- author: Prem Muthedath.

module Types where

-- | `Qsort` -- specifies haskell quicksort implementations.
-- each value refers to a specfic haskell quicksort implementation.
-- except `Classic`, values named after implementation authors.
data Qsort = Classic | Diller | Leal | LealM | Bird deriving (Eq, Show, Enum)

-- | type synonym for a string that represents a name.
type Name = String


