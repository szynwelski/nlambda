{-# LANGUAGE DeriveGeneric #-}

module NLambdaTest where

import NLambda
import Prelude (Eq, Ord, Show)
import GHC.Generics (Generic)

-- hiding all
import qualified Prelude

a = atom "a"
b = atom "b"
c = atom "c"

-- Define some simple custom type
data MyAutomaton = InitialState | S2 Atom | S3 (Atom, Atom)
  deriving (Eq, Ord, Show, Generic)

instance BareNominalType MyAutomaton

test = filter (\x -> exists (\a -> x `eq` S2 a) atoms) (singleton InitialState) -- {}
test2 = leastSupport (S3 (a, b)) -- [a, b]

-- Recursive data type
data Something = X (Set (Atom, Something)) | Y (Set Atom)
  deriving (Eq, Ord, Show, Generic)

instance BareNominalType Something

something1 = X (map (\a -> (a, Y (singleton c))) atoms)
something2 = X (singleton (a, something1))
something3 = X (singleton (b, something1))
test3 = leastSupport something2 -- [a, c]
test4 = something2 `eq` something3 -- a = b

infi = X (map (\a -> (a, infi)) atoms)
infi2 = X (singleton (a, infi2))
test5 = leastSupport infi -- Does not terminate
test6 = leastSupport infi2 -- Does not terminate
test7 = infi `eq` infi2 -- Does not terminate :(
