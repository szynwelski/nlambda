module Nominal.Type where

import Data.Map.Strict (assocs, keys, mapKeys)
import Data.Set (Set, elems, empty, singleton, union, unions)
import Formula
import Nominal.Variants
import Prelude hiding (and, or, not)

----------------------------------------------------------------------------------------------------
-- NominalType
----------------------------------------------------------------------------------------------------

class Ord a => NominalType a where
    eq :: a -> a -> Formula
    support :: a -> Set Variable
    support = const empty
    variants :: a -> Variants a
    variants = variant

instance NominalType Variable where
    eq x1 x2 = if x1 == x2 then T else Equals x1 x2
    support = singleton

instance NominalType Formula where
    eq = iff
    support = freeVariablesSet

nominalTypeFromEq :: (Eq a) => a -> a -> Formula
nominalTypeFromEq x y = fromBool (x == y)

instance NominalType Bool where
    eq = nominalTypeFromEq

instance NominalType Char where
    eq = nominalTypeFromEq

instance NominalType Double where
    eq = nominalTypeFromEq

instance NominalType Float where
    eq = nominalTypeFromEq

instance NominalType Int where
    eq = nominalTypeFromEq

instance NominalType Integer where
    eq = nominalTypeFromEq

instance NominalType Ordering where
    eq = nominalTypeFromEq

instance NominalType a => NominalType [a] where
    eq l1 l2 = and $ zipWith eq l1 l2
    support l = unions (fmap support l)

instance NominalType () where
    eq = nominalTypeFromEq

instance (NominalType a, NominalType b) => NominalType (a, b) where
    eq (a1, b1) (a2, b2) = (eq a1 a2) /\ (eq b1 b2)
    support (a, b) = union (support a) (support b)

instance (NominalType a, NominalType b, NominalType c) => NominalType (a, b, c) where
    eq (a1, b1, c1) (a2, b2, c2) = (eq a1 a2) /\ (eq b1 b2) /\ (eq c1 c2)
    support (a, b, c) = (support a) `union` (support b) `union` (support c)

instance NominalType a => NominalType (Variants a) where
    eq (Variants vs1) (Variants vs2) = or [(eq v1 v2) /\ c1 /\ c2 | (v1, c1) <- assocs vs1,
                                                                    (v2, c2) <- assocs vs2]
    support (Variants vs) = support $ keys vs
    variants (Variants vs) = Variants $ mapKeys variant vs
