module Nominal.Type where

import Control.Arrow ((***))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Formula
import Nominal.Variants
import Prelude hiding (and, or, not)

----------------------------------------------------------------------------------------------------
-- NominalType
----------------------------------------------------------------------------------------------------

class Ord a => NominalType a where
    eq :: a -> a -> Formula
    eq x y = fromBool (x == y)
    neq :: a -> a -> Formula
    neq x y = not (eq x y)
    support :: a -> Set Variable
    support = const Set.empty
    variants :: a -> Variants a
    variants = variant
    countSetLevel :: Int -> a -> a
    countSetLevel level = id

instance NominalType Variable where
    eq x1 x2 = if x1 == x2 then T else Equals x1 x2
    support = Set.singleton

instance NominalType Formula where
    eq = iff
    support = freeVariablesSet

instance NominalType Bool

instance NominalType Char

instance NominalType Double

instance NominalType Float

instance NominalType Int

instance NominalType Integer

instance NominalType Ordering

instance NominalType a => NominalType [a] where
    eq l1 l2 = and $ zipWith eq l1 l2
    support l = Set.unions (fmap support l)
    countSetLevel level = fmap (countSetLevel level)

instance NominalType ()

instance (NominalType a, NominalType b) => NominalType (a, b) where
    eq (a1, b1) (a2, b2) = (eq a1 a2) /\ (eq b1 b2)
    support (a, b) = Set.union (support a) (support b)
    countSetLevel level (a, b) = (countSetLevel level a, countSetLevel level b)

instance (NominalType a, NominalType b, NominalType c) => NominalType (a, b, c) where
    eq (a1, b1, c1) (a2, b2, c2) = (eq a1 a2) /\ (eq b1 b2) /\ (eq c1 c2)
    support (a, b, c) = (support a) `Set.union` (support b) `Set.union` (support c)
    countSetLevel level (a, b, c) = (countSetLevel level a, countSetLevel level b, countSetLevel level c)

instance NominalType a => NominalType (Variants a) where
    eq (Variants vs1) (Variants vs2) = or [(eq v1 v2) /\ c1 /\ c2 | (v1, c1) <- Map.assocs vs1,
                                                                    (v2, c2) <- Map.assocs vs2]
    support (Variants vs) = support $ Map.keys vs
    variants = Nominal.Variants.map variant
    countSetLevel level = Nominal.Variants.map (countSetLevel level)
