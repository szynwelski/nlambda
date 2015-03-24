module Nominal.Type where

import Data.Map (Map, findWithDefault)
import Data.Set (Set, empty, insert)
import Formula (Formula, (/\), and, equals, false, freeVariables, foldFormulaVariables, fromBool,
                iff, mapFormulaVariables, not, or, true)
import Nominal.Variable (Variable)
import Nominal.Variants (Variants, fromList, map, toList, variant, variantsRelation)
import Prelude hiding (and, map, not, or)

----------------------------------------------------------------------------------------------------
-- NominalType
----------------------------------------------------------------------------------------------------

-- remove Show
class (Show a, Ord a) => NominalType a where
    eq :: a -> a -> Formula
    eq x y = fromBool (x == y)
    variants :: a -> Variants a
    variants = variant
    mapVariables :: (Variable -> Variable) -> a -> a
    mapVariables = const id
    foldVariables :: (Variable -> b -> b) -> b -> a -> b
    foldVariables _ acc _ = acc

neq :: NominalType a => a -> a -> Formula
neq x1 x2 = not $ eq x1 x2

collectWith :: (NominalType a, Ord b) => (Variable -> Maybe b) -> a -> Set b
collectWith cf = foldVariables (maybe id insert . cf) empty

mapVariablesIf :: NominalType a => (Variable -> Bool) -> (Variable -> Variable) -> a -> a
mapVariablesIf cf mf = mapVariables (\v -> if cf v then mf v else v)

replaceVariables :: NominalType a => Map Variable Variable -> a -> a
replaceVariables varsMap = mapVariables (\var -> findWithDefault var var varsMap)

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance NominalType Variable where
    eq = equals
    mapVariables = ($)
    foldVariables f acc v = f v acc

instance NominalType Formula where
    eq = iff
    mapVariables = mapFormulaVariables
    foldVariables = foldFormulaVariables

instance NominalType Bool

instance NominalType Char

instance NominalType Double

instance NominalType Float

instance NominalType Int

instance NominalType Integer

instance NominalType Ordering

instance NominalType a => NominalType [a] where
    eq l1 l2 = and $ zipWith eq l1 l2
    mapVariables f = fmap $ mapVariables f
    foldVariables f = foldl $ foldVariables f

instance NominalType ()

instance (NominalType a, NominalType b) => NominalType (a, b) where
    eq (a1, b1) (a2, b2) = (eq a1 a2) /\ (eq b1 b2)
    mapVariables f (a, b) = (mapVariables f a, mapVariables f b)
    foldVariables f acc (a, b) = foldVariables f (foldVariables f acc a) b

instance (NominalType a, NominalType b, NominalType c) => NominalType (a, b, c) where
    eq (a1, b1, c1) (a2, b2, c2) = (eq a1 a2) /\ (eq b1 b2) /\ (eq c1 c2)
    mapVariables f (a, b, c) = (mapVariables f a, mapVariables f b, mapVariables f c)
    foldVariables f acc (a, b, c) = foldVariables f (foldVariables f (foldVariables f acc a) b) c

instance NominalType a => NominalType (Variants a) where
    eq = variantsRelation eq
    variants = map variant
    mapVariables f = fromList . mapVariables f . toList
    foldVariables f acc = foldl (foldVariables f) acc . toList

instance NominalType a => NominalType (Maybe a) where
    eq Nothing Nothing = true
    eq (Just v1) (Just v2) = eq v1 v2
    eq _ _ = false
    mapVariables f = fmap $ mapVariables f
    foldVariables _ acc Nothing = acc
    foldVariables f acc (Just v) = foldVariables f acc v

instance (NominalType a, NominalType b) => NominalType (Either a b) where
    eq (Left v1) (Left v2) = eq v1 v2
    eq (Right v1) (Right v2) = eq v1 v2
    eq _ _ = false
    mapVariables f = fmap $ mapVariables f
    foldVariables f acc (Left v) = foldVariables f acc v
    foldVariables f acc (Right v) = foldVariables f acc v
