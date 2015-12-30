module Nominal.Type where

import Data.Map (Map, findWithDefault)
import Data.Set (Set, elems, empty, insert)
import Nominal.Atom (Atom)
import Nominal.Conditional
import Nominal.Formula
import Nominal.Variable (Variable)
import Nominal.Variants (Variants, fromList, fromVariant, map, toList, variant, variantsRelation)
import Prelude hiding (and, map, not, or)

----------------------------------------------------------------------------------------------------
-- NominalType
----------------------------------------------------------------------------------------------------

data Scope = All | Free

type MapVarFun = (Scope, Variable -> Variable)
type FoldVarFun b = (Scope, Variable -> b -> b)

-- | Basic type in 'NLambda' required by most of functions in the module.
class Ord a => NominalType a where
    -- | Checks equivalence of two given elements.
    eq :: a -> a -> Formula
    eq x y = fromBool (x == y)
    variants :: a -> Variants a
    variants = variant
    mapVariables :: MapVarFun -> a -> a
    mapVariables _ = id
    foldVariables :: FoldVarFun b -> b -> a -> b
    foldVariables _ acc _ = acc

-- | Checks whether two elements are not equivalent.
neq :: NominalType a => a -> a -> Formula
neq x1 x2 = not $ eq x1 x2

----------------------------------------------------------------------------------------------------
-- Operations on all variables
----------------------------------------------------------------------------------------------------

collectWith :: (NominalType a, Ord b) => (Variable -> Maybe b) -> a -> Set b
collectWith cf = foldVariables (All, maybe id insert . cf) empty

getAllVariables :: (NominalType a) => a -> Set Variable
getAllVariables = foldVariables (All, insert) empty

mapVariablesIf :: NominalType a => (Variable -> Bool) -> (Variable -> Variable) -> a -> a
mapVariablesIf cf mf = mapVariables (All, \v -> if cf v then mf v else v)

replaceVariables :: NominalType a => Map Variable Variable -> a -> a
replaceVariables varsMap = mapVariables (All, \var -> findWithDefault var var varsMap)

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance NominalType Variable where
    eq = equals
    mapVariables (_, f) = f
    foldVariables (_, f) acc v = f v acc

instance NominalType Formula where
    eq = iff
    mapVariables (_, f) = mapFormulaVariables f
    foldVariables (_, f) = foldFormulaVariables f

instance NominalType Bool

instance NominalType Char

instance NominalType Double

instance NominalType Float

instance NominalType Int

instance NominalType Integer

instance NominalType Ordering

instance NominalType a => NominalType [a] where
    eq l1 l2 = if length l1 == length l2 then and $ zipWith eq l1 l2 else false
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
    mapVariables f (Left v) = Left (mapVariables f v)
    mapVariables f (Right v) = Right (mapVariables f v)
    foldVariables f acc (Left v) = foldVariables f acc v
    foldVariables f acc (Right v) = foldVariables f acc v
