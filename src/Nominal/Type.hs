{-# LANGUAGE ConstraintKinds, DefaultSignatures, FlexibleContexts, CPP, TypeOperators #-}

module Nominal.Type where

import Data.Map (Map, findWithDefault)
import Data.Set (Set, empty, insert)
import Nominal.Formula
import Nominal.Formula.Operators (foldFormulaVariables, mapFormulaVariables)
import Nominal.Variable (Variable)
import Nominal.Variants (Variants, fromList, map, mapWithMono, prodWithMono, toList, variant, variantsRelation)
import Prelude hiding (and, map, not, or)
import GHC.Generics

----------------------------------------------------------------------------------------------------
-- BareNominalType
----------------------------------------------------------------------------------------------------

-- | Variables scope.
data Scope
    -- | All variables.
    = All
    -- | Only free variables.
    | Free

-- | Map function for variables from a scope.
type MapVarFun = (Scope, Variable -> Variable)
-- | Fold function for variables from a scope.
type FoldVarFun b = (Scope, Variable -> b -> b)

-- | Basic type in 'NLambda' required by most of functions in the module.
class BareNominalType a where
    -- | Checks equivalence of two given elements.
    eq :: a -> a -> Formula
    -- | If __a__ is a variant type then returns variants values.
    variants :: a -> Variants a
    -- | Map all variables from a given scope.
    mapVariables :: MapVarFun -> a -> a
    -- | Fold all variables form a given scope.
    foldVariables :: FoldVarFun b -> b -> a -> b

    -- We give default implementation if the type is generic.
    -- Some care must be taken when using the Ord instance as well,
    -- deriving it is fine. But a custom Ord instance might introduce bugs.
    -- See below for more details on this
    default eq :: (Generic a, GBareNominalType (Rep a)) => a -> a -> Formula
    eq x y = geq (from x) (from y)
    default variants :: (Generic a, GBareNominalType (Rep a)) => a -> Variants a
    variants x = mapWithMono to $ gvariants (from x)
    default mapVariables :: (Generic a, GBareNominalType (Rep a)) => MapVarFun -> a -> a
    mapVariables f x = to (gmapVariables f (from x))
    default foldVariables :: (Generic a, GBareNominalType (Rep a)) => FoldVarFun b -> b -> a -> b
    foldVariables f b x = gfoldVariables f b (from x)

-- Since I dropped the Ord constraint in the type class, we add it again
-- so that the other modules work just fine.
type NominalType a = (Ord a, BareNominalType a)

-- | Checks whether two elements are not equivalent.
neq :: BareNominalType a => a -> a -> Formula
neq x1 x2 = not $ eq x1 x2

----------------------------------------------------------------------------------------------------
-- Operations on all variables
----------------------------------------------------------------------------------------------------

collectWith :: (BareNominalType a, Ord b) => (Variable -> Maybe b) -> a -> Set b
collectWith cf = foldVariables (All, maybe id insert . cf) empty

getAllVariables :: BareNominalType a => a -> Set Variable
getAllVariables = foldVariables (All, insert) empty

mapVariablesIf :: BareNominalType a => (Variable -> Bool) -> (Variable -> Variable) -> a -> a
mapVariablesIf cf mf = mapVariables (All, \v -> if cf v then mf v else v)

replaceVariables :: BareNominalType a => Map Variable Variable -> a -> a
replaceVariables varsMap = mapVariables (All, \var -> findWithDefault var var varsMap)

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance BareNominalType Variable where
    eq = equals
    variants = variant
    mapVariables (_, f) = f
    foldVariables (_, f) acc v = f v acc

instance BareNominalType Formula where
    eq = iff
    variants = variant
    mapVariables (_, f) = mapFormulaVariables f
    foldVariables (_, f) = foldFormulaVariables f

instance (Ord a, BareNominalType a) => BareNominalType (Variants a) where
    eq = variantsRelation eq
    variants = map variant
    mapVariables f = fromList . mapVariables f . toList
    foldVariables f acc = foldl (foldVariables f) acc . toList

-- We will define trivial instances (i.e. where the action on Atoms does
-- not do anything at all). We could spell this out for each type, but
-- I am lazy, so I defined a macro. There are other ways to do this,
-- but this is a pretty obvious and readable one. This used to be the default
-- of BareNominalType, but I think it's better to make it explicit (i.e. not
-- default)
#define NOM_INSTANCE(foo)         \
instance BareNominalType foo where;   \
    eq x y = fromBool (x == y);   \
    variants = variant;           \
    mapVariables _ = id;          \
    foldVariables _ acc _ = acc;  \

-- All of these are nominal types with the trivial action
NOM_INSTANCE(Bool)
NOM_INSTANCE(Char)
NOM_INSTANCE(Double)
NOM_INSTANCE(Float)
NOM_INSTANCE(Int)
NOM_INSTANCE(Integer)
NOM_INSTANCE(Ordering)

-- Here are some generic instances. We do not have to provide any
-- implementation as they are dictated by their algebraic data type.
instance BareNominalType ()
instance BareNominalType a => BareNominalType [a]
instance BareNominalType a => BareNominalType (Maybe a)
instance (BareNominalType a, BareNominalType b) => BareNominalType (Either a b)
instance (BareNominalType a, BareNominalType b) => BareNominalType (a, b)
instance (BareNominalType a, BareNominalType b, BareNominalType c) => BareNominalType (a, b, c)
instance (BareNominalType a, BareNominalType b, BareNominalType c, BareNominalType d) => BareNominalType (a, b, c, d)
instance (BareNominalType a, BareNominalType b, BareNominalType c, BareNominalType d, BareNominalType e) => BareNominalType (a, b, c, d, e)
instance (BareNominalType a, BareNominalType b, BareNominalType c, BareNominalType d, BareNominalType e, BareNominalType f) => BareNominalType (a, b, c, d, e, f)
instance (BareNominalType a, BareNominalType b, BareNominalType c, BareNominalType d, BareNominalType e, BareNominalType f, BareNominalType g) => BareNominalType (a, b, c, d, e, f, g)

-- The generic interface on the functor level
-- Note that we do not require an ordering, because that is not easy
-- to do on the functor level (and also not really needed for this
-- class to make sense).
class GBareNominalType f where
    geq :: f a -> f a -> Formula
    gvariants :: f a -> Variants (f a)
    gmapVariables :: MapVarFun -> f a -> f a
    gfoldVariables :: FoldVarFun b -> b -> f a -> b

-- We will define an instance for 0, 1, +, * and constants (on the type
-- level). Then we can derive instances for any algebraic datatype.
-- The instances for Formula and Variant are special cases, and are
-- implemented by hand.

-- In order to implement some functions without an Ord instance, I
-- use the fact that constructors are monotone (i.e. if a < b then
-- C a < C b). This is true if the Ord instance is derived for a
-- algebraic data type. But it might not hold for other data types
-- (think of a data type Desc, where the Ord instance is given by the
-- reverse order). So one should be careful when deriving the Generic
-- instance, but giving a custom Ord instance!

-- For the void type (no constructor). This is a bit awkward, but valid.
instance GBareNominalType V1 where
    geq _ _ = true          -- can also be implemented with false, since
    gvariants = variant     -- there are no inhabitants anyways.
    gmapVariables _ = id
    gfoldVariables _ acc _ = acc

-- For the unit type (constructors without fields)
instance GBareNominalType U1 where
    geq _ _ = true
    gvariants = variant
    gmapVariables _ = id
    gfoldVariables _ acc _ = acc

-- For constants
instance BareNominalType c => GBareNominalType (K1 i c) where
    geq (K1 a) (K1 b) = eq a b
    gvariants (K1 a) = mapWithMono K1 $ variants a
    gmapVariables f (K1 a) = K1 $ mapVariables f a
    gfoldVariables f b (K1 a) = foldVariables f b a

-- For constructors with meta information (which we ignore)
instance GBareNominalType a => GBareNominalType (M1 i c a) where
    geq (M1 a) (M1 b) = geq a b
    gvariants (M1 a) = mapWithMono M1 $ gvariants a
    gmapVariables f (M1 a) = M1 $ gmapVariables f a
    gfoldVariables f b (M1 a) = gfoldVariables f b a

-- For sums
instance (GBareNominalType a, GBareNominalType b) => GBareNominalType (a :+: b) where
    geq (L1 a) (L1 b) = geq a b
    geq (R1 a) (R1 b) = geq a b
    geq _ _ = false
    gvariants (L1 a) = mapWithMono L1 $ gvariants a
    gvariants (R1 a) = mapWithMono R1 $ gvariants a
    gmapVariables f (L1 a) = L1 $ gmapVariables f a
    gmapVariables f (R1 a) = R1 $ gmapVariables f a
    gfoldVariables f b (L1 a) = gfoldVariables f b a
    gfoldVariables f b (R1 a) = gfoldVariables f b a

-- For products
instance (GBareNominalType a, GBareNominalType b) => GBareNominalType (a :*: b) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 /\ geq b1 b2
    gvariants (a :*: b) = prodWithMono (:*:) (gvariants a) (gvariants b)
    gmapVariables f (a :*: b) = gmapVariables f a :*: gmapVariables f b
    gfoldVariables f c (a :*: b) = gfoldVariables f (gfoldVariables f c a) b

