{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
{-# LANGUAGE DefaultSignatures, FlexibleContexts, CPP, TypeOperators #-}
module Nominal.Type where

import Data.Map (Map, findWithDefault)
import Data.Set (Set, empty, insert)
import Nominal.Formula
import Nominal.Meta (WithMeta)
import Nominal.Meta.GHC.Classes (NLambda_Ord)
import Nominal.Variable (Var, Variable)
import Nominal.Variants (Variants, fromList, map, prod, toList, variant, variantsRelation)
import Prelude hiding (and, map, not, or)
import GHC.Generics

----------------------------------------------------------------------------------------------------
-- NominalType
----------------------------------------------------------------------------------------------------

-- | Basic type in 'NLambda' required by most of functions in the module.
-- The Ord instance is used for efficiency.
-- By using generics, one can derive instances of this class for custom
-- data types, like this:
--
-- > {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- > import GHC.Generics (Generic)
-- >
-- > data Foo = Foo1 Atom [Atom] | Foo2 (Set Atom)
-- >   deriving (Eq, Ord, Generic, NominalType)
class (Ord a, Var a) => NominalType a where
    -- | Checks equivalence of two given elements.
    eq :: a -> a -> Formula
    -- | If __a__ is a variant type then returns variants values.
    variants :: a -> Variants a

    -- We give default implementation if the type is generic.
    default eq :: (Generic a, GNominalType (Rep a)) => a -> a -> Formula
    eq x y = geq (from x) (from y)
    default variants :: (Generic a, GNominalType (Rep a)) => a -> Variants a
    variants x = fromList . gvmap to $ gvariants (from x)

-- | Checks whether two elements are not equivalent.
neq :: NominalType a => a -> a -> Formula
neq x1 x2 = not $ eq x1 x2

class (NLambda_Ord a, Var a) => NLambda_NominalType a where
    nlambda_eq :: WithMeta a -> WithMeta a -> WithMeta Formula
    nlambda_variants :: WithMeta a -> WithMeta (Variants a)

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance NominalType Variable where
    eq = equals
    variants = variant

instance NominalType Formula where
    eq = iff
    variants = variant

instance NominalType a => NominalType (Variants a) where
    eq = variantsRelation eq
    variants = map variant

-- We will define trivial instances (i.e. where the action on Atoms does
-- not do anything at all). We could spell this out for each type, but
-- I am lazy, so I defined a macro. There are other ways to do this,
-- but this is a pretty obvious and readable one. This used to be the default
-- of NominalType, but I think it's better to make it explicit (i.e. not
-- default)
#define NOM_INSTANCE(foo)         \
instance NominalType foo where;   \
    eq x y = fromBool (x == y);   \
    variants = variant;           \

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
instance NominalType ()
instance NominalType a => NominalType [a]
instance NominalType a => NominalType (Maybe a)
instance (NominalType a, NominalType b) => NominalType (Either a b)
instance (NominalType a, NominalType b) => NominalType (a, b)
instance (NominalType a, NominalType b, NominalType c) => NominalType (a, b, c)
instance (NominalType a, NominalType b, NominalType c, NominalType d) => NominalType (a, b, c, d)
instance (NominalType a, NominalType b, NominalType c, NominalType d, NominalType e) => NominalType (a, b, c, d, e)
instance (NominalType a, NominalType b, NominalType c, NominalType d, NominalType e, NominalType f) => NominalType (a, b, c, d, e, f)
instance (NominalType a, NominalType b, NominalType c, NominalType d, NominalType e, NominalType f, NominalType g) => NominalType (a, b, c, d, e, f, g)

-- The generic interface on the functor level
-- Note that we do not require an ordering, because that is not easy
-- to do on the functor level. To represet Variants here, we use a
-- simple list data type, then in the NominalType implementation it
-- will be put in a Data.Map for efficiency. This is equivalent to
-- the previous implementation (since I was using monotonic functions)
class GNominalType f where
    geq :: f a -> f a -> Formula
    gvariants :: f a -> GVariants (f a)

type GVariants a = [(a, Formula)]

gvmap :: (a -> b) -> GVariants a -> GVariants b
gvmap f ls = [(f x, form) | (x, form) <- ls]

-- We will define an instance for 0, 1, +, * and constants (on the type
-- level). Then we can derive instances for any algebraic datatype.
-- The instances for Formula and Variant are special cases, and are
-- implemented by hand above.

-- For the void type (no constructor). This is a bit awkward, but valid.
instance GNominalType V1 where
    geq _ _ = true
    gvariants x = [] -- void does not have any inhabitants

-- For the unit type (constructors without fields)
instance GNominalType U1 where
    geq _ _ = true
    gvariants x = [(x, true)]

-- For constants
instance NominalType c => GNominalType (K1 i c) where
    geq (K1 a) (K1 b) = eq a b
    gvariants (K1 a) = gvmap K1 . toList $ variants a

-- For constructors with meta information (which we ignore)
instance GNominalType a => GNominalType (M1 i c a) where
    geq (M1 a) (M1 b) = geq a b
    gvariants (M1 a) = gvmap M1 $ gvariants a

-- For sums
instance (GNominalType a, GNominalType b) => GNominalType (a :+: b) where
    geq (L1 a) (L1 b) = geq a b
    geq (R1 a) (R1 b) = geq a b
    geq _ _ = false
    gvariants (L1 a) = gvmap L1 $ gvariants a
    gvariants (R1 a) = gvmap R1 $ gvariants a

-- For products
instance (GNominalType a, GNominalType b) => GNominalType (a :*: b) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 /\ geq b1 b2
    gvariants (a :*: b) = [(x1 :*: x2, f1 /\ f2) | (x1, f1) <- gvariants a, (x2, f2) <- gvariants b]
