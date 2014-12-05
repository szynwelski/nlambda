module Nominal where

import Prelude hiding (or, and, not, sum, map, filter)
import Formula
import Data.List.Utils (join)
import Data.Maybe (fromMaybe)

----------------------------------------------------------------------------------------------------
-- Conditional
----------------------------------------------------------------------------------------------------

class Conditional a where
    iF :: Formula -> a -> a -> a

instance Conditional Formula where
    iF f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

----------------------------------------------------------------------------------------------------
-- Variant
----------------------------------------------------------------------------------------------------

data Variant a = Variant {value :: a, condition :: Formula} deriving Eq

variant :: a -> Variant a
variant = flip Variant T

variantIf :: Formula -> Variant a -> Variant a
variantIf c v = Variant (value v) (condition v /\ c)

instance Show a => Show (Variant a) where
    show (Variant v c) = show v ++ (case c of T -> ""
                                              _ -> ":" ++ show c)

instance Functor Variant where
    fmap f (Variant v c) = Variant (f v) c

instance FormulaEq a => FormulaEq (Variant a) where
    eq (Variant v1 c1) (Variant v2 c2) = (eq v1 v2) /\ c1 /\ c2

----------------------------------------------------------------------------------------------------
-- Variants
----------------------------------------------------------------------------------------------------

data Variants a = Variants {variantsList :: [Variant a]}

variants :: a -> Variants a
variants x = Variants [Variant x T]

iFVariants :: (a -> [Variant b]) -> ([Variant b] -> a) -> Formula -> a -> a -> a
iFVariants toVariantList fromVariantList f x1 x2 = fromMaybe (ifVariants f x1 x2) (unsafeIfFormula f x1 x2)
    where toVariantsIf f x = fmap (variantIf f) (toVariantList x)
          ifVariants f x1 x2 = fromVariantList $ (toVariantsIf f x1) ++ (toVariantsIf (not f) x2)

instance Show a => Show (Variants a) where
    show vs = join " | " (fmap show (variantsList vs))

instance Functor Variants where
    fmap f (Variants vs) = Variants $ fmap (fmap f) vs

instance FormulaEq a => FormulaEq (Variants a) where
    eq (Variants vl1) (Variants vl2) = or [(eq v1 v2) | v1 <- vl1, v2 <- vl2]

instance Conditional (Variants a) where
    iF = iFVariants variantsList Variants

iFv :: Formula -> a -> a -> Variants a
iFv c x1 x2 = iF c (variants x1) (variants x2)

----------------------------------------------------------------------------------------------------
-- Atom
----------------------------------------------------------------------------------------------------

type Atom = Variants Variable

atom :: String -> Atom
atom name = Variants [variant $ Variable name]

----------------------------------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------------------------------

data Set a = Set {elements :: [Variant a]}

instance Show a => Show (Set a) where
    show s = "{" ++ (join ", " (fmap show (elements s))) ++ "}"

instance Functor Set where
    fmap f (Set es) = Set $ fmap (fmap f) es

instance FormulaEq a => FormulaEq (Set a) where
    eq s1 s2 = (isSubset s1 s2) /\ (isSubset s2 s1)

instance Conditional (Set a) where
    iF = iFVariants elements Set

----------------------------------------------------------------------------------------------------
-- Function
----------------------------------------------------------------------------------------------------

instance Conditional b => Conditional (a -> b) where
    iF f f1 f2 = \x -> iF f (f1 x) (f2 x)

----------------------------------------------------------------------------------------------------
-- nLambda
----------------------------------------------------------------------------------------------------

emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Formula
isEmpty s = and (fmap (not . condition) (elements s))

add :: a -> Set a -> Set a
add e = Set . ((variant e) :) . elements

map :: (a -> b) -> Set a -> Set b
map = fmap

sum :: Set (Set a) -> Set a
sum = Set . concat . (fmap setIf) . elements
        where setIf e = fmap (variantIf (condition e)) (elements (value e))

----------------------------------------------------------------------------------------------------
-- Additional functions
----------------------------------------------------------------------------------------------------

just :: a -> Set a
just e = add e emptySet

filter :: (a -> Formula) -> Set a -> Set a
filter f s = sum $ map (\x -> (iF (f x) (just x) emptySet)) s

exists :: (a -> Formula) -> Set a -> Formula
exists f = not . isEmpty . (filter f)

forall :: (a -> Formula) -> Set a -> Formula
forall f = isEmpty . (filter $ \x -> not (f x))

union :: Set a -> Set a -> Set a
union s1 s2 = sum (add s1 (add s2 emptySet))

contains :: FormulaEq a => Set a -> a -> Formula
contains s e = exists (eq e) s

isSubset :: FormulaEq a => Set a -> Set a -> Formula
isSubset s1 s2 = forall (contains s2) s1

----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------

a = atom "a"
b = atom "b"
c = atom "c"
cond = eq a b
at = iF cond a b
set1 = add a $ just b
set2 = just at
