module Nominal where

import Prelude hiding (or, and, not, sum, map, filter)
import Formula
import Data.List.Utils (join)
import Data.Maybe (fromMaybe)

----------------------------------------------------------------------------------------------------
-- Variant
----------------------------------------------------------------------------------------------------

data Variant a = Variant {value :: a, condition :: Formula} deriving Eq

variant :: a -> Variant a
variant = flip Variant T

variantIf :: Formula -> Variant a -> Variant a
variantIf c v = Variant (value v) (condition v /\ c)

variantsIf :: Formula -> [Variant a] -> [Variant a]
variantsIf = fmap . ($) . variantIf

instance Show a => Show (Variant a) where
    show (Variant v c) = show v ++ (if (isTrue c) then "" else " : " ++ show c)

instance Functor Variant where
    fmap f (Variant v c) = Variant (f v) c

instance FormulaEq a => FormulaEq (Variant a) where
    eq (Variant v1 c1) (Variant v2 c2) = (eq v1 v2) /\ c1 /\ c2

----------------------------------------------------------------------------------------------------
-- Variants
----------------------------------------------------------------------------------------------------

class Variants a where
    iF :: Formula -> a -> a -> a

instance Variants Formula where
    iF f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

----------------------------------------------------------------------------------------------------
-- Atom
----------------------------------------------------------------------------------------------------

data Atom = Atom {atomVariants :: [Variant Variable]}

atom :: String -> Atom
atom a = Atom [Variant (Variable a) T]

instance Show Atom where
    show a = join " | " (fmap show (atomVariants a))

instance Variants Atom where
    iF f a1 a2 = fromMaybe (ifAtom f a1 a2) (ifFormula f a1 a2)
      where atomIf f = (variantsIf f) . atomVariants
            ifAtom f a1 a2 = Atom $ (atomIf f a1) ++ (atomIf (not f) a2)

instance FormulaEq Atom where
    eq (Atom av1) (Atom av2) = or [(eq a1 a2) | a1 <- av1, a2 <- av2]

----------------------------------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------------------------------

data Set a = Set {elements :: [Variant a]}

instance Show a => Show (Set a) where
    show s = "{" ++ (join ", " (fmap show (elements s))) ++ "}"

instance Variants (Set a) where
    iF f s1 s2 = fromMaybe (ifSet f s1 s2) (ifFormula f s1 s2)
      where setIf f = (variantsIf f) . elements
            ifSet f s1 s2 = Set $ (setIf f s1) ++ (setIf (not f) s2)

instance Functor Set where
    fmap f (Set es) = Set $ fmap (fmap f) es

instance FormulaEq a => FormulaEq (Set a) where
    eq s1 s2 = (isSubset s1 s2) /\ (isSubset s2 s1)

----------------------------------------------------------------------------------------------------
-- Function
----------------------------------------------------------------------------------------------------

instance Variants b => Variants (a -> b) where
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
        where setIf e = variantsIf (condition e) $ elements (value e)

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
