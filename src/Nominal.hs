module Nominal where

import Data.List.Utils (join)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Formula
import Formula.Solver (unsafeSolve, solve)
import Prelude hiding (or, and, not, sum, map, filter)

----------------------------------------------------------------------------------------------------
-- Conditional
----------------------------------------------------------------------------------------------------

class Conditional a where
    iF :: Formula -> a -> a -> a

ifFormula :: Formula -> a -> a -> a -> a
ifFormula f x1 x2 x3 = case unsafeSolve f of
                        Just True -> x1
                        Just False -> x2
                        Nothing -> x3

instance Conditional Formula where
    iF f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

instance Conditional b => Conditional (a -> b) where
    iF f f1 f2 = \x -> iF f (f1 x) (f2 x)

instance (Conditional a) => Conditional [a] where
    iF f l1 l2 = ifFormula f l1 l2 (zipWith (iF f) l1 l2)

instance (Conditional a, Conditional b) => Conditional (a, b) where
    iF f (a1, b1) (a2, b2) = ifFormula f (a1, b1) (a2, b2) ((iF f a1 a2), (iF f b1 b2))

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
                                              _ -> " : " ++ show c)
instance Functor Variant where
    fmap f (Variant v c) = Variant (f v) c

instance Monad Variant where
    return = variant
    v >>= f = variantIf (condition v) (f $ value v)

instance FormulaEq a => FormulaEq (Variant a) where
    eq (Variant v1 c1) (Variant v2 c2) = (eq v1 v2) /\ c1 /\ c2
    freeVariables (Variant v c) = freeVariables v

----------------------------------------------------------------------------------------------------
-- Variants
----------------------------------------------------------------------------------------------------

data Variants a = Variants {variantsList :: [Variant a]}

variants :: a -> Variants a
variants x = Variants [Variant x T]

variantsIf :: Formula -> Variants a -> Variants a
variantsIf c = Variants . (fmap $ variantIf c) . variantsList

instance Show a => Show (Variants a) where
    show vs = join " | " (fmap show (variantsList vs))

instance Functor Variants where
    fmap f (Variants vs) = Variants $ fmap (fmap f) vs

instance Monad Variants where
    return = variants
    vs >>= f = Variants $ concat $ fmap variantsList $
                fmap (\v -> variantsIf (condition v) (value v)) (variantsList $ fmap f vs)

instance FormulaEq a => FormulaEq (Variants a) where
    eq (Variants vl1) (Variants vl2) = or [(eq v1 v2) | v1 <- vl1, v2 <- vl2]
    freeVariables (Variants vl) = freeVariables vl

instance Conditional (Variants a) where
    iF f vs1 vs2 = ifFormula f vs1 vs2 (Variants $ variantsIf f vs1 ++ variantsIf (not f) vs2)
        where variantsIf f = (fmap (variantIf f)) . variantsList

iFv :: Formula -> a -> a -> Variants a
iFv c x1 x2 = iF c (variants x1) (variants x2)

----------------------------------------------------------------------------------------------------
-- Atom
----------------------------------------------------------------------------------------------------

type Atom = Variants Variable

atom :: String -> Atom
atom name = Variants [variant $ Variable name]

----------------------------------------------------------------------------------------------------
-- Set element
----------------------------------------------------------------------------------------------------

data SetElement a = SetElement {
    elementValue :: a,
    elementForVariables :: Set.Set Variable,
    elementCondition :: Formula}

setElement :: (FormulaEq a) => a -> Set.Set Variable -> Formula -> SetElement a
setElement v vs c = SetElement v (Set.intersection valueFreeVars vs) cc
    where valueFreeVars = freeVariables v
          cc = Set.foldr (∃) c $ (Set.intersection (freeVariables c) (vs Set.\\ valueFreeVars))

simpleSetElement :: a -> SetElement a
simpleSetElement v = SetElement v Set.empty T

setElementIf :: Formula -> SetElement a -> SetElement a
setElementIf c e = SetElement (elementValue e) (elementForVariables e) (elementCondition e /\ c)

setElementCondition :: SetElement a -> Formula
setElementCondition e = Set.foldr (∃) (elementCondition e) (elementForVariables e)

setElementMap :: (FormulaEq b) => (a -> b) -> SetElement a -> SetElement b
setElementMap f (SetElement v vs c) = setElement (f v) vs c

setElementFreeVariables :: (FormulaEq a) => SetElement a -> Set.Set Variable
setElementFreeVariables (SetElement v vs c) = (Set.union (freeVariables v) (freeVariables c)) Set.\\ vs

instance Show a => Show (SetElement a) where
    show (SetElement v vs c) = show v ++ (if null conditionStr then "" else " :" ++ conditionStr)
                                where conditionStr = formulaStr ++ forVariablesStr
                                      formulaStr = (case c of T -> ""
                                                              _ -> " " ++ show c)
                                      forVariablesStr = if Set.null vs
                                                        then ""
                                                        else " for " ++ (join "," (fmap show $ Set.toList vs))
                                                                    ++ " ∊ 𝔸"

----------------------------------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------------------------------

data Set a = Set {elements :: [SetElement a]}

atomsSet :: String -> Set Atom
atomsSet name = Set [SetElement (atom name) (Set.singleton $ Variable name) T]

instance Show a => Show (Set a) where
    show s = "{" ++ (join ", " (fmap show (elements s))) ++ "}"

instance FormulaEq a => FormulaEq (Set a) where
    eq s1 s2 = (isSubset s1 s2) /\ (isSubset s2 s1)
    freeVariables (Set es) = Set.unions $ fmap setElementFreeVariables es

instance Conditional (Set a) where
    iF f s1 s2 = ifFormula f s1 s2 (Set $ setElementsIf f s1 ++ setElementsIf (not f) s2)
        where setElementsIf f = (fmap (setElementIf f)) . elements

----------------------------------------------------------------------------------------------------
-- nLambda
----------------------------------------------------------------------------------------------------

emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Formula
isEmpty s = and (fmap (not . setElementCondition) (elements s))

add :: a -> Set a -> Set a
add e = Set . (simpleSetElement e :) . elements

map :: (FormulaEq b) => (a -> b) -> Set a -> Set b
map f (Set es) = Set $ fmap (setElementMap f) es

sum :: Set (Set a) -> Set a
sum = Set . concat . (fmap setIf) . elements
        where setIf e = fmap (\ee -> SetElement
                                        (elementValue ee)
                                        (Set.union (elementForVariables e) (elementForVariables ee))
                                        ((elementCondition e) /\ (elementCondition ee)))
                             (elements (elementValue e))

----------------------------------------------------------------------------------------------------
-- Additional functions
----------------------------------------------------------------------------------------------------

just :: a -> Set a
just e = add e emptySet

filter :: (FormulaEq a) => (a -> Formula) -> Set a -> Set a
filter f s = sum $ map (\x -> (iF (f x) (just x) emptySet)) s

exists :: (FormulaEq a) => (a -> Formula) -> Set a -> Formula
exists f = not . isEmpty . (filter f)

forall :: (FormulaEq a) => (a -> Formula) -> Set a -> Formula
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
sa = atomsSet "a"
sb = atomsSet "b"
sc = atomsSet "c"
