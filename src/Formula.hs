module Formula where

import Prelude hiding (or, and, not)
import Data.Set (Set, delete, empty, fromList, member, union)
import Nominal.Variable (Variable, quantificationVariable)

----------------------------------------------------------------------------------------------------
-- Formula
----------------------------------------------------------------------------------------------------

data Formula
    = T
    | F
    | Equals Variable Variable
    | And Formula Formula
    | Or Formula Formula
    | Not Formula
    | Imply Formula Formula
    | Equivalent Formula Formula
    | ForAll Variable Formula
    | Exists Variable Formula

-- true
true :: Formula
true = T

-- false
false :: Formula
false = F

-- equals
equals :: Variable -> Variable -> Formula
equals x1 x2 = if x1 == x2 then T else Equals x1 x2

-- and
(/\) :: Formula -> Formula -> Formula
T /\ f = f
F /\ _ = F
f /\ T = f
_ /\ F = F
(Not f1) /\ (Not f2) = (not (f1 \/ f2))
f1 /\ f2
    | f1 == f2       = f1
    | (not f1) == f2 = F
    | otherwise      = And f1 f2

and :: [Formula] -> Formula
and [] = T
and fs = foldr1 (/\) fs

-- or
(\/) :: Formula -> Formula -> Formula
F \/ f = f
T \/ _ = T
f \/ F = f
_ \/ T = T
(Not f1) \/ (Not f2) = (not (f1 /\ f2))
f1 \/ f2
    | f1 == f2       = f1
    | (not f1) == f2 = T
    | otherwise      = Or f1 f2

or :: [Formula] -> Formula
or [] = F
or fs = foldr1 (\/) fs

-- not
not :: Formula -> Formula
not F = T
not T = F
not (Not f) = f
not f = Not f

-- imply
(==>) :: Formula -> Formula -> Formula
T ==> f = f
F ==> _ = T
_ ==> T = T
f ==> F = f
(Not f1) ==> (Not f2) = f2 ==> f1
f1 ==> f2
    | f1 == f2       = T
    | (not f1) == f2 = f2
    | otherwise      = Imply f1 f2

implies :: Formula -> Formula -> Formula
implies = (==>)

-- equivalent
(<==>) :: Formula -> Formula -> Formula
T <==> f = f
F <==> f = not f
f <==> T = f
f <==> F = not f
(Not f1) <==> (Not f2) = f1 <==> f2
f1 <==> f2
    | f1 == f2       = T
    | (not f1) == f2 = F
    | otherwise      = Equivalent f1 f2

iff :: Formula -> Formula -> Formula
iff = (<==>)

-- for all
(∀) :: Variable -> Formula -> Formula
(∀) _ T = T
(∀) _ F = F
(∀) x (Not f) = not $ (∃) x f
(∀) x f = quantificationFormula ForAll x f

forAllVars :: Variable -> Formula -> Formula
forAllVars = (∀)

-- exists
(∃) :: Variable -> Formula -> Formula
(∃) _ T = T
(∃) _ F = F
(∃) x (Not f) = not $ (∀) x f
(∃) x f = quantificationFormula Exists x f

existsVar :: Variable -> Formula -> Formula
existsVar = (∃)

----------------------------------------------------------------------------------------------------
-- Formula instances
----------------------------------------------------------------------------------------------------

-- Show

showFormula :: Formula -> String
showFormula f@(And f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Or f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Imply f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Equivalent f1 f2) = "(" ++ show f ++ ")"
showFormula (ForAll x f) = "∀" ++ show x ++ "(" ++ show f ++ ")"
showFormula (Exists x f) = "∃" ++ show x ++ "(" ++ show f ++ ")"
showFormula f = show f

instance Show Formula where
    show T = "true"
    show F = "false"
    show (Equals x1 x2) = show x1 ++ " = " ++ show x2
    show (And f1 f2) = showFormula f1 ++ " ∧ " ++ showFormula f2
    show (Or f1 f2) = showFormula f1 ++ " ∨ " ++ showFormula f2
    show (Not (Equals x1 x2)) = show x1 ++ " ≠ " ++ show x2
    show (Not f) = "¬(" ++ show f ++ ")"
    show (Imply f1 f2) = showFormula f1 ++ " → " ++ showFormula f2
    show (Equivalent f1 f2) = showFormula f1 ++ " ↔ " ++ showFormula f2
    show (ForAll x f) = "∀" ++ show x ++ " " ++ show f
    show (Exists x f) = "∃" ++ show x ++ " " ++ show f

-- Ord

compareEquivalentPairs :: (Ord a) => (a, a) -> (a, a) -> Ordering
compareEquivalentPairs (x11, x12) (x21, x22) =
    compareSortedPairs
        (if x11 <= x12 then (x11, x12) else (x12, x11))
        (if x21 <= x22 then (x21, x22) else (x22, x21))

compareSortedPairs :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareSortedPairs (x11, x12) (x21, x22) =
    let compareFirst = compare x11 x21
    in if compareFirst == EQ
         then compare x12 x22
         else compareFirst

instance Ord Formula where
    compare T T = EQ
    compare T _ = GT
    compare _ T = LT

    compare F F = EQ
    compare F _ = GT
    compare _ F = LT

    compare (Equals x1 y1) (Equals x2 y2) = compareEquivalentPairs (x1, y1) (x2, y2)
    compare (Equals _ _) _ = GT
    compare _ (Equals _ _) = LT

    compare (And f11 f12) (And f21 f22) = compareEquivalentPairs (f11, f12) (f21, f22)
    compare (And _ _) _ = GT
    compare _ (And _ _) = LT

    compare (Or f11 f12) (Or f21 f22) = compareEquivalentPairs (f11, f12) (f21, f22)
    compare (Or _ _) _ = GT
    compare _ (Or _ _) = LT

    compare (Not f1) (Not f2) = compare f1 f2
    compare (Not _) _ = GT
    compare _ (Not _) = LT

    compare (Imply f11 f12) (Imply f21 f22) = compareSortedPairs (f11, f12) (f21, f22)
    compare (Imply _ _) _ = GT
    compare _ (Imply _ _) = LT

    compare (Equivalent f11 f12) (Equivalent f21 f22) = compareEquivalentPairs (f11, f12) (f21, f22)
    compare (Equivalent _ _) _ = GT
    compare _ (Equivalent _ _) = LT

    compare (ForAll x1 f1) (ForAll x2 f2) =  compareSortedPairs (x1, f1) (x2, f2)
    compare (ForAll _ _) _ = GT
    compare _ (ForAll _ _) = LT

    compare (Exists x1 f1) (Exists x2 f2) =  compareSortedPairs (x1, f1) (x2, f2)

-- Eq

instance Eq Formula where
    f1 == f2 = (compare f1 f2) == EQ

----------------------------------------------------------------------------------------------------
-- Auxiliary functions
----------------------------------------------------------------------------------------------------

freeVariables :: Formula -> Set Variable
freeVariables T = empty
freeVariables F = empty
freeVariables (And f1 f2) = union (freeVariables f1) (freeVariables f2)
freeVariables (Or f1 f2) = union (freeVariables f1) (freeVariables f2)
freeVariables (Not f) = freeVariables f
freeVariables (Imply f1 f2) = union (freeVariables f1) (freeVariables f2)
freeVariables (Equivalent f1 f2) = union (freeVariables f1) (freeVariables f2)
freeVariables (Equals x1 x2) = fromList [x1, x2]
freeVariables (ForAll x f) = delete x (freeVariables f)
freeVariables (Exists x f) = delete x (freeVariables f)

mapFormulaVariables :: (Variable -> Variable) -> Formula -> Formula
mapFormulaVariables _ T = T
mapFormulaVariables _ F = F
mapFormulaVariables fun (Equals x1 x2) = fun x1 `equals` fun x2
mapFormulaVariables fun (And f1 f2) = mapFormulaVariables fun f1 /\ mapFormulaVariables fun f2
mapFormulaVariables fun (Or f1 f2) = mapFormulaVariables fun f1 \/ mapFormulaVariables fun f2
mapFormulaVariables fun (Not f) = not $ mapFormulaVariables fun f
mapFormulaVariables fun (Imply f1 f2) = mapFormulaVariables fun f1 ==> mapFormulaVariables fun f2
mapFormulaVariables fun (Equivalent f1 f2) = mapFormulaVariables fun f1 <==> mapFormulaVariables fun f2
mapFormulaVariables fun (ForAll x f) = (∀) (fun x) (mapFormulaVariables fun f)
mapFormulaVariables fun (Exists x f) = (∃) (fun x) (mapFormulaVariables fun f)

replaceFormulaVariable :: Variable -> Variable -> Formula -> Formula
replaceFormulaVariable oldVar newVar = mapFormulaVariables (\var -> if oldVar == var then newVar else var)

foldFormulaVariables :: (Variable -> a -> a) -> a -> Formula -> a
foldFormulaVariables _ acc T = acc
foldFormulaVariables _ acc F = acc
foldFormulaVariables fun acc (Equals x1 x2) = fun x2 $ fun x1 acc
foldFormulaVariables fun acc (And f1 f2) = foldFormulaVariables fun (foldFormulaVariables fun acc f1) f2
foldFormulaVariables fun acc (Or f1 f2) = foldFormulaVariables fun (foldFormulaVariables fun acc f1) f2
foldFormulaVariables fun acc (Not f) = foldFormulaVariables fun acc f
foldFormulaVariables fun acc (Imply f1 f2) = foldFormulaVariables fun (foldFormulaVariables fun acc f1) f2
foldFormulaVariables fun acc (Equivalent f1 f2) = foldFormulaVariables fun (foldFormulaVariables fun acc f1) f2
foldFormulaVariables fun acc (ForAll x f) = foldFormulaVariables fun (fun x acc) f
foldFormulaVariables fun acc (Exists x f) = foldFormulaVariables fun (fun x acc) f

getQuantificationLevel :: Formula -> Int
getQuantificationLevel T = 0
getQuantificationLevel F = 0
getQuantificationLevel (And f1 f2) = max (getQuantificationLevel f1) (getQuantificationLevel f2)
getQuantificationLevel (Or f1 f2) = max (getQuantificationLevel f1) (getQuantificationLevel f2)
getQuantificationLevel (Not f) = getQuantificationLevel f
getQuantificationLevel (Imply f1 f2) = max (getQuantificationLevel f1) (getQuantificationLevel f2)
getQuantificationLevel (Equivalent f1 f2) = max (getQuantificationLevel f1) (getQuantificationLevel f2)
getQuantificationLevel (Equals _ _) = 0
getQuantificationLevel (ForAll x f) = succ $ getQuantificationLevel f
getQuantificationLevel (Exists x f) = succ $ getQuantificationLevel f

quantificationFormula :: (Variable -> Formula -> Formula) -> Variable -> Formula -> Formula
quantificationFormula makeFormula x f = if member x $ freeVariables f
                                        then let qv = quantificationVariable $ succ $ getQuantificationLevel f
                                             in makeFormula qv (replaceFormulaVariable x qv f)
                                        else f

fromBool :: Bool -> Formula
fromBool True = T
fromBool False = F
