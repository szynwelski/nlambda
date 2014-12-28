module Formula where

import Prelude hiding (or, and, not)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)


----------------------------------------------------------------------------------------------------
-- Variable
----------------------------------------------------------------------------------------------------

newtype Variable = Variable {variableName :: String} deriving (Eq, Ord)

instance Show Variable where
    show = variableName

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
    | (not f1) == f2 = Not f1
    | otherwise      = Imply f1 f2

implies :: Formula -> Formula -> Formula
implies = (==>)

-- equivalent
(<==>) :: Formula -> Formula -> Formula
T <==> T = T
F <==> F = T
F <==> T = F
T <==> F = F
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
(∀) x f = ForAll x f

forallVar :: Variable -> Formula -> Formula
forallVar = (∀)

-- exists
(∃) :: Variable -> Formula -> Formula
(∃) _ T = T
(∃) _ F = F
(∃) x (Not f) = not $ (∀) x f
(∃) x f = Exists x f

existsVar :: Variable -> Formula -> Formula
existsVar = (∃)

----------------------------------------------------------------------------------------------------
-- Formula instances
----------------------------------------------------------------------------------------------------

showFormula :: Formula -> String
showFormula f@(And f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Or f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Imply f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Equivalent f1 f2) = "(" ++ show f ++ ")"
showFormula (ForAll x f) = "∀" ++ show x ++ "(" ++ show f ++ ")"
showFormula (Exists x f) = "∃" ++ show x ++ "(" ++ show f ++ ")"
showFormula f = show f

instance Show Formula where
    show T = "True"
    show F = "False"
    show (Equals x1 x2) = show x1 ++ " = " ++ show x2
    show (And f1 f2) = showFormula f1 ++ " ∧ " ++ showFormula f2
    show (Or f1 f2) = showFormula f1 ++ " ∨ " ++ showFormula f2
    show (Not (Equals x1 x2)) = show x1 ++ " ≠ " ++ show x2
    show (Not f) = "¬(" ++ show f ++ ")"
    show (Imply f1 f2) = showFormula f1 ++ " → " ++ showFormula f2
    show (Equivalent f1 f2) = showFormula f1 ++ " ↔ " ++ showFormula f2
    show (ForAll x f) = "∀" ++ show x ++ " " ++ show f
    show (Exists x f) = "∃" ++ show x ++ " " ++ show f

instance Eq Formula where
    T == T = True
    F == F = True
    (Equals f11 f12) == (Equals f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (And f11 f12) == (And f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (Or f11 f12) == (Or f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (Not f1) == (Not f2) = f1 == f2
    (Imply f11 f12) == (Imply f21 f22) = (f11 == f21) && (f12 == f22)
    (Equivalent f11 f12) == (Equivalent f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (ForAll x1 f1) == (ForAll x2 f2) = x1 == x2 && f1 == f2
    (Exists x1 f1) == (Exists x2 f2) = x1 == x2 && f1 == f2
    _ == _ = False

----------------------------------------------------------------------------------------------------
-- Auxiliary functions
----------------------------------------------------------------------------------------------------

freeVariablesSet :: Formula -> Set.Set Variable
freeVariablesSet T = Set.empty
freeVariablesSet F = Set.empty
freeVariablesSet (And f1 f2) = Set.union (freeVariablesSet f1) (freeVariablesSet f2)
freeVariablesSet (Or f1 f2) = Set.union (freeVariablesSet f1) (freeVariablesSet f2)
freeVariablesSet (Not f) = freeVariablesSet f
freeVariablesSet (Imply f1 f2) = Set.union (freeVariablesSet f1) (freeVariablesSet f2)
freeVariablesSet (Equivalent f1 f2) = Set.union (freeVariablesSet f1) (freeVariablesSet f2)
freeVariablesSet (Equals x1 x2) = Set.fromList [x1, x2]
freeVariablesSet (ForAll x f) = Set.delete x (freeVariablesSet f)
freeVariablesSet (Exists x f) = Set.delete x (freeVariablesSet f)

fromBool :: Bool -> Formula
fromBool True = T
fromBool False = F

----------------------------------------------------------------------------------------------------
-- FormulaEq
----------------------------------------------------------------------------------------------------

class FormulaEq a where
    eq :: a -> a -> Formula
    freeVariables :: a -> Set.Set Variable
    freeVariables = const Set.empty

instance FormulaEq Formula where
    eq = iff
    freeVariables = freeVariablesSet

instance FormulaEq Variable where
    eq x1 x2 = if x1 == x2 then T else Equals x1 x2
    freeVariables = Set.singleton

formulaEqFromEq :: (Eq a) => a -> a -> Formula
formulaEqFromEq x y = fromBool (x == y)

instance FormulaEq Bool where
    eq = formulaEqFromEq

instance FormulaEq Char where
    eq = formulaEqFromEq

instance FormulaEq Double where
    eq = formulaEqFromEq

instance FormulaEq Float where
    eq = formulaEqFromEq

instance FormulaEq Int where
    eq = formulaEqFromEq

instance FormulaEq Integer where
    eq = formulaEqFromEq

instance FormulaEq Ordering where
    eq = formulaEqFromEq

instance (FormulaEq a) => FormulaEq [a] where
    eq l1 l2 = and $ zipWith eq l1 l2
    freeVariables l = Set.unions (fmap freeVariables l)

instance FormulaEq () where
    eq = formulaEqFromEq

instance (FormulaEq a, FormulaEq b) => FormulaEq (a, b) where
    eq (a1, b1) (a2, b2) = (eq a1 a2) /\ (eq b1 b2)
    freeVariables (a, b) = Set.union (freeVariables a) (freeVariables b)

instance (FormulaEq a, FormulaEq b, FormulaEq c) => FormulaEq (a, b, c) where
    eq (a1, b1, c1) (a2, b2, c2) = (eq a1 a2) /\ (eq b1 b2) /\ (eq c1 c2)
    freeVariables (a, b, c) = Set.unions [(freeVariables a), (freeVariables b), (freeVariables c)]

----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------
x = Variable "x"
y = Variable "y"
z = Variable "z"
cc = eq x y
ce = (eq x y) /\ (eq y z) /\ (eq z x)
nce =  (eq x y) /\ (eq y z) /\ not (eq z x)
ice = (eq x y) /\ (eq y z) ==> (eq z x)
af = (∀) x cc
ef = (∃) x cc
aef = (∀) x $ (∃) y cc
naef = not aef
eaf = (∃) x $ (∀) y cc
aaf = (∀) x $ (∀) y cc
eef = (∃) x $ (∃) y cc

