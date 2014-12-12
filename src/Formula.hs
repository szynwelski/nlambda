module Formula where

import Prelude hiding (or, and, not)
import qualified Data.SBV as SBV
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
(∀) x (Not f) = not $ (∃) x f
(∀) x f = ForAll x f

forall :: Variable -> Formula -> Formula
forall = (∀)

-- exists
(∃) :: Variable -> Formula -> Formula
(∃) x (Not f) = not $ (∀) x f
(∃) x f = Exists x f

exists :: Variable -> Formula -> Formula
exists = (∃)

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

freeVariables :: Formula -> [Variable]
freeVariables = Set.toList . freeVariablesSet

fromBool :: Bool -> Formula
fromBool True = T
fromBool False = F

----------------------------------------------------------------------------------------------------
-- Solving
----------------------------------------------------------------------------------------------------

-- transform formula to SBV types
interpret :: Formula -> Map.Map Variable SBV.SInt8 -> SBV.Symbolic SBV.SBool
interpret T env = return SBV.true
interpret F env = return SBV.false
interpret (Equals x1 x2) env = return $ env Map.! x1 SBV..== env Map.! x2
interpret (And f1 f2) env = do {if1 <- interpret f1 env; if2 <- interpret f2 env; return $ if1 SBV.&&& if2}
interpret (Or f1 f2) env = do {if1 <- interpret f1 env; if2 <- (interpret f2 env); return $ if1 SBV.||| if2}
interpret (Not f) env = do {ifo <- interpret f env; return $ SBV.bnot ifo}
interpret (Imply f1 f2) env = do {if1 <- interpret f1 env; if2 <- interpret f2 env; return $ if1 SBV.==> if2}
interpret (Equivalent f1 f2) env = do {if1 <- interpret f1 env; if2 <- interpret f2 env; return $ if1 SBV.<=> if2}
interpret (ForAll x f) env = do {xx <- SBV.forall (variableName x); interpret f (Map.insert x xx env)}
interpret (Exists x f) env = do {xx <- SBV.exists (variableName x); interpret f (Map.insert x xx env)}

quantifiedFormula :: (String -> SBV.Symbolic SBV.SInt8) -> Formula -> SBV.Symbolic SBV.SBool
quantifiedFormula q f = do
    let vs = freeVariables f
    syms <- mapM q $ fmap variableName vs
    interpret f $ Map.fromList (zip vs syms)

instance SBV.Provable Formula where
  forAll_ = quantifiedFormula SBV.forall
  forAll _ = quantifiedFormula SBV.forall
  forSome_ = quantifiedFormula SBV.exists
  forSome _ = quantifiedFormula SBV.exists

-- check is tautology
isTrue :: Formula -> IO Bool
isTrue T = return True
isTrue F = return False
isTrue f = do
    result <- (SBV.isSatisfiable Nothing) $ SBV.forAll_ f
    return $ fromJust result

unsafeIsTrue :: Formula -> Bool
unsafeIsTrue = unsafePerformIO . isTrue

-- check is contradiction
isFalse :: Formula -> IO Bool
isFalse F = return True
isFalse T = return False
isFalse f = do
    result <- (SBV.isSatisfiable Nothing) $ SBV.forSome_ f
    return $ if fromJust result then False else True

unsafeIsFalse :: Formula -> Bool
unsafeIsFalse = unsafePerformIO . isFalse

-- solve
solve :: Formula -> IO (Maybe Bool)
solve f = do
        trueCond <- isTrue f
        if trueCond
            then return (Just True)
            else do
                 falseCond <- isFalse f
                 if falseCond
                    then return (Just False)
                    else return Nothing

unsafeSolve :: Formula -> Maybe Bool
unsafeSolve f
         | unsafeIsTrue f  = Just True
         | unsafeIsFalse f = Just False
         | otherwise = Nothing

----------------------------------------------------------------------------------------------------
-- FormulaEq
----------------------------------------------------------------------------------------------------

class FormulaEq a where
    eq :: a -> a -> Formula

instance FormulaEq Formula where
    eq = iff

instance FormulaEq Variable where
    eq x1 x2 = if x1 == x2 then T else Equals x1 x2

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

instance (Eq a) => FormulaEq [a] where
    eq = formulaEqFromEq

instance FormulaEq () where
    eq = formulaEqFromEq

instance (Eq a, Eq b) => FormulaEq (a, b) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c) => FormulaEq (a, b, c) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d) => FormulaEq (a, b, c, d) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e) => FormulaEq (a, b, c, d, e) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => FormulaEq (a, b, c, d, e, f) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => FormulaEq (a, b, c, d, e, f, g) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => FormulaEq (a, b, c, d, e, f, g, h) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => FormulaEq (a, b, c, d, e, f, g, h, i) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => FormulaEq (a, b, c, d, e, f, g, h, i, j) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => FormulaEq (a, b, c, d, e, f, g, h, i, j, k) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => FormulaEq (a, b, c, d, e, f, g, h, i, j, k, l) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => FormulaEq (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n) => FormulaEq (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    eq = formulaEqFromEq

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => FormulaEq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    eq = formulaEqFromEq

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
eaf = (∃) x $ (∀) y cc
aaf = (∀) x $ (∀) y cc
eef = (∃) x $ (∃) y cc

