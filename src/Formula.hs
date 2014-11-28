{-# LANGUAGE ScopedTypeVariables #-}

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

data Formula = T | F | And Formula Formula | Or Formula Formula | Not Formula
    |Imply Formula Formula | Equivalent Formula Formula | Equals Variable Variable

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

----------------------------------------------------------------------------------------------------
-- Formula instances
----------------------------------------------------------------------------------------------------

showFormula :: Formula -> String
showFormula f@(And f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Or f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Imply f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Equivalent f1 f2) = "(" ++ show f ++ ")"
showFormula f = show f

instance Show Formula where
    show T = "True"
    show F = "False"
    show (And f1 f2) = showFormula f1 ++ " ∧ " ++ showFormula f2
    show (Or f1 f2) = showFormula f1 ++ " ∨ " ++ showFormula f2
    show (Not (Equals x1 x2)) = show x1 ++ " ≠ " ++ show x2
    show (Not f) = "¬(" ++ show f ++ ")"
    show (Equals x1 x2) = show x1 ++ " = " ++ show x2
    show (Imply f1 f2) = showFormula f1 ++ " → " ++ showFormula f2
    show (Equivalent f1 f2) = showFormula f1 ++ " ↔ " ++ showFormula f2

instance Eq Formula where
    T == T = True
    F == F = True
    (And f11 f12) == (And f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (Or f11 f12) == (Or f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (Not f1) == (Not f2) = f1 == f2
    (Imply f11 f12) == (Imply f21 f22) = (f11 == f21) && (f12 == f22)
    (Equivalent f11 f12) == (Equivalent f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (Equals f11 f12) == (Equals f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    _ == _ = False

----------------------------------------------------------------------------------------------------
-- FormulaEq
----------------------------------------------------------------------------------------------------

class FormulaEq a where
    eq :: a -> a -> Formula

instance FormulaEq Formula where
    eq = iff

instance FormulaEq Variable where
    eq x1 x2 = if x1 == x2 then T else Equals x1 x2

----------------------------------------------------------------------------------------------------
-- Solving
----------------------------------------------------------------------------------------------------

-- variables in formula
variablesSet :: Formula -> Set.Set Variable
variablesSet T = Set.empty
variablesSet F = Set.empty
variablesSet (And f1 f2) = Set.union (variablesSet f1) (variablesSet f2)
variablesSet (Or f1 f2) = Set.union (variablesSet f1) (variablesSet f2)
variablesSet (Not f) = variablesSet f
variablesSet (Imply f1 f2) = Set.union (variablesSet f1) (variablesSet f2)
variablesSet (Equivalent f1 f2) = Set.union (variablesSet f1) (variablesSet f2)
variablesSet (Equals x1 x2) = Set.fromList [x1, x2]

variables :: Formula -> [Variable]
variables = Set.toList . variablesSet

-- transform formula to SBV types
interpret :: Formula -> Map.Map Variable SBV.SInt8 -> SBV.SBool
interpret T env = SBV.true
interpret F env = SBV.false
interpret (And f1 f2) env = (interpret f1 env) SBV.&&& (interpret f2 env)
interpret (Or f1 f2) env = (interpret f1 env) SBV.||| (interpret f2 env)
interpret (Not f) env = SBV.bnot (interpret f env)
interpret (Imply f1 f2) env = (interpret f1 env) SBV.==> (interpret f2 env)
interpret (Equivalent f1 f2) env = (interpret f1 env) SBV.<=> (interpret f2 env)
interpret (Equals x1 x2) env = (env Map.! x1) SBV..== (env Map.! x2)

quantifiedFormula :: (String -> SBV.Symbolic SBV.SInt8) -> Formula -> SBV.Symbolic SBV.SBool
quantifiedFormula q f = ssyms >>= (\syms -> return . interpret f $ Map.fromList (zip vs syms))
  where vs = variables f
        ssyms = mapM q $ fmap variableName vs

instance SBV.Provable Formula where
  forAll_ = quantifiedFormula SBV.forall
  forAll _ = quantifiedFormula SBV.forall
  forSome_ = quantifiedFormula SBV.exists
  forSome _ = quantifiedFormula SBV.exists

-- simple solving
isTrue :: Formula -> Bool
isTrue T = True
isTrue _ = False

isFalse :: Formula -> Bool
isFalse F = True
isFalse _ = False

-- SBV solving
solve :: Formula -> IO SBV.SatResult
solve = SBV.sat . SBV.forSome_

isTrueIO :: Formula -> IO Bool
isTrueIO T = return True
isTrueIO f = do
    result <- (SBV.isSatisfiable Nothing) $ SBV.forAll_ f
    return $ Data.Maybe.fromJust result

isFalseIO :: Formula -> IO Bool
isFalseIO F = return False
isFalseIO f = do
    result <- (SBV.isSatisfiable Nothing) $ SBV.forSome_ f
    return $ if Data.Maybe.fromJust result then False else True

-- unsafe SBV solving
unsafeIsTrue :: Formula -> Bool
unsafeIsTrue = unsafePerformIO . isTrueIO

unsafeIsFalse :: Formula -> Bool
unsafeIsFalse = unsafePerformIO . isFalseIO

----------------------------------------------------------------------------------------------------
-- If with formula
----------------------------------------------------------------------------------------------------

ifFormula :: Formula -> a -> a -> Maybe a
ifFormula f v1 v2
         | isTrue f  = Just v1
         | isFalse f = Just v2
         | otherwise = Nothing

ifFormulaIO :: Formula -> IO (Maybe (a -> a -> a))
ifFormulaIO f = do
        trueCond <- isTrueIO f
        if trueCond
            then return (Just const)
            else do
                 falseCond <- isFalseIO f
                 if falseCond
                    then return (Just $ flip const)
                    else return Nothing

unsafeIfFormula :: Formula -> a -> a -> Maybe a
unsafeIfFormula f v1 v2
         | unsafeIsTrue f  = Just v1
         | unsafeIsFalse f = Just v2
         | otherwise = Nothing

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
