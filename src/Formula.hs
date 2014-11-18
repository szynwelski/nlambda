{-# LANGUAGE ScopedTypeVariables #-}

module Formula where

import Prelude hiding (or, and, not)
import qualified Data.SBV as SBV
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)


----------------------------------------------------------------------------------------------------
-- FormulaEq
----------------------------------------------------------------------------------------------------

class FormulaEq a where
    eq :: a -> a -> Formula

----------------------------------------------------------------------------------------------------
-- Atom
----------------------------------------------------------------------------------------------------

newtype Atom = Atom {atomName :: String}

instance Show Atom where
    show = atomName

instance Eq Atom where
    a1 == a2 = (atomName a1) == (atomName a2)

instance Ord Atom where
    compare a1 a2 = compare (atomName a1) (atomName a2)

instance FormulaEq Atom where
    eq a1 a2 = if a1 == a2 then T else Equals a1 a2


----------------------------------------------------------------------------------------------------
-- Formula
----------------------------------------------------------------------------------------------------

data Formula = T | F | And Formula Formula | Or Formula Formula | Not Formula
    |Imply Formula Formula | Equivalent Formula Formula | Equals Atom Atom

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
    show (And f1 f2) = showFormula f1 ++ " /\\ " ++ showFormula f2
    show (Or f1 f2) = showFormula f1 ++ " \\/ " ++ showFormula f2
    show (Not (Equals x1 x2)) = show x1 ++ " != " ++ show x2
    show (Not f) = "!(" ++ show f ++ ")"
    show (Equals x1 x2) = show x1 ++ " = " ++ show x2
    show (Imply f1 f2) = showFormula f1 ++ " ==> " ++ showFormula f2
    show (Equivalent f1 f2) = showFormula f1 ++ " <==> " ++ showFormula f2

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
-- Solving
----------------------------------------------------------------------------------------------------

-- get atoms
atomsSet :: Formula -> Set.Set Atom
atomsSet T = Set.empty
atomsSet F = Set.empty
atomsSet (And f1 f2) = Set.union (atomsSet f1) (atomsSet f2)
atomsSet (Or f1 f2) = Set.union (atomsSet f1) (atomsSet f2)
atomsSet (Not f) = atomsSet f
atomsSet (Imply f1 f2) = Set.union (atomsSet f1) (atomsSet f2)
atomsSet (Equivalent f1 f2) = Set.union (atomsSet f1) (atomsSet f2)
atomsSet (Equals x1 x2) = Set.fromList [x1, x2]

atoms :: Formula -> [Atom]
atoms = Set.toList . atomsSet

interpret :: Formula -> Map.Map Atom SBV.SInt8 -> SBV.SBool
interpret T env = SBV.true
interpret F env = SBV.false
interpret (And f1 f2) env = (interpret f1 env) SBV.&&& (interpret f2 env)
interpret (Or f1 f2) env = (interpret f1 env) SBV.||| (interpret f2 env)
interpret (Not f) env = SBV.bnot (interpret f env)
interpret (Imply f1 f2) env = (interpret f1 env) SBV.==> (interpret f2 env)
interpret (Equivalent f1 f2) env = (interpret f1 env) SBV.<=> (interpret f2 env)
interpret (Equals a1 a2) env = (env Map.! a1) SBV..== (env Map.! a2)

quantifiedFormula :: (String -> SBV.Symbolic SBV.SInt8) -> Formula -> SBV.Symbolic SBV.SBool
quantifiedFormula q f = ssyms >>= (\syms -> return . interpret f $ Map.fromList (zip as syms))
  where as = atoms f
        ssyms = mapM q $ fmap atomName as

instance SBV.Provable Formula where
  forAll_ = quantifiedFormula SBV.forall
  forAll _ = quantifiedFormula SBV.forall
  forSome_ = quantifiedFormula SBV.exists
  forSome _ = quantifiedFormula SBV.exists

isTrue :: Formula -> Bool
isTrue T = True
isTrue _ = False

isFalse :: Formula -> Bool
isFalse F = True
isFalse _ = False

solve :: Formula -> IO SBV.SatResult
solve = SBV.sat . SBV.forSome_

isTrueIO :: Formula -> IO Bool
isTrueIO T = return $ True
isTrueIO f = do
    result <- (SBV.isSatisfiable Nothing) $ SBV.forAll_ f
    return $ Data.Maybe.fromJust result

--isFalseIO :: Formula -> IO (Maybe Bool)
--isFalseIO F = return Just False
isFalseIO f = do
    result <- (SBV.isSatisfiable Nothing) $ SBV.forSome_ f
    return $ if Data.Maybe.fromJust result then False else True

----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------
x = Atom "x"
y = Atom "y"
z = Atom "z"
c = eq x y
ce = (eq x y) /\ (eq y z) /\ (eq z x)
nce =  (eq x y) /\ (eq y z) /\ not (eq z x)
ice = (eq x y) /\ (eq y z) ==> (eq z x)
