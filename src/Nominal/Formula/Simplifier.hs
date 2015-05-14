module Nominal.Formula.Simplifier where

import Data.Set
import Nominal.Formula.Definition
import Nominal.Formula.Instances
import Nominal.Variable (Variable, isQuantificationVariable, quantificationVariable)
import Prelude hiding (foldl, map, null)

----------------------------------------------------------------------------------------------------
-- Signature of simplification function
----------------------------------------------------------------------------------------------------

simplifyFormula :: Formula -> Formula

----------------------------------------------------------------------------------------------------
-- Simplify constraint
----------------------------------------------------------------------------------------------------

simplifyFormula (Constraint r x1 x2) | x1 == x2 = if r == LessThan || r == GreaterThan then F else T

----------------------------------------------------------------------------------------------------
-- Simplify conjunction
----------------------------------------------------------------------------------------------------
-- TODO constraints

simplifyFormula (And fs) = checkSize $ fold addToAnd (And empty) fs

----------------------------------------------------------------------------------------------------
-- Simplify disjunction
----------------------------------------------------------------------------------------------------
-- TODO constraints

simplifyFormula (Or fs) = checkSize $ fold addToOr (Or empty) fs

----------------------------------------------------------------------------------------------------
-- Simplify negation
----------------------------------------------------------------------------------------------------
-- TODO constraints

simplifyFormula (Not T) = F
simplifyFormula (Not F) = T
simplifyFormula (Not (Not f)) = f
simplifyFormula (Not (Or fs)) = simplifyFormula $ And $ map (simplifyFormula . Not) fs
simplifyFormula (Not (And fs)) = simplifyFormula $ Or $ map (simplifyFormula . Not) fs
simplifyFormula (Not (ForAll x f)) = simplifyFormula $ Exists x (simplifyFormula $ Not f)
simplifyFormula (Not (Exists x f)) = simplifyFormula $ ForAll x (simplifyFormula $ Not f)

----------------------------------------------------------------------------------------------------
-- Simplify universal quantification
----------------------------------------------------------------------------------------------------
-- TODO constraints

simplifyFormula (ForAll _ T) = T
simplifyFormula (ForAll _ F) = F
simplifyFormula (ForAll x f)
    | not $ member x (freeVariables f) = f
    | not (isQuantificationVariable x) =
        let qv = quantificationVariable $ succ $ getQuantificationLevel f
        in ForAll qv (replaceFormulaVariable x qv f)

----------------------------------------------------------------------------------------------------
-- Simplify existential quantification
----------------------------------------------------------------------------------------------------
-- TODO constraints

simplifyFormula (Exists _ T) = T
simplifyFormula (Exists _ F) = F
simplifyFormula (Exists x f)
    | not $ member x (freeVariables f) = f
    | not (isQuantificationVariable x) =
        let qv = quantificationVariable $ succ $ getQuantificationLevel f
        in Exists qv (replaceFormulaVariable x qv f)

----------------------------------------------------------------------------------------------------
-- Otherwise
----------------------------------------------------------------------------------------------------

simplifyFormula f = f

----------------------------------------------------------------------------------------------------
-- Simplify binary operations (conjunction, disjunction)
----------------------------------------------------------------------------------------------------

checkSetSize :: Formula -> Formula -> Set Formula -> Formula
checkSetSize nullValue f fs
    | null fs = nullValue
    | size fs == 1 = findMin fs
    | otherwise = f

checkSize :: Formula -> Formula
checkSize f@(And fs) = checkSetSize T f fs
checkSize f@(Or fs) = checkSetSize F f fs
checkSize f = f

simpleNot = simplifyFormula . Not

-- conjunction

andContains :: Formula -> Formula -> Bool
andContains (And fs) f = member f fs
andContains _ _ = False

andDelete :: Formula -> Formula -> Formula
andDelete f (And fs) = checkSize $ And $ delete f fs
addDelete _ _ = error "Can only delete from AND"

addToAnd :: Formula -> Formula -> Formula
addToAnd T f = f
addToAnd f T = f
addToAnd F _ = F
addToAnd _ F = F
addToAnd (And fs) f = fold addToAnd f fs
addToAnd f (And fs)
    | null fs = f
    | member f fs = And fs
    | member (simpleNot f) fs = F
addToAnd f (And fs) | not $ null fs1 = addToAnd f (And fs2)
    where (fs1, fs2) = partition (flip orContains f) fs
addToAnd f (And fs) | not $ null fs1 = addToAnd f (And $ union (map (orDelete nf) fs1) fs2)
    where nf = simpleNot f
          (fs1, fs2) = partition (flip orContains $ nf) fs
addToAnd f1@(Or _) f2@(And fs) | not $ null $ fs1 = f2
    where (fs1, fs2) = partition (orContains f1) fs
addToAnd f1@(Or _) f2@(And fs) | not $ null $ fs1 = addToAnd (orDelete (simpleNot $ findMin fs1) f1) f2
        where (fs1, fs2) = partition (orContains f1 . simpleNot) fs
addToAnd f (And fs) = And $ insert f fs
addToAnd f1 f2 = addToAnd f1 (And $ singleton f2)

-- disjunction
orContains :: Formula -> Formula -> Bool
orContains (Or fs) f = member f fs
orContains _ _ = False

orDelete :: Formula -> Formula -> Formula
orDelete f (Or fs) = checkSize $ Or $ delete f fs
orDelete _ _ = error "Can only delete from OR"

addToOr :: Formula -> Formula -> Formula
addToOr T _ = T
addToOr _ T = T
addToOr F f = f
addToOr f F = f
addToOr (Or fs) f = fold addToOr f fs
addToOr f (Or fs)
    | null fs = f
    | member f fs = Or fs
    | member (simpleNot f) fs = T
addToOr f (Or fs) | not $ null fs1 = addToOr f (Or fs2)
    where (fs1, fs2) = partition (flip andContains f) fs
addToOr f (Or fs) | not $ null fs1 = addToOr f (And $ union (map (andDelete nf) fs1) fs2)
    where nf = simpleNot f
          (fs1, fs2) = partition (flip andContains $ nf) fs
addToOr f1@(And _) f2@(Or fs) | not $ null $ fs1 = f2
    where (fs1, fs2) = partition (andContains f1) fs
addToOr f1@(And _) f2@(Or fs) | not $ null $ fs1 = addToOr (andDelete (simpleNot $ findMin fs1) f1) f2
        where (fs1, fs2) = partition (andContains f1 . simpleNot) fs
addToOr f (Or fs) = Or $ insert f fs
addToOr f1 f2 = addToOr f1 (Or $ singleton f2)

----------------------------------------------------------------------------------------------------
-- Auxiliary function
----------------------------------------------------------------------------------------------------

foldFormulaVariables :: (Variable -> a -> a) -> a -> Formula -> a
foldFormulaVariables _ acc T = acc
foldFormulaVariables _ acc F = acc
foldFormulaVariables fun acc (Constraint _ x1 x2) = fun x2 $ fun x1 acc
foldFormulaVariables fun acc (And fs) = foldl (foldFormulaVariables fun) acc fs
foldFormulaVariables fun acc (Or fs) = foldl (foldFormulaVariables fun) acc fs
foldFormulaVariables fun acc (Not f) = foldFormulaVariables fun acc f
foldFormulaVariables fun acc (ForAll x f) = foldFormulaVariables fun (fun x acc) f
foldFormulaVariables fun acc (Exists x f) = foldFormulaVariables fun (fun x acc) f

mapFormulaVariables :: (Variable -> Variable) -> Formula -> Formula
mapFormulaVariables _ T = T
mapFormulaVariables _ F = F
mapFormulaVariables fun (Constraint r x1 x2) = simplifyFormula $ Constraint r (fun x1) (fun x2)
mapFormulaVariables fun (And fs) = simplifyFormula $ And $ map (mapFormulaVariables fun) fs
mapFormulaVariables fun (Or fs) = simplifyFormula $ Or $ map (mapFormulaVariables fun) fs
mapFormulaVariables fun (Not f) = simplifyFormula $ Not $ mapFormulaVariables fun f
mapFormulaVariables fun (ForAll x f) = simplifyFormula $ ForAll (fun x) (mapFormulaVariables fun f)
mapFormulaVariables fun (Exists x f) = simplifyFormula $ Exists (fun x) (mapFormulaVariables fun f)

replaceFormulaVariable :: Variable -> Variable -> Formula -> Formula
replaceFormulaVariable oldVar newVar = mapFormulaVariables (\var -> if oldVar == var then newVar else var)

getQuantificationLevel :: Formula -> Int
getQuantificationLevel T = 0
getQuantificationLevel F = 0
getQuantificationLevel (Constraint _ _ _) = 0
getQuantificationLevel (And fs) = maximum $ fmap getQuantificationLevel $ elems fs
getQuantificationLevel (Or fs) = maximum $ fmap getQuantificationLevel $ elems fs
getQuantificationLevel (Not f) = getQuantificationLevel f
getQuantificationLevel (ForAll x f) = succ $ getQuantificationLevel f
getQuantificationLevel (Exists x f) = succ $ getQuantificationLevel f

freeVariables :: Formula -> Set Variable
freeVariables T = empty
freeVariables F = empty
freeVariables (Constraint _ x1 x2) = fromList [x1, x2]
freeVariables (And fs) = unions $ fmap freeVariables $ elems fs
freeVariables (Or fs) = unions $ fmap freeVariables $ elems fs
freeVariables (Not f) = freeVariables f
freeVariables (ForAll x f) = delete x (freeVariables f)
freeVariables (Exists x f) = delete x (freeVariables f)
