module Nominal.Formula.Operators where

import Data.Set (Set, delete, elems, fromList, insert, map, notMember, union)
import Nominal.Formula.Definition
import Nominal.Formula.Constructors
import Nominal.Variable (Variable, isQuantificationVariable, quantificationVariable)
import Prelude hiding (map, not)

----------------------------------------------------------------------------------------------------
-- And
----------------------------------------------------------------------------------------------------

createAnd :: FormulaStructure -> FormulaStructure -> FormulaStructure
createAnd (And fs) f = And (insert f fs)
createAnd f (And fs) = And (insert f fs)
createAnd f1 f2 = And (fromList [f1, f2])

(/\) :: Formula -> Formula -> Formula
Formula _ F /\ _ = false
_ /\ Formula _ F = false
Formula _ T /\ f = f
f /\ Formula _ T = f
(Formula fv1 f1) /\ (Formula fv2 f2) = Formula (union fv1 fv2) (createAnd f1 f2)

and :: [Formula] -> Formula
and [] = true
and fs = foldr1 (/\) fs

----------------------------------------------------------------------------------------------------
-- Or
----------------------------------------------------------------------------------------------------

createOr :: FormulaStructure -> FormulaStructure -> FormulaStructure
createOr (Or fs) f = Or (insert f fs)
createOr f (Or fs) = Or (insert f fs)
createOr f1 f2 = Or (fromList [f1, f2])

(\/) :: Formula -> Formula -> Formula
Formula _ T \/ _ = true
_ \/ Formula _ T = true
Formula _ F \/ f = f
f \/ Formula _ F = f
(Formula fv1 f1) \/ (Formula fv2 f2) = Formula (union fv1 fv2) (createOr f1 f2)

or :: [Formula] -> Formula
or [] = false
or fs = foldr1 (\/) fs

----------------------------------------------------------------------------------------------------
-- Not
----------------------------------------------------------------------------------------------------

createNot :: FormulaStructure -> FormulaStructure
createNot T = F
createNot F = T
createNot (Constraint LessThan x1 x2) = Constraint GreaterEquals x1 x2
createNot (Constraint LessEquals x1 x2) = Constraint GreaterThan x1 x2
createNot (Constraint Equals x1 x2) = Constraint NotEquals x1 x2
createNot (Constraint NotEquals x1 x2) = Constraint Equals x1 x2
createNot (Constraint GreaterThan x1 x2) = Constraint LessEquals x1 x2
createNot (Constraint GreaterEquals x1 x2) = Constraint LessThan x1 x2
createNot (And fs) = Or $ map createNot fs
createNot (Or fs) = And $ map createNot fs
createNot (Not f) = f
createNot (Exists x f) = ForAll x $ createNot f
createNot (ForAll x f) = Exists x $ createNot f

not :: Formula -> Formula
not (Formula fv f) = Formula fv (createNot f)

----------------------------------------------------------------------------------------------------
-- Imply
----------------------------------------------------------------------------------------------------

infix 8 ==>
(==>) :: Formula -> Formula -> Formula
f1 ==> f2 = not f1 \/ f2

infix 8 <==
(<==) :: Formula -> Formula -> Formula
f1 <== f2 = f1 \/ not f2

implies :: Formula -> Formula -> Formula
implies = (==>)

----------------------------------------------------------------------------------------------------
-- Equivalent
----------------------------------------------------------------------------------------------------

infix 8 <==>
(<==>) :: Formula -> Formula -> Formula
f1 <==> f2 = (f1 ==> f2) /\ (f1 <== f2)

iff :: Formula -> Formula -> Formula
iff = (<==>)


----------------------------------------------------------------------------------------------------
-- Quantification
----------------------------------------------------------------------------------------------------

getQuantificationLevel :: FormulaStructure -> Int
getQuantificationLevel T = 0
getQuantificationLevel F = 0
getQuantificationLevel (Constraint _ _ _) = 0
getQuantificationLevel (And fs) = maximum $ fmap getQuantificationLevel $ elems fs
getQuantificationLevel (Or fs) = maximum $ fmap getQuantificationLevel $ elems fs
getQuantificationLevel (Not f) = getQuantificationLevel f
getQuantificationLevel (Exists x f) = succ $ getQuantificationLevel f
getQuantificationLevel (ForAll x f) = succ $ getQuantificationLevel f

----------------------------------------------------------------------------------------------------
-- Exists
----------------------------------------------------------------------------------------------------

createExists :: Variable -> Set Variable -> FormulaStructure -> Formula
createExists x fv f = if isQuantificationVariable x
                      then Formula (delete x fv) (Exists x f)
                      else let qv = quantificationVariable $ succ $ getQuantificationLevel f
                           in replaceFormulaVariable x qv $ Formula (delete x fv) (Exists x f)

(∃) :: Variable -> Formula -> Formula
(∃) x (Formula _ T) = true
(∃) x (Formula _ F) = false
(∃) x ff@(Formula fv f) | notMember x fv = ff
(∃) x (Formula _ (Constraint _ _ _)) = true
(∃) x (Formula fv f) = createExists x fv f

existsVar :: Variable -> Formula -> Formula
existsVar = (∃)

----------------------------------------------------------------------------------------------------
-- For all
----------------------------------------------------------------------------------------------------

createForAll :: Variable -> Set Variable -> FormulaStructure -> Formula
createForAll x fv f = if isQuantificationVariable x
                      then Formula (delete x fv) (ForAll x f)
                      else let qv = quantificationVariable $ succ $ getQuantificationLevel f
                           in replaceFormulaVariable x qv $ Formula (delete x fv) (ForAll x f)

(∀) :: Variable -> Formula -> Formula
(∀) x (Formula _ T) = true
(∀) x (Formula _ F) = false
(∀) x ff@(Formula fv f) | notMember x fv = ff
(∀) x (Formula _ (Constraint _ _ _)) = false
(∀) x (Formula fv f) = createForAll x fv f

forAllVars :: Variable -> Formula -> Formula
forAllVars = (∀)
