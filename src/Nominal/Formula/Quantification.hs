module Nominal.Formula.Quantification (existsVar, forAllVars) where

import Control.Monad (liftM2)
import Data.Set
import Nominal.Formula.Definition
import Nominal.Formula.Constructors
import Nominal.Formula.Operators
import Nominal.Variable (Variable)
import Prelude hiding (and, filter, map, not, or)

----------------------------------------------------------------------------------------------------
-- Quantifiers elimination (assumption: formula in NNF)
----------------------------------------------------------------------------------------------------

getConstraintsForVariable :: Variable -> Formula -> (Set Variable, Set Variable, Set Variable)
getConstraintsForVariable x (Formula _ f) = get f
    where get T = (empty, empty, empty)
          get F = (empty, empty, empty)
          get (Constraint r x1 x2) | x1 == x = getFromConstraint r x2
          get (Constraint r x1 x2) | x2 == x = getFromConstraint (symmetricRelation r) x1
          get (Constraint _ _ _) = (empty, empty, empty)
          get (And fs) = getFromSet $ map formula fs
          get (Or fs) = getFromSet $ map formula fs
          getFromConstraint LessThan y = (empty, empty, singleton y)
          getFromConstraint LessEquals y = (empty, singleton y, singleton y)
          getFromConstraint Equals y = (empty, singleton y, empty)
          getFromConstraint NotEquals y = (singleton y, empty, singleton y)
          getFromConstraint GreaterEquals y = (singleton y, singleton y, empty)
          getFromConstraint GreaterThan y = (singleton y, empty, empty)
          getFromSet fs = (\(s1, s2, s3) -> (unions s1, unions s2, unions s3)) $ unzip3 $ elems $ map get fs

replaceConstraintsInFormula :: Variable -> (Relation -> Variable -> Formula) -> Formula -> Formula
replaceConstraintsInFormula x cf (Formula _ (Constraint r x1 x2)) | x1 == x = cf r x2
replaceConstraintsInFormula x cf (Formula _ (Constraint r x1 x2)) | x2 == x = cf (symmetricRelation r) x1
replaceConstraintsInFormula x cf (Formula _ (And fs)) = andFromSet $ map (replaceConstraintsInFormula x cf) fs
replaceConstraintsInFormula x cf (Formula _ (Or fs)) = orFromSet $ map (replaceConstraintsInFormula x cf) fs
replaceConstraintsInFormula _ _ f = f

replaceByVariables :: Variable -> Set Variable -> Formula -> Set Formula
replaceByVariables x ys f = map (\y -> replaceConstraintsInFormula x (\r z -> constraint r y z) f) ys

replaceByMinusInfinity :: Variable -> Formula -> Formula
replaceByMinusInfinity x = replaceConstraintsInFormula x replace
    where replace LessThan _ = true
          replace LessEquals _ = true
          replace Equals _ = false
          replace NotEquals _ = true
          replace GreaterEquals _ = false
          replace GreaterThan _ = false

replaceByPlusInfinity :: Variable -> Formula -> Formula
replaceByPlusInfinity x = replaceConstraintsInFormula x replace
    where replace LessThan _ = false
          replace LessEquals _ = false
          replace Equals _ = false
          replace NotEquals _ = true
          replace GreaterEquals _ = true
          replace GreaterThan _ = true

replaceByLowerBounds :: Variable -> Set Variable -> Formula -> Set Formula
replaceByLowerBounds x ys f = map (\y -> replaceConstraintsInFormula x (replace y) f) ys
    where replace y LessThan z = lessThan y z
          replace y LessEquals z = lessThan y z
          replace y Equals _ = false
          replace y NotEquals _ = true
          replace y GreaterEquals z = greaterEquals y z
          replace y GreaterThan z = greaterEquals y z

replaceByUpperBounds :: Variable -> Set Variable -> Formula -> Set Formula
replaceByUpperBounds x ys f = map (\y -> replaceConstraintsInFormula x (replace y) f) ys
    where replace y LessThan z = lessEquals y z
          replace y LessEquals z = lessEquals y z
          replace y Equals _ = false
          replace y NotEquals _ = true
          replace y GreaterEquals z = greaterThan y z
          replace y GreaterThan z = greaterThan y z

quantifiersEliminationFromExists :: Variable -> Formula -> Formula
quantifiersEliminationFromExists x f = orFromSet $ union (replaceByVariables x e f) $ insert infinity bounds
    where (l, e, u) = getConstraintsForVariable x f
          (infinity, bounds) = if size u < size l
                                 then (replaceByPlusInfinity x f, replaceByUpperBounds x u f)
                                 else (replaceByMinusInfinity x f, replaceByLowerBounds x l f)

----------------------------------------------------------------------------------------------------
-- Exists
----------------------------------------------------------------------------------------------------

reduceVariable :: Variable -> Formula -> Formula
reduceVariable x (Formula fvs (And fs)) | size cs > 0 = Formula fvs $ And $ insert c $ map (replaceFormulaVariable x y) (delete c fs)
    where isEqualityWithVar x (Formula _ (Constraint Equals y z)) = x == y || x == z
          isEqualityWithVar _ _ = False
          cs = filter (isEqualityWithVar x) fs
          c = findMin cs
          y = (\(Formula _ (Constraint _ x1 x2)) -> if x1 == x then x2 else x1) c
reduceVariable x (Formula fvs (Or fs)) | size cs > 0 = Formula fvs $ Or $ insert c $ map (replaceFormulaVariable x y) (delete c fs)
    where isInequalityWithVar x (Formula _ (Constraint NotEquals x1 x2)) = x == x1 || x == x2
          isInequalityWithVar _ _ = False
          cs = filter (isInequalityWithVar x) fs
          c = findMin cs
          y = (\(Formula _ (Constraint _ x1 x2)) -> if x1 == x then x2 else x1) c
reduceVariable _ f = f

createExists :: Variable -> Formula -> Formula
createExists _ (Formula _ T) = true
createExists _ (Formula _ F) = false
createExists x f@(Formula fvs _) | notMember x fvs = f
createExists x (Formula _ (And fs)) | size fs2 > 0 = and $ (createExists x $ and $ elems fs1) : elems fs2
    where (fs1, fs2) = partition (\(Formula fvs _) -> member x fvs) fs
createExists x (Formula _ (Or fs)) = orFromSet $ map (createExists x) fs
createExists x f = quantifiersEliminationFromExists x f

existsVar :: Variable -> Formula -> Formula
existsVar x f = createExists x (reduceVariable x f)

----------------------------------------------------------------------------------------------------
-- For all
----------------------------------------------------------------------------------------------------

createForAll :: Variable -> Formula -> Formula
createForAll x (Formula _ T) = true
createForAll x (Formula _ F) = false
createForAll x ff@(Formula fv f) | notMember x fv = ff
createForAll x (Formula _ (Constraint _ _ _)) = false
createForAll x (Formula _ (And fs)) = andFromSet $ map (createForAll x) fs
createForAll x (Formula _ (Or fs)) | size fs2 > 0 = or $ (createForAll x $ or $ elems fs1) : elems fs2
    where (fs1, fs2) = partition (\(Formula fvs _) -> member x fvs) fs
createForAll x f = not $ quantifiersEliminationFromExists x (not f)

forAllVars :: Variable -> Formula -> Formula
forAllVars x f = createForAll x (reduceVariable x f)
