module Nominal.Formula.Operators where

import qualified Data.MultiMap as MM
import Data.Set
import Nominal.Formula.Definition
import Nominal.Formula.Constructors
import Nominal.Variable (Variable, isQuantificationVariable, quantificationVariable)
import Prelude hiding (and, filter, foldr, map, not, null, or)

----------------------------------------------------------------------------------------------------
-- And
----------------------------------------------------------------------------------------------------

-- checking constraints

--andRelations :: Maybe Relation -> Maybe Relation -> Maybe Relation
--andRelations r1 r2
--    | r1 == r2 = r1
--    | r1 > r2 = andRelations r2 r1
--andRelations (Just LessThan)      (Just LessEquals)    = Just LessThan
--andRelations (Just LessThan)      (Just NotEquals)     = Just LessThan
--andRelations (Just LessEquals)    (Just NotEquals)     = Just LessThan
--andRelations (Just LessEquals)    (Just Equals)        = Just Equals
--andRelations (Just Equals)        (Just GreaterEquals) = Just Equals
--andRelations (Just LessEquals)    (Just GreaterEquals) = Just Equals
--andRelations (Just NotEquals)     (Just GreaterEquals) = Just GreaterThan
--andRelations (Just NotEquals)     (Just GreaterThan)   = Just GreaterThan
--andRelations (Just GreaterEquals) (Just GreaterThan)   = Just GreaterThan
--andRelations _                    _                    = Nothing
--
--isConstraint :: FormulaStructure -> Bool
--isConstraint (Constraint _ _ _) = True
--isConstraint _ = False
--
--sameVarsInConstraints :: FormulaStructure -> FormulaStructure -> Bool
--sameVarsInConstraints (Constraint _ x1 x2) (Constraint _ y1 y2) = (x1, x2) == (y1, y2)
--sameVarsInConstraints _ _ = False
--
--checkSize :: (Set FormulaStructure -> FormulaStructure) -> FormulaStructure -> Set FormulaStructure -> FormulaStructure
--checkSize creator def fs
--    | null fs = def
--    | size fs == 1 = findMin fs
--    | size fs > 1 = creator fs
--
---- TODO update free variables
--checkConstraints :: (Set FormulaStructure -> FormulaStructure) -> FormulaStructure
--    -> (Maybe Relation -> Maybe Relation -> Maybe Relation) -> Set Variable -> Set FormulaStructure -> Formula
--checkConstraints creator def relFun fv fs =
--    if member def cs
--        then Formula empty def
--        else Formula fv (checkSize creator def $ union cs fs2)
--    where (fs1, fs2) = partition isConstraint fs
--          relLists = MM.assocs $ foldr (\(Constraint r x1 x2) -> MM.insert (x1,x2) (Just r)) MM.empty fs1
--          cs = fromList $ fmap (\((x1,x2),rs) -> maybe def (\r -> Constraint r x1 x2) (foldr1 relFun rs)) relLists

-- create and formula

createAndSet :: Formula -> Formula -> Set Formula
createAndSet (Formula _ (And fs1)) (Formula _ (And fs2)) = union fs1 fs2
createAndSet (Formula _ (And fs)) f = insert f fs
createAndSet f (Formula _ (And fs)) = insert f fs
createAndSet f1 f2 = fromList [f1, f2]

(/\) :: Formula -> Formula -> Formula
Formula _ F /\ _ = false
_ /\ Formula _ F = false
Formula _ T /\ f = f
f /\ Formula _ T = f
f1@(Formula fvs1 _) /\ f2@(Formula fvs2 _) = --checkConstraints And F andRelations
    Formula (union fvs1 fvs2) $ And (createAndSet f1 f2)

and :: [Formula] -> Formula
and [] = true
and fs = foldr1 (/\) fs

----------------------------------------------------------------------------------------------------
-- Or
----------------------------------------------------------------------------------------------------

createOrSet :: Formula -> Formula -> Set Formula
createOrSet (Formula _ (Or fs1)) (Formula _ (Or fs2)) = (union fs1 fs2)
createOrSet (Formula _ (Or fs)) f = insert f fs
createOrSet f (Formula _ (Or fs)) = insert f fs
createOrSet f1 f2 = fromList [f1, f2]

(\/) :: Formula -> Formula -> Formula
Formula _ T \/ _ = true
_ \/ Formula _ T = true
Formula _ F \/ f = f
f \/ Formula _ F = f
f1@(Formula fvs1 _) \/ f2@(Formula fvs2 _) = Formula (union fvs1 fvs2) $ Or (createOrSet f1 f2)

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
createNot (And fs) = Or $ map not fs
createNot (Or fs) = And $ map not fs
createNot (Not (Formula _ f)) = f
createNot (Exists x f) = ForAll x $ not f
createNot (ForAll x f) = Exists x $ not f

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

getQuantificationLevel :: Formula -> Int
getQuantificationLevel (Formula _ f) = getLevel f
    where getLevel T = 0
          getLevel F = 0
          getLevel (Constraint _ _ _) = 0
          getLevel (And fs) = maximum $ fmap getQuantificationLevel $ elems fs
          getLevel (Or fs) = maximum $ fmap getQuantificationLevel $ elems fs
          getLevel (Not f) = getQuantificationLevel f
          getLevel (Exists x f) = succ $ getQuantificationLevel f
          getLevel (ForAll x f) = succ $ getQuantificationLevel f

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
createExists x (Formula _ T) = true
createExists x (Formula _ F) = false
createExists x f@(Formula fvs _) | notMember x fvs = f
createExists x (Formula _ (Constraint _ _ _)) = true
createExists x (Formula _ (And fs)) | size fs2 > 0 = and $ (createExists x $ and $ elems fs1) : elems fs2
    where (fs1, fs2) = partition (\(Formula fvs _) -> member x fvs) fs
createExists x (Formula _ (Or fs)) = or $ elems $ map (createExists x) fs
createExists x f@(Formula fvs _) = if isQuantificationVariable x
                                   then Formula (delete x fvs) (Exists x f)
                                   else let qv = quantificationVariable $ succ $ getQuantificationLevel f
                                        in replaceFormulaVariable x qv $ Formula (delete x fvs) (Exists x f)

(∃) :: Variable -> Formula -> Formula
(∃) x f = createExists x (reduceVariable x f)

existsVar :: Variable -> Formula -> Formula
existsVar = (∃)

----------------------------------------------------------------------------------------------------
-- For all
----------------------------------------------------------------------------------------------------

createForAll :: Variable -> Formula -> Formula
createForAll x (Formula _ T) = true
createForAll x (Formula _ F) = false
createForAll x ff@(Formula fv f) | notMember x fv = ff
createForAll x (Formula _ (Constraint _ _ _)) = false
createForAll x (Formula _ (And fs)) = and $ elems $ map (createForAll x) fs
createForAll x (Formula _ (Or fs)) | size fs2 > 0 = or $ (createForAll x $ or $ elems fs1) : elems fs2
    where (fs1, fs2) = partition (\(Formula fvs _) -> member x fvs) fs
createForAll x f@(Formula fvs _) = if isQuantificationVariable x
                                   then Formula (delete x fvs) (ForAll x f)
                                   else let qv = quantificationVariable $ succ $ getQuantificationLevel f
                                        in replaceFormulaVariable x qv $ Formula (delete x fvs) (ForAll x f)

(∀) :: Variable -> Formula -> Formula
(∀) x f = createForAll x (reduceVariable x f)

forAllVars :: Variable -> Formula -> Formula
forAllVars = (∀)
