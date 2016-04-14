module Nominal.Formula.Operators where

import qualified Data.MultiMap as MM
import Data.Set
import Nominal.Formula.Definition
import Nominal.Formula.Constructors
import Nominal.Variable (Variable)
import Prelude hiding (foldl, foldr, map, not, null)

----------------------------------------------------------------------------------------------------
-- Simplification of constraints with the same variables
----------------------------------------------------------------------------------------------------

isConstraint :: Formula -> Bool
isConstraint (Formula _ (Constraint _ _ _)) = True
isConstraint _ = False

sameVarsInConstraints :: FormulaStructure -> FormulaStructure -> Bool
sameVarsInConstraints (Constraint _ x1 x2) (Constraint _ y1 y2) = (x1, x2) == (y1, y2)
sameVarsInConstraints _ _ = False

checkSize :: (Set Formula -> FormulaStructure) -> Set Variable -> Formula -> Set Formula -> Formula
checkSize creator freeVars defVal fs
    | null fs = defVal
    | size fs == 1 = findMin fs
    | size fs > 1 = Formula freeVars (creator fs)

checkConstraints :: (Set Formula -> FormulaStructure) -> Set Variable -> Formula
    -> (Maybe Relation -> Maybe Relation -> Maybe Relation) -> Set Formula -> Formula
checkConstraints creator freeVars defVal relFun fs =
    if member nDefVal cs
        then nDefVal
        else checkSize creator freeVars defVal (union cs fs2)
    where nDefVal = not defVal
          (fs1, fs2) = partition isConstraint fs
          relLists = MM.assocs $ foldr (\(Constraint r x1 x2) -> MM.insert (x1,x2) (Just r)) MM.empty (map formula fs1)
          rels = fmap (fmap $ foldr1 relFun) relLists
          cs = fromList $ fmap (\((x1,x2),rel) -> maybe nDefVal (\r -> (constraint r x1 x2)) rel) rels

----------------------------------------------------------------------------------------------------
-- And
----------------------------------------------------------------------------------------------------

-- merging constraints
andRelations :: Maybe Relation -> Maybe Relation -> Maybe Relation
andRelations r1 r2
    | r1 == r2 = r1
    | r1 > r2 = andRelations r2 r1
andRelations (Just LessThan)      (Just LessEquals)    = Just LessThan
andRelations (Just LessThan)      (Just NotEquals)     = Just LessThan
andRelations (Just LessEquals)    (Just NotEquals)     = Just LessThan
andRelations (Just LessEquals)    (Just Equals)        = Just Equals
andRelations (Just Equals)        (Just GreaterEquals) = Just Equals
andRelations (Just LessEquals)    (Just GreaterEquals) = Just Equals
andRelations (Just NotEquals)     (Just GreaterEquals) = Just GreaterThan
andRelations (Just NotEquals)     (Just GreaterThan)   = Just GreaterThan
andRelations (Just GreaterEquals) (Just GreaterThan)   = Just GreaterThan
andRelations _                    _                    = Nothing

-- create and formula
createAndSet :: Set Formula -> Set Formula
createAndSet = delete true . unions . elems . map fromAnd
    where fromAnd (Formula _ (And fs)) = fs
          fromAnd f = singleton f

andFromSet :: Set Formula -> Formula
andFromSet fs = if member false fs
                  then false
                  else checkConstraints And (unions $ elems $ map freeVariables fs) true andRelations (createAndSet fs)

-- | Creates a logical conjunction of two given formulas, e.g.
--
-- > f /\ false == false
-- > f /\ true  == f
-- > f /\ g     == g /\ f
infixr 3 /\
(/\) :: Formula -> Formula -> Formula
Formula _ F /\ _ = false
_ /\ Formula _ F = false
Formula _ T /\ f = f
f /\ Formula _ T = f
f1 /\ f2 = andFromSet (fromList [f1,f2])

-- | Creates a logical conjunction of a given list of formulas.
and :: [Formula] -> Formula
and = andFromSet . fromList

----------------------------------------------------------------------------------------------------
-- Or
----------------------------------------------------------------------------------------------------

-- merging constraints
orRelations :: Maybe Relation -> Maybe Relation -> Maybe Relation
orRelations r1 r2
    | r1 == r2 = r1
    | r1 > r2 = orRelations r2 r1
orRelations (Just LessThan)      (Just LessEquals)    = Just LessEquals
orRelations (Just LessThan)      (Just Equals)        = Just LessEquals
orRelations (Just LessEquals)    (Just Equals)        = Just LessEquals
orRelations (Just LessThan)      (Just NotEquals)     = Just NotEquals
orRelations (Just LessThan)      (Just GreaterThan)   = Just NotEquals
orRelations (Just NotEquals)     (Just GreaterThan)   = Just NotEquals
orRelations (Just Equals)        (Just GreaterEquals) = Just GreaterEquals
orRelations (Just Equals)        (Just GreaterThan)   = Just GreaterEquals
orRelations (Just GreaterEquals) (Just GreaterThan)   = Just GreaterEquals
orRelations _                    _                    = Nothing

-- create or formula
createOrSet :: Set Formula -> Set Formula
createOrSet = delete false . unions . elems . map fromOr
    where fromOr (Formula _ (Or fs)) = fs
          fromOr f = singleton f

orFromSet :: Set Formula -> Formula
orFromSet fs = if member true fs
                  then true
                  else checkConstraints Or (unions $ elems $ map freeVariables fs) false orRelations (createOrSet fs)

-- | Creates a logical disjunction of two given formulas, e.g.
--
-- > f \/ true  == true
-- > f \/ false == f
-- > f \/ g     == g \/ f
infixr 2 \/
(\/) :: Formula -> Formula -> Formula
Formula _ T \/ _ = true
_ \/ Formula _ T = true
Formula _ F \/ f = f
f \/ Formula _ F = f
f1 \/ f2 = orFromSet (fromList [f1,f2])

-- | Creates a logical disjunction of a given list of formulas.
or :: [Formula] -> Formula
or = orFromSet . fromList

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

-- | Creates a negation of a given formula, e.g.
--
-- > not true    == false
-- > not false   == true
-- > not (not f) == f
not :: Formula -> Formula
not (Formula fv f) = Formula fv (createNot f)

----------------------------------------------------------------------------------------------------
-- Imply
----------------------------------------------------------------------------------------------------

-- | Creates an implication of given formulas, e.g.
--
-- > false ==> f    == true
-- > f     ==> true == true
-- > f     ==> g    == g <== f
infix 8 ==>
(==>) :: Formula -> Formula -> Formula
f1 ==> f2 = not f1 \/ f2

-- | Creates an implication of given formulas, e.g.
--
-- > f    <== false == true
-- > true <== f     == true
-- > f    <== g     == g ==> f
infix 8 <==
(<==) :: Formula -> Formula -> Formula
f1 <== f2 = f1 \/ not f2

-- | Creates a implication of given formulas, equivalent to '==>'.
implies :: Formula -> Formula -> Formula
implies = (==>)

----------------------------------------------------------------------------------------------------
-- Equivalent
----------------------------------------------------------------------------------------------------

-- | Creates a formula representing "if and only if" relation between given formulas, e.g.
--
-- > f <==> f     == true
-- > f <==> not f == false
-- > f <==> g     == (f /\ g \/ not f /\ not g)
infix 8 <==>
(<==>) :: Formula -> Formula -> Formula
f1 <==> f2 = (f1 /\ f2) \/ (not f1 /\ not f2)

-- | Equivalent to '<==>'.
iff :: Formula -> Formula -> Formula
iff = (<==>)

----------------------------------------------------------------------------------------------------
-- Variables functions
----------------------------------------------------------------------------------------------------

foldFormulaVariables :: (Variable -> a -> a) -> a -> Formula -> a
foldFormulaVariables fun acc (Formula _ f) = doFold fun acc f
    where doFold _ acc T = acc
          doFold _ acc F = acc
          doFold fun acc (Constraint _ x1 x2) = fun x2 $ fun x1 acc
          doFold fun acc (And fs) = foldl (foldFormulaVariables fun) acc fs
          doFold fun acc (Or fs) = foldl (foldFormulaVariables fun) acc fs
          doFold fun acc (Not f) = foldFormulaVariables fun acc f

mapFormulaVariables :: (Variable -> Variable) -> Formula -> Formula
mapFormulaVariables fun (Formula fvs f) = doMap fun f
    where doMap _ T = true
          doMap _ F = false
          doMap fun (Constraint r x1 x2) = constraint r (fun x1) (fun x2)
          doMap fun (And fs) = andFromSet $ map (mapFormulaVariables fun) fs
          doMap fun (Or fs) = orFromSet $ map (mapFormulaVariables fun) fs
          doMap fun (Not f) = not $ mapFormulaVariables fun f

replaceFormulaVariable :: Variable -> Variable -> Formula -> Formula
replaceFormulaVariable oldVar newVar = mapFormulaVariables (\var -> if oldVar == var then newVar else var)

getEquationsFromFormula :: Formula -> Set (Variable, Variable)
getEquationsFromFormula f = go (formula f)
    where go (Constraint Equals x1 x2) = singleton (x1, x2)
          go (And fs) = unions $ elems $ map (go . formula) fs
          go _ = empty
