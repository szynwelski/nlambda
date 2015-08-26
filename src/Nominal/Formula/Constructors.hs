module Nominal.Formula.Constructors where

import Data.Set (Set, delete, elems, empty, foldl, fromList, map, unions)
import Nominal.Formula.Definition
import Nominal.Variable (Variable)
import Prelude hiding (foldl, map)

----------------------------------------------------------------------------------------------------
-- Formula constructors
----------------------------------------------------------------------------------------------------

-- true
true :: Formula
true = Formula empty T

-- false
false :: Formula
false = Formula empty F

-- from bool
fromBool :: Bool -> Formula
fromBool True = true
fromBool False = false

----------------------------------------------------------------------------------------------------
-- Constraints
----------------------------------------------------------------------------------------------------

symmetricRelation :: Relation -> Relation
symmetricRelation LessThan = GreaterThan
symmetricRelation LessEquals = GreaterEquals
symmetricRelation GreaterThan = LessThan
symmetricRelation GreaterEquals = LessEquals
symmetricRelation Equals = Equals
symmetricRelation NotEquals = NotEquals

constraintStruct :: Relation -> Variable -> Variable -> FormulaStructure
constraintStruct r x1 x2
    | x1 == x2 = if r == LessThan || r == GreaterThan || r == NotEquals then F else T
    | x1 > x2 = Constraint (symmetricRelation r) x2 x1
    | otherwise = Constraint r x1 x2

constraint :: Relation -> Variable -> Variable -> Formula
constraint r x1 x2 = let c = constraintStruct r x1 x2 in Formula (getFreeVariables c) c

equals :: Variable -> Variable -> Formula
equals = constraint Equals

lessThan :: Variable -> Variable -> Formula
lessThan = constraint LessThan

lessEquals :: Variable -> Variable -> Formula
lessEquals = constraint LessEquals

greaterThan :: Variable -> Variable -> Formula
greaterThan = constraint GreaterThan

greaterEquals :: Variable -> Variable -> Formula
greaterEquals = constraint GreaterEquals

----------------------------------------------------------------------------------------------------
-- Variables functions
----------------------------------------------------------------------------------------------------

getFreeVariables :: FormulaStructure -> Set Variable
getFreeVariables T = empty
getFreeVariables F = empty
getFreeVariables (Constraint r x1 x2) = fromList [x1,x2]
getFreeVariables (And fs) = unions $ fmap freeVariables $ elems fs
getFreeVariables (Or fs) = unions $ fmap freeVariables $ elems fs
getFreeVariables (Not f) = freeVariables f
getFreeVariables (Exists x f) = delete x $ freeVariables f
getFreeVariables (ForAll x f) = delete x $ freeVariables f

foldFormulaVariables :: (Variable -> a -> a) -> a -> Formula -> a
foldFormulaVariables fun acc (Formula _ f) = doFold fun acc f
    where doFold _ acc T = acc
          doFold _ acc F = acc
          doFold fun acc (Constraint _ x1 x2) = fun x2 $ fun x1 acc
          doFold fun acc (And fs) = foldl (foldFormulaVariables fun) acc fs
          doFold fun acc (Or fs) = foldl (foldFormulaVariables fun) acc fs
          doFold fun acc (Not f) = foldFormulaVariables fun acc f
          doFold fun acc (Exists x f) = foldFormulaVariables fun (fun x acc) f
          doFold fun acc (ForAll x f) = foldFormulaVariables fun (fun x acc) f

mapFormulaVariables :: (Variable -> Variable) -> Formula -> Formula
mapFormulaVariables fun (Formula fs f) = Formula (map fun fs) (doMap fun f)
    where doMap _ T = T
          doMap _ F = F
          doMap fun (Constraint r x1 x2) = constraintStruct r (fun x1) (fun x2)
          doMap fun (And fs) = And $ map (mapFormulaVariables fun) fs
          doMap fun (Or fs) = Or $ map (mapFormulaVariables fun) fs
          doMap fun (Not f) = Not $ mapFormulaVariables fun f
          doMap fun (Exists x f) = Exists (fun x) (mapFormulaVariables fun f)
          doMap fun (ForAll x f) = ForAll (fun x) (mapFormulaVariables fun f)

replaceFormulaVariable :: Variable -> Variable -> Formula -> Formula
replaceFormulaVariable oldVar newVar = mapFormulaVariables (\var -> if oldVar == var then newVar else var)
