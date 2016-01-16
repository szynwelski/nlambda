module Nominal.Formula.Constructors where

import Data.Set (Set, delete, elems, empty, foldl, fromList, map, unions)
import Nominal.Formula.Definition
import Nominal.Variable (Variable)
import Prelude hiding (foldl, map)

----------------------------------------------------------------------------------------------------
-- Formula constructors
----------------------------------------------------------------------------------------------------

-- | Creates the tautology formula.
true :: Formula
true = Formula empty T

-- | Creates the contradiction formula.
false :: Formula
false = Formula empty F

-- | Creates a formula based on a given 'Bool' value.
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
constraint r x1 x2 = let f = constraintStruct r x1 x2
                         fvs = case f of (Constraint _ _ _) -> fromList [x1,x2]
                                         otherwise          -> empty
                     in Formula fvs f

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
