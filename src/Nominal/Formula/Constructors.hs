module Nominal.Formula.Constructors where

import Data.Set (empty, fromList)
import Nominal.Formula.Definition
import Nominal.Variable (Variable)

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

constraint :: Relation -> Variable -> Variable -> Formula
constraint r x1 x2
    | x1 == x2 = if r == LessThan || r == GreaterThan || r == NotEquals then false else true
    | x1 > x2 = Formula (fromList [x1, x2]) (Constraint (symmetricRelation r) x2 x1)
    | otherwise = Formula (fromList [x1, x2]) (Constraint r x1 x2)

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
