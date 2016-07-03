module Nominal.Formula.Constructors where

import Nominal.Atoms.Signature (Relation(..), checkConstants, symmetricRelation)
import Nominal.Formula.Definition
import Nominal.Variable (Variable, isConstant, constantValue)

----------------------------------------------------------------------------------------------------
-- Formula constructors
----------------------------------------------------------------------------------------------------

-- | Creates the tautology formula.
true :: Formula
true = Formula True T

-- | Creates the contradiction formula.
false :: Formula
false = Formula True F

-- | Creates a formula based on a given 'Bool' value.
fromBool :: Bool -> Formula
fromBool True = true
fromBool False = false

----------------------------------------------------------------------------------------------------
-- Constraints
----------------------------------------------------------------------------------------------------

constraint :: Relation -> Variable -> Variable -> Formula
constraint r x1 x2
    | isConstant x1 && isConstant x2 = fromBool $ checkConstants r (constantValue x1) (constantValue x2)
    | x1 == x2 = if r == LessThan || r == GreaterThan || r == NotEquals then false else true
    | x1 > x2 = Formula True $ Constraint (symmetricRelation r) x2 x1
    | otherwise = Formula True $ Constraint r x1 x2

equals :: Variable -> Variable -> Formula
equals = constraint Equals

notEquals :: Variable -> Variable -> Formula
notEquals = constraint NotEquals

lessThan :: Variable -> Variable -> Formula
lessThan = constraint LessThan

lessEquals :: Variable -> Variable -> Formula
lessEquals = constraint LessEquals

greaterThan :: Variable -> Variable -> Formula
greaterThan = constraint GreaterThan

greaterEquals :: Variable -> Variable -> Formula
greaterEquals = constraint GreaterEquals
