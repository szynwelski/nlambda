module Nominal.Formula.Definition where

import Data.List.Utils (join)
import Data.Set (Set, elems)
import Nominal.Variable (Variable)

----------------------------------------------------------------------------------------------------
-- Relation
----------------------------------------------------------------------------------------------------
data Relation = LessThan | LessEquals | Equals | NotEquals | GreaterEquals | GreaterThan deriving (Eq, Ord)

instance Show Relation where
    show LessThan = "<"
    show LessEquals = "≤"
    show Equals = "="
    show NotEquals = "≠"
    show GreaterThan = ">"
    show GreaterEquals = "≥"

relationAscii :: Relation -> String
relationAscii LessThan = "<"
relationAscii LessEquals = "<="
relationAscii Equals = "="
relationAscii NotEquals = "/="
relationAscii GreaterThan = ">"
relationAscii GreaterEquals = ">="

----------------------------------------------------------------------------------------------------
-- Formula structure
----------------------------------------------------------------------------------------------------

data FormulaStructure
    = T
    | F
    | Constraint Relation Variable Variable
    | And (Set Formula)
    | Or (Set Formula)
    | Not Formula
    | Exists Variable Formula
    | ForAll Variable Formula deriving (Eq, Ord)

----------------------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------------------

showFormula :: FormulaStructure -> String
showFormula f@(And fs) = "(" ++ show f ++ ")"
showFormula f@(Or fs) = "(" ++ show f ++ ")"
showFormula f = show f

instance Show FormulaStructure where
    show T = "true"
    show F = "false"
    show (Constraint r x1 x2) = show x1 ++ " " ++ show r ++ " " ++ show x2
    show (And fs) = join " ∧ " $ fmap (showFormula . formula) $ elems fs
    show (Or fs) = join " ∨ " $ fmap (showFormula . formula) $ elems fs
    show (Not f) = "¬(" ++ show f ++ ")"
    show (Exists x f) = "∃" ++ show x ++ " " ++ showFormula (formula f)
    show (ForAll x f) = "∀" ++ show x ++ " " ++ showFormula (formula f)

----------------------------------------------------------------------------------------------------
-- Formula
----------------------------------------------------------------------------------------------------

data Formula = Formula {freeVariables :: Set Variable, formula :: FormulaStructure}

instance Eq Formula where
    (Formula _ f1) == (Formula _ f2) = f1 == f2

instance Ord Formula where
    compare (Formula _ f1) (Formula _ f2) = compare f1 f2

instance Show Formula where
    show (Formula _ f) = show f
