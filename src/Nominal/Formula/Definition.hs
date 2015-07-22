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
-- Formula
----------------------------------------------------------------------------------------------------

data Formula
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

showFormula :: Formula -> String
showFormula f@(And fs) = "(" ++ show f ++ ")"
showFormula f@(Or fs) = "(" ++ show f ++ ")"
showFormula (Exists x f) = "∃" ++ show x ++ "(" ++ show f ++ ")"
showFormula (ForAll x f) = "∀" ++ show x ++ "(" ++ show f ++ ")"
showFormula f = show f

instance Show Formula where
    show T = "true"
    show F = "false"
    show (Constraint r x1 x2) = show x1 ++ " " ++ show r ++ " " ++ show x2
    show (And fs) = join " ∧ " $ fmap showFormula $ elems fs
    show (Or fs) = join " ∨ " $ fmap showFormula $ elems fs
    show (Not f) = "¬(" ++ show f ++ ")"
    show (Exists x f) = "∃" ++ show x ++ " " ++ show f
    show (ForAll x f) = "∀" ++ show x ++ " " ++ show f
