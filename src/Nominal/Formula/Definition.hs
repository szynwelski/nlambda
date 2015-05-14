module Nominal.Formula.Definition where

import Data.Set (Set)
import Nominal.Variable (Variable)

----------------------------------------------------------------------------------------------------
-- Relation
----------------------------------------------------------------------------------------------------
data Relation = Equals | LessThan | LessEquals | GreaterThan | GreaterEquals deriving (Eq, Ord)

instance Show Relation where
    show Equals = "="
    show LessThan = "<"
    show LessEquals = "≤"
    show GreaterThan = ">"
    show GreaterEquals = "≥"

relationAscii :: Relation -> String
relationAscii Equals = "="
relationAscii LessThan = "<"
relationAscii LessEquals = "<="
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
    | ForAll Variable Formula
    | Exists Variable Formula
