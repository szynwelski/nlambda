module Nominal.Formula.Definition where

import Data.Set (Set)
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
    | ForAll Variable Formula
