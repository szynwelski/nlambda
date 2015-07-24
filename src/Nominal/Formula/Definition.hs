module Nominal.Formula.Definition where

import Data.List.Utils (join)
import Data.Set (Set, elems, foldl, map)
import Nominal.Variable (Variable)
import Prelude hiding (foldl, map)

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
    | And (Set FormulaStructure)
    | Or (Set FormulaStructure)
    | Not FormulaStructure
    | Exists Variable FormulaStructure
    | ForAll Variable FormulaStructure deriving (Eq, Ord)

----------------------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------------------

showFormula :: FormulaStructure -> String
showFormula f@(And fs) = "(" ++ show f ++ ")"
showFormula f@(Or fs) = "(" ++ show f ++ ")"
showFormula (Exists x f) = "∃" ++ show x ++ "(" ++ show f ++ ")"
showFormula (ForAll x f) = "∀" ++ show x ++ "(" ++ show f ++ ")"
showFormula f = show f

instance Show FormulaStructure where
    show T = "true"
    show F = "false"
    show (Constraint r x1 x2) = show x1 ++ " " ++ show r ++ " " ++ show x2
    show (And fs) = join " ∧ " $ fmap showFormula $ elems fs
    show (Or fs) = join " ∨ " $ fmap showFormula $ elems fs
    show (Not f) = "¬(" ++ show f ++ ")"
    show (Exists x f) = "∃" ++ show x ++ " " ++ show f
    show (ForAll x f) = "∀" ++ show x ++ " " ++ show f

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

----------------------------------------------------------------------------------------------------
-- Variables functions
----------------------------------------------------------------------------------------------------

foldFormulaVariables :: (Variable -> a -> a) -> a -> Formula -> a
foldFormulaVariables fun acc (Formula _ f) = doFold fun acc f
    where doFold _ acc T = acc
          doFold _ acc F = acc
          doFold fun acc (Constraint _ x1 x2) = fun x2 $ fun x1 acc
          doFold fun acc (And fs) = foldl (doFold fun) acc fs
          doFold fun acc (Or fs) = foldl (doFold fun) acc fs
          doFold fun acc (Not f) = doFold fun acc f
          doFold fun acc (Exists x f) = doFold fun (fun x acc) f
          doFold fun acc (ForAll x f) = doFold fun (fun x acc) f

mapFormulaVariables :: (Variable -> Variable) -> Formula -> Formula
mapFormulaVariables fun (Formula fs f) = Formula (map fun fs) (doMap fun f)
    where doMap _ T = T
          doMap _ F = F
          doMap fun (Constraint r x1 x2) = Constraint r (fun x1) (fun x2)
          doMap fun (And fs) = And $ map (doMap fun) fs
          doMap fun (Or fs) = Or $ map (doMap fun) fs
          doMap fun (Not f) = Not $ doMap fun f
          doMap fun (Exists x f) = Exists (fun x) (doMap fun f)
          doMap fun (ForAll x f) = ForAll (fun x) (doMap fun f)

replaceFormulaVariable :: Variable -> Variable -> Formula -> Formula
replaceFormulaVariable oldVar newVar = mapFormulaVariables (\var -> if oldVar == var then newVar else var)
