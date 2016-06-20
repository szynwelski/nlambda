module Nominal.Formula.Definition where

import Data.List.Utils (join)
import Data.Set (Set, elems)
import Nominal.Atoms.Signature (Relation)
import qualified Nominal.Text.Symbols as Symbols
import Nominal.Variable (Variable)

----------------------------------------------------------------------------------------------------
-- Formula structure
----------------------------------------------------------------------------------------------------

data FormulaStructure
    = T
    | F
    | Constraint Relation Variable Variable
    | And (Set Formula)
    | Or (Set Formula)
    | Not Formula deriving (Eq, Ord)

----------------------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------------------

showSubformula :: FormulaStructure -> String
showSubformula f@(And fs) = "(" ++ show f ++ ")"
showSubformula f@(Or fs) = "(" ++ show f ++ ")"
showSubformula f = show f

instance Show FormulaStructure where
    show T = "true"
    show F = "false"
    show (Constraint r x1 x2) = show x1 ++ " " ++ show r ++ " " ++ show x2
    show (And fs) = join Symbols.and $ fmap (showSubformula . formula) $ elems fs
    show (Or fs) = join Symbols.or $ fmap (showSubformula . formula) $ elems fs
    show (Not f) = Symbols.not ++ "(" ++ show f ++ ")"

----------------------------------------------------------------------------------------------------
-- Formula
----------------------------------------------------------------------------------------------------

-- | First order formula with free variables and relations between variables.
data Formula = Formula {isSimplified :: Bool, formula :: FormulaStructure}

instance Eq Formula where
    (Formula _ f1) == (Formula _ f2) = f1 == f2

instance Ord Formula where
    compare (Formula _ f1) (Formula _ f2) = compare f1 f2

instance Show Formula where
    show (Formula _ f) = show f
