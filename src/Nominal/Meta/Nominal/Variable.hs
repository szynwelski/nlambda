module Nominal.Meta.Nominal.Variable where

import Data.Set (Set)
import Nominal.Atoms.Signature (Constant)
import Nominal.Meta.GHC.Classes
import Nominal.Meta.GHC.Read
import Nominal.Meta.GHC.Show
import Nominal.Meta
import Nominal.Variable

instance NLambda_Eq Variable

instance NLambda_Ord Variable

instance NLambda_Read Variable

instance NLambda_Show Variable

nlambda_isConstant :: WithMeta Variable -> Bool
nlambda_isConstant = isConstant . value

nlambda_constantValue :: WithMeta Variable -> Constant
nlambda_constantValue = constantValue . value

nlambda_constantVar :: Constant -> WithMeta Variable
nlambda_constantVar = noMeta . constantVar

nlambda_variable :: String -> WithMeta Variable
nlambda_variable = noMeta . variable

nlambda_iterationVariable :: Int -> Int -> WithMeta Variable
nlambda_iterationVariable l = noMeta . iterationVariable l

nlambda_freeVariables :: Var a => WithMeta a -> WithMeta (Set Variable)
nlambda_freeVariables = idOp freeVariables
