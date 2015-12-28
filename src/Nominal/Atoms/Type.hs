module Nominal.Atoms.Type (AtomsType(..), _DEFAULT_ATOMS_TYPE_, existsVar, forAllVars, isTrue, isFalse, relations) where

import Nominal.Formula.Definition
import Nominal.Formula.Constructors (equals, lessThan)
import qualified Nominal.Formula.Solver as S
import qualified Nominal.Formula.Quantification as Q
import Nominal.Variable (Variable)

----------------------------------------------------------------------------------------------------
-- Atoms type
----------------------------------------------------------------------------------------------------

data AtomsType = AtomsWithEquality | AtomsWithTotalOrder

existsVarInAtomsType :: AtomsType -> Variable -> Formula -> Formula
existsVarInAtomsType AtomsWithEquality = Q.existsVar
existsVarInAtomsType AtomsWithTotalOrder = Q.existsVar

forAllVarsInAtomsType :: AtomsType -> Variable -> Formula -> Formula
forAllVarsInAtomsType AtomsWithEquality = Q.forAllVars
forAllVarsInAtomsType AtomsWithTotalOrder = Q.forAllVars

isTrueInAtomsType :: AtomsType -> Formula -> Bool
isTrueInAtomsType AtomsWithEquality = S.isTrue S.lia
isTrueInAtomsType AtomsWithTotalOrder = S.isTrue S.lra

isFalseInAtomsType :: AtomsType -> Formula -> Bool
isFalseInAtomsType AtomsWithEquality = S.isFalse S.lia
isFalseInAtomsType AtomsWithTotalOrder = S.isFalse S.lra

relationsInAtomsType :: AtomsType -> [Variable -> Variable -> Formula]
relationsInAtomsType AtomsWithEquality = [equals]
relationsInAtomsType AtomsWithTotalOrder = [equals, lessThan]

----------------------------------------------------------------------------------------------------
-- Default atoms type
----------------------------------------------------------------------------------------------------

_DEFAULT_ATOMS_TYPE_ = AtomsWithTotalOrder

existsVar :: Variable -> Formula -> Formula
existsVar = existsVarInAtomsType _DEFAULT_ATOMS_TYPE_

forAllVars :: Variable -> Formula -> Formula
forAllVars = forAllVarsInAtomsType _DEFAULT_ATOMS_TYPE_

isTrue :: Formula -> Bool
isTrue = isTrueInAtomsType _DEFAULT_ATOMS_TYPE_

isFalse :: Formula -> Bool
isFalse = isFalseInAtomsType _DEFAULT_ATOMS_TYPE_

relations :: [Variable -> Variable -> Formula]
relations = relationsInAtomsType _DEFAULT_ATOMS_TYPE_
