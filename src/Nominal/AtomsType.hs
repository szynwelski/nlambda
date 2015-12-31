{-# LANGUAGE CPP #-}
module Nominal.AtomsType (existsVar, forAllVars, isTrue, isFalse, relations) where

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
-- Current atoms type
----------------------------------------------------------------------------------------------------

currentAtomsType :: AtomsType
#if TOTAL_ORDER
currentAtomsType = AtomsWithTotalOrder
#else
currentAtomsType = AtomsWithEquality
#endif

-- | Creates a formula representing ∃x.f
existsVar :: Variable -> Formula -> Formula
existsVar = existsVarInAtomsType currentAtomsType

-- | Creates a formula representing ∀x.f
forAllVars :: Variable -> Formula -> Formula
forAllVars = forAllVarsInAtomsType currentAtomsType

-- | Checks whether the formula is a tautology.
isTrue :: Formula -> Bool
isTrue = isTrueInAtomsType currentAtomsType

-- | Checks whether the formula is a contradiction.
isFalse :: Formula -> Bool
isFalse = isFalseInAtomsType currentAtomsType

relations :: [Variable -> Variable -> Formula]
relations = relationsInAtomsType currentAtomsType
