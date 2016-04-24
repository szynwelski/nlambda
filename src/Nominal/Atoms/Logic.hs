{-# LANGUAGE CPP, NullaryTypeClasses #-}
module Nominal.Atoms.Logic where

import Nominal.Formula.Definition (Formula)
import qualified Nominal.Formula.Solver as S
import qualified Nominal.Formula.Quantification as Q
import Nominal.Variable (Variable)

----------------------------------------------------------------------------------------------------
-- Atoms logic
----------------------------------------------------------------------------------------------------

class AtomsLogic where
    -- | Creates a formula representing ∃x.f
    existsVar :: Variable -> Formula -> Formula

    -- | Creates a formula representing ∀x.f
    forAllVars :: Variable -> Formula -> Formula

    -- | Checks whether the formula is a tautology.
    isTrue :: Formula -> Bool

    -- | Checks whether the formula is a contradiction.
    isFalse :: Formula -> Bool

    -- | Simplify given formula
    simplifyFormula :: Formula -> Formula

----------------------------------------------------------------------------------------------------
-- Current atoms logic
----------------------------------------------------------------------------------------------------

#if TOTAL_ORDER

instance AtomsLogic where
    existsVar x = simplifyFormula . Q.existsVar x
    forAllVars x = simplifyFormula . Q.forAllVars x
    isTrue = S.isTrue S.lra
    isFalse = S.isFalse S.lra
    simplifyFormula = S.simplifyFormula S.lra

#else

instance AtomsLogic where
    existsVar x = simplifyFormula . Q.existsVar x
    forAllVars x = simplifyFormula . Q.forAllVars x
    isTrue = S.isTrue S.lia
    isFalse = S.isFalse S.lia
    simplifyFormula = S.simplifyFormula S.lia

#endif
