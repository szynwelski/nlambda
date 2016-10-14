{-# LANGUAGE CPP, MultiParamTypeClasses #-}
module Nominal.Atoms.Logic where

import Data.List (permutations)
import Data.Map (Map)
import Nominal.Formula.Definition (Formula)
import Nominal.Formula.Constructors (equals, false, lessThan, notEquals, true)
import Nominal.Formula.Operators (simplifiedAnd)
import qualified Nominal.Formula.Solver as S
import qualified Nominal.Formula.Quantification as Q
import Nominal.Variable (Variable)
import Math.Combinat.Partitions.Set (fromSetPartition, setPartitions)
import Prelude hiding (and)

----------------------------------------------------------------------------------------------------
-- Atoms logic
----------------------------------------------------------------------------------------------------

class AtomsLogic where
    -- | Creates a formula representing ∃x.f
    existsVar :: Variable -> Formula -> Formula

    -- | Returns conditions for all orbits for given list of variables
    exclusiveConditions :: [Variable] -> [Formula]

    -- | Creates a formula representing ∀x.f
    forAllVars :: Variable -> Formula -> Formula

    -- | Checks whether the formula is a tautology.
    isTrue :: Formula -> Bool

    -- | Checks whether the formula is a contradiction.
    isFalse :: Formula -> Bool

    -- | Simplify given formula.
    simplifyFormula :: Formula -> Formula

    -- | Returns a model for a given formula or report an error when formula is unsatisfied.
    model :: Formula -> Map Variable Variable

----------------------------------------------------------------------------------------------------
-- Current atoms logic
----------------------------------------------------------------------------------------------------

partitions :: [Variable] -> [[[Variable]]]
partitions vars = fmap (fmap (fmap $ (vars !!) . pred) . fromSetPartition) (setPartitions $ length vars)

sortedPartitions :: [Variable] -> [[[Variable]]]
sortedPartitions = concatMap permutations . partitions

consecutiveRelations :: (Variable -> Variable -> Formula) -> [Variable] -> [Formula]
consecutiveRelations rel vs = uncurry rel <$> zip vs (tail vs)

pairwiseDifferent :: [Variable] -> [Formula]
pairwiseDifferent vs = [notEquals v1 v2 | v1 <- vs, v2 <- vs, v1 < v2]

equivalenceClasses :: ([Variable] -> [Formula]) -> [[Variable]] -> Formula
equivalenceClasses classRelations parts = simplifiedAnd (classRelations (fmap head parts) ++ classes)
    where classes = concatMap (consecutiveRelations equals) parts

#if TOTAL_ORDER

instance AtomsLogic where
    existsVar x = simplifyFormula . Q.existsVar x
    exclusiveConditions = fmap (equivalenceClasses $ consecutiveRelations lessThan) . sortedPartitions
    forAllVars x = simplifyFormula . Q.forAllVars x
    isTrue = S.isTrue S.lra
    isFalse = S.isFalse S.lra
    simplifyFormula = S.simplifyFormula S.lra
    model = S.model S.lra

#else

instance AtomsLogic where
    existsVar x = simplifyFormula . Q.existsVar x
    exclusiveConditions = fmap (equivalenceClasses pairwiseDifferent) . partitions
    forAllVars x = simplifyFormula . Q.forAllVars x
    isTrue = S.isTrue S.lia
    isFalse = S.isFalse S.lia
    simplifyFormula = S.simplifyFormula S.lia
    model = S.model S.lia

#endif
