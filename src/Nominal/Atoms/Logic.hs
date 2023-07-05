{-# LANGUAGE CPP, MultiParamTypeClasses #-}
module Nominal.Atoms.Logic where

import Data.List (permutations)
import Data.Map (Map)
import Nominal.Formula.Definition (Formula)
import Nominal.Formula.Constructors (equals, false, lessThan, notEquals, true)
import Nominal.Formula.Operators (and)
import qualified Nominal.Formula.Solver as S
import qualified Nominal.Formula.Quantification as Q
import Nominal.Variable (Variable)
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

#if TOTAL_ORDER

instance AtomsLogic where
    existsVar x = simplifyFormula . Q.existsVar x
    exclusiveConditions = orderedPartitionsFormulas
    forAllVars x = simplifyFormula . Q.forAllVars x
    isTrue = S.isTrue S.lra
    isFalse = S.isFalse S.lra
    simplifyFormula = S.simplifyFormula S.lra
    model = S.model S.lra

#else

instance AtomsLogic where
    existsVar x = simplifyFormula . Q.existsVar x
    exclusiveConditions = partitionsFormulas
    forAllVars x = simplifyFormula . Q.forAllVars x
    isTrue = S.isTrue S.lia
    isFalse = S.isFalse S.lia
    simplifyFormula = S.simplifyFormula S.lia
    model = S.model S.lia

#endif


----------------------------------------------------------------------------------------------------
-- Combinatorics
----------------------------------------------------------------------------------------------------

-- This function enumerates all possible partitions on a set of elements
-- (assuming they are distinct elements). Each partitions consists of
-- 1. a list of constraints (of type c, constructed by rel)
-- 2. a list of classes, each class represented by an element a
-- The number of paritions is given by the n-th Bell number, so this function
-- has an exponential running time.
partitionsWith :: (a -> a -> c) -> [a] -> [([c], [a])]
partitionsWith rel = go []
  where
    go bins vars = case vars of
      -- Empty set => one partition
      [] -> [([], [])]
      -- If there is an element, the function recurses
      (v:remainder) -> do
        newClass <- [True, False]
        -- We make a case distinction whether v will get its own class
        if newClass
          then do
            -- If so, we create a new "bin" and recurse
            -- this increases the number of classes
            (ps, classes) <- go (v:bins) remainder
            return (ps, v:classes)
          else do
            -- If not, v has to be put in an existing bin
            -- and it is related to the representative
            representative <- bins
            (ps, classes) <- go bins remainder
            return (rel representative v:ps, classes)

-- This function enumerates all paritions on a set, and outputs a formula
-- describing each partition. (That means: the formula states which elements
-- are equal, and which are distinct.)
partitionsFormulas :: [Variable] -> [Formula]
partitionsFormulas = fmap mkFormula . partitionsWith equals
  where
    mkFormula (eqConstraints, classes) = and (neqConstraints classes <> eqConstraints)
    neqConstraints = distinctPairsWith notEquals

    -- enumerate all distinct pairs, without enumerating all pairs and then
    -- filtering the distinct ones
    distinctPairsWith op l = case l of
      [] -> []
      (x:xs) -> fmap (op x) xs <> distinctPairsWith op xs

-- This functions enumerates all partitions, including an order on the
-- classes, and outputs a formula for each. The size of the output is
-- given by the "ordered Bell numbers", which grow even faster than
-- the Bell numbers.
orderedPartitionsFormulas :: [Variable] -> [Formula]
orderedPartitionsFormulas = concatMap mkFormulas . partitionsWith equals
  where
    mkFormulas (eqConstraints, classes) = [and (linear order <> eqConstraints) | order <- permutations classes]
    linear ls = zipWith lessThan ls (tail ls)
