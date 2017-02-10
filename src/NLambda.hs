{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module:         NLambda
Description:    Module for computations over infinite structures.
Stability:      experimental

Module supports computations over infinite structures using logical formulas and SMT solving.
-}
module NLambda (module Export) where

#if TOTAL_ORDER
import Nominal.Atoms as Export
#else
import Nominal.Atoms as Export hiding (lt, le, gt, ge)
#endif
import Nominal.Atoms.Signature as Export (Constant)
import Nominal.Atoms.Space as Export
import Nominal.Automaton.Base as Export
import Nominal.Automaton.Deterministic as Export
import Nominal.Automaton.Nondeterministic as Export
import Nominal.Conditional as Export
import Nominal.Contextual as Export
import Nominal.Either as Export
#if TOTAL_ORDER
import Nominal.Formula as Export
#else
import Nominal.Formula as Export hiding (lessThan, lessEquals, greaterThan, greaterEquals)
#endif
#if TOTAL_ORDER
import Nominal.Graph as Export
#else
import Nominal.Graph as Export hiding (monotonicGraph)
#endif
import Nominal.Maybe as Export
import Nominal.Orbit as Export
#if TOTAL_ORDER
import Nominal.Set as Export
#else
import Nominal.Set as Export hiding (range, openRange, isLowerBound, hasLowerBound, isUpperBound, hasUpperBound, isMinimum, hasMinimum, isMaximum, hasMaximum, isInfimum, isSupremum, isConnected, isOpen, isClosed, isCompact)
#endif
import Nominal.Type as Export (NominalType(..), Scope, MapVarFun, FoldVarFun, neq)
import Nominal.Variable as Export (Variable, variable)
import Nominal.Variants as Export (Variants, variant, fromVariant, iteV)
import Prelude hiding (or, and, not, sum, map, filter, maybe)

