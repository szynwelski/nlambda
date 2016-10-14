{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module:         NLambda
Description:    Module for computations over infinite structures.
Stability:      experimental

Module supports computations over infinite structures using logical formulas and SMT solving.
-}
module NLambda where

#if TOTAL_ORDER
import Nominal.Atoms as NLambda
#else
import Nominal.Atoms as NLambda hiding (lt, le, gt, ge)
#endif
import Nominal.Atoms.Signature as NLambda (Constant)
import Nominal.Atoms.Space as NLambda
import Nominal.Automaton.Base as NLambda
import Nominal.Automaton.Deterministic as NLambda
import Nominal.Automaton.Nondeterministic as NLambda
import Nominal.Conditional as NLambda
import Nominal.Contextual as NLambda
import Nominal.Either as NLambda
#if TOTAL_ORDER
import Nominal.Formula as NLambda
#else
import Nominal.Formula as NLambda hiding (lessThan, lessEquals, greaterThan, greaterEquals)
#endif
#if TOTAL_ORDER
import Nominal.Graph as NLambda
#else
import Nominal.Graph as NLambda hiding (monotonicGraph)
#endif
import Nominal.Maybe as NLambda
import Nominal.Orbit as NLambda
#if TOTAL_ORDER
import Nominal.Set as NLambda
#else
import Nominal.Set as NLambda hiding (range, openRange, isLowerBound, hasLowerBound, isUpperBound, hasUpperBound, isMinimum, hasMinimum, isMaximum, hasMaximum, isInfimum, isSupremum, isConnected, isOpen, isClosed, isCompact)
#endif
import Nominal.Type as NLambda (BareNominalType(..), NominalType(..), Scope, MapVarFun, FoldVarFun, neq)
import Nominal.Variable as NLambda (Variable, variable, variableName)
import Nominal.Variants as NLambda (Variants, variant, fromVariant, iteV)
import Prelude hiding (or, and, not, sum, map, filter, maybe)

