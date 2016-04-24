{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module:         NLambda
Description:    Module for computations over infinite structures.
Stability:      experimental

Module supports computations over infinite structures using logical formulas and SMT solving.
-}
module NLambda
(
-- * Formula
-- ** Variable
module Nominal.Variable,
-- ** Type
module Nominal.Formula,
-- * Nominal type
module Nominal.Type,
-- * Conditional
module Nominal.Conditional,
-- * Contextual
module Nominal.Contextual,
-- * Variants
module Nominal.Variants,
-- ** Atom
module Nominal.Atoms,
module Nominal.Atoms.Space,
module Nominal.Atoms.Signature,
-- ** Either
module Nominal.Either,
-- ** Maybe
module Nominal.Maybe,
-- * Nominal set
module Nominal.Set,
-- * Group action, support and orbits
module Nominal.Orbit,
-- * Graph
module Nominal.Graph,
-- * Automaton
module Nominal.Automaton.Base,
-- ** Deterministic automaton
module Nominal.Automaton.Deterministic,
-- ** Nondeterministic automaton
module Nominal.Automaton.Nondeterministic) where

#if TOTAL_ORDER
import Nominal.Atoms
#else
import Nominal.Atoms hiding (lt, le, gt, ge)
#endif
import Nominal.Atoms.Signature (Constant)
import Nominal.Atoms.Space
import Nominal.Automaton.Base
import Nominal.Automaton.Deterministic
import Nominal.Automaton.Nondeterministic
import Nominal.Conditional
import Nominal.Contextual
import Nominal.Either
#if TOTAL_ORDER
import Nominal.Formula
#else
import Nominal.Formula hiding (lessThan, lessEquals, greaterThan, greaterEquals)
#endif
#if TOTAL_ORDER
import Nominal.Graph
#else
import Nominal.Graph hiding (monotonicGraph)
#endif
import Nominal.Maybe
import Nominal.Orbit
#if TOTAL_ORDER
import Nominal.Set
#else
import Nominal.Set hiding (range, openRange, isLowerBound, hasLowerBound, isUpperBound, hasUpperBound, isMinimum, hasMinimum, isMaximum, hasMaximum, isInfimum, isSupremum, isConnected, isOpen, isClosed, isCompact)
#endif
import Nominal.Type (BareNominalType(..), NominalType(..), Scope, MapVarFun, FoldVarFun, neq)
import Nominal.Variable (Variable, variable, variableName)
import Nominal.Variants (Variants, variant, fromVariant, iteV)
import Prelude hiding (or, and, not, sum, map, filter, maybe)

