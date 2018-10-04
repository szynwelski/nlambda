{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
module Nominal.Formula (
    Formula,

    -- ** Constructors
    true,
    false,
    fromBool,
    equals,
    notEquals,
    lessThan,
    lessEquals,
    greaterThan,
    greaterEquals,

    -- ** Connectives
    (/\),
    and,
    (\/),
    or,
    not,
    (==>),
    (<==),
    implies,
    (<==>),
    iff,

    -- ** Quantifiers
    existsVar,
    (∃),
    forAllVars,
    (∀),

    -- ** Formula solving
    isTrue,
    isFalse,
    model,
    simplifyFormula,
    solve) where

import Nominal.Atoms.Logic (existsVar, forAllVars, model, isFalse, isTrue, simplifyFormula)
import Nominal.Formula.Definition
import Nominal.Variable (Variable)
import Prelude hiding (and, not, or)

-- | Equivalent to 'existsVar'.
(∃) :: Variable -> Formula -> Formula
(∃) = existsVar

-- | Equivalent to 'forAllVars'.
(∀) :: Variable -> Formula -> Formula
(∀) = forAllVars

-- | Returns:
--
-- * 'Just' 'True' if formula is a tautology,
-- * 'Just' 'False' if formula is a contradiction,
-- * 'Nothing' otherwise.
solve :: Formula -> Maybe Bool
solve f
    | isTrue f  = Just True
    | isFalse f = Just False
    | otherwise = Nothing



