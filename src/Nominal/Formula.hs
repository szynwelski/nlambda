module Nominal.Formula (
    Formula,

    -- constructors
    true,
    false,
    fromBool,
    equals,
    lessThan,
    lessEquals,
    greaterThan,
    greaterEquals,

    -- connectives
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

    -- quantifiers
    (∃),
    existsVar,
    (∀),
    forAllVars,

    -- variable functions
    foldFormulaVariables,
    mapFormulaVariables,

    -- solving
    isFalse,
    isTrue,
    solve) where

import Nominal.Atoms.Type (existsVar, forAllVars, isFalse, isTrue)
import Nominal.Formula.Definition
import Nominal.Formula.Constructors
import Nominal.Formula.Operators
import Nominal.Variable (Variable)
import Prelude hiding (and, not, or)

(∃) :: Variable -> Formula -> Formula
(∃) = existsVar

(∀) :: Variable -> Formula -> Formula
(∀) = forAllVars

solve :: Formula -> Maybe Bool
solve f
    | isTrue f  = Just True
    | isFalse f = Just False
    | otherwise = Nothing
