module Nominal.Formula (
    Formula,

    -- ** Constructors
    true,
    false,
    fromBool,
    equals,
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

    -- variable functions
    foldFormulaVariables,
    mapFormulaVariables,

    -- ** Formula solving
    isTrue,
    isFalse,
    solve) where

import Nominal.Atoms.Type (existsVar, forAllVars, isFalse, isTrue)
import Nominal.Formula.Definition
import Nominal.Formula.Constructors
import Nominal.Formula.Operators
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
