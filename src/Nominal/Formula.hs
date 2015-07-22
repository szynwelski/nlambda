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
    (∃),
    existsVar,
    (∀),
    forAllVars,

    -- operations
    foldFormulaVariables,
    mapFormulaVariables,

    -- solving
    isFalse,
    isTrue,
    solve) where

import Nominal.Formula.Definition
import Nominal.Formula.Constructors
import Nominal.Formula.Simplifier
import Nominal.Formula.Solver
import Prelude hiding (and, not, or)
