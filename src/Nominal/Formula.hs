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

import Nominal.Formula.Definition
import Nominal.Formula.Constructors
import Nominal.Formula.Operators
import Nominal.Formula.Solver
import Prelude hiding (and, not, or)
