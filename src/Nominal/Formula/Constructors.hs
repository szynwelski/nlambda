module Nominal.Formula.Constructors where

import Data.Set (fromList)
import Nominal.Formula.Definition
import Nominal.Formula.Simplifier
import Nominal.Variable (Variable)
import Prelude hiding (not)

----------------------------------------------------------------------------------------------------
-- Formula constructors
----------------------------------------------------------------------------------------------------

-- true
true :: Formula
true = T

-- false
false :: Formula
false = F

-- from bool
fromBool :: Bool -> Formula
fromBool True = T
fromBool False = F

-- constraints
equals :: Variable -> Variable -> Formula
equals x1 x2 = simplifyFormula $ Constraint Equals x1 x2

lessThan :: Variable -> Variable -> Formula
lessThan x1 x2 = simplifyFormula $ Constraint LessThan x1 x2

lessEquals :: Variable -> Variable -> Formula
lessEquals x1 x2 = simplifyFormula $ Constraint LessEquals x1 x2

greaterThan :: Variable -> Variable -> Formula
greaterThan x1 x2 = simplifyFormula $ Constraint GreaterThan x1 x2

greaterEquals :: Variable -> Variable -> Formula
greaterEquals x1 x2 = simplifyFormula $ Constraint GreaterEquals x1 x2

-- and
(/\) :: Formula -> Formula -> Formula
f1 /\ f2 = simplifyFormula $ And $ fromList [f1, f2]

and :: [Formula] -> Formula
and [] = T
and fs = foldr1 (/\) fs

-- or
(\/) :: Formula -> Formula -> Formula
f1 \/ f2 = simplifyFormula $ Or $ fromList [f1, f2]

or :: [Formula] -> Formula
or [] = F
or fs = foldr1 (\/) fs

-- not
not :: Formula -> Formula
not f = simplifyFormula $ Not f

-- imply
infix 8 ==>
(==>) :: Formula -> Formula -> Formula
f1 ==> f2 = not f1 \/ f2

infix 8 <==
(<==) :: Formula -> Formula -> Formula
f1 <== f2 = f1 \/ not f2

implies :: Formula -> Formula -> Formula
implies = (==>)

-- equivalent
infix 8 <==>
(<==>) :: Formula -> Formula -> Formula
f1 <==> f2 = (f1 ==> f2) /\ (f1 <== f2)

iff :: Formula -> Formula -> Formula
iff = (<==>)

-- for all
(∀) :: Variable -> Formula -> Formula
(∀) x f = simplifyFormula $ ForAll x f

forAllVars :: Variable -> Formula -> Formula
forAllVars = (∀)

-- exists
(∃) :: Variable -> Formula -> Formula
(∃) x f = simplifyFormula $ Exists x f

existsVar :: Variable -> Formula -> Formula
existsVar = (∃)
