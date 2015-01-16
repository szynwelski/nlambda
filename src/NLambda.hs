module NLambda where

import Formula
import Formula.Solver
import Nominal.Conditional
import Nominal.Set
import Nominal.Type
import Nominal.VariablesSpace
import Nominal.Variants hiding (map)
import Prelude hiding (or, and, not, sum, map, filter)

----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------
x = Variable "x"
y = Variable "y"
z = Variable "z"
cc = eq x y
ncc = not cc
ce = (eq x y) /\ (eq y z) /\ (eq z x)
nce =  (eq x y) /\ (eq y z) /\ not (eq z x)
ice = (eq x y) /\ (eq y z) ==> (eq z x)
af = (∀) x cc
ef = (∃) x cc
aef = (∀) x $ (∃) y cc
naef = not aef
eaf = (∃) x $ (∀) y cc
aaf = (∀) x $ (∀) y cc
eef = (∃) x $ (∃) y cc

a = atom "a"
b = atom "b"
c = atom "c"
cond = eq a b
at = iF cond a b
set1 = fromList [a, b]
set2 = just at
sa = atomSet

-- example program

nlProgram = do
    a <- newAtom
    b <- newAtom
    return $ let set = insert a atomSet
             in insert b set


