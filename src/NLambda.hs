module NLambda where

import Formula
import Formula.Solver
import Nominal.Conditional
import Nominal.Either
import Nominal.Graph
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Nominal.Variable
import Nominal.VariablesSpace
import Nominal.Variants hiding (filter, fromList, map)
import Prelude hiding (or, and, not, sum, map, filter, maybe)

----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------
x = variable "x"
y = variable "y"
z = variable "z"
cc = eq x y
ncc = not cc
ce = (eq x y) /\ (eq y z) /\ (eq z x)
nce =  (eq x y) /\ (eq y z) /\ not (eq z x)
ice = (eq x y) /\ (eq y z) ==> (eq z x)
af = (∀) y cc
ef = (∃) y cc
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
set2 = singleton at
sa = atoms
da = delete a sa
ps = pairs sa sa
ts = triples sa sa sa

a1 = variant $ iterationVariable 0 1
b1 = variant $ iterationVariable 1 1

isSingleton :: NominalType a => Set a -> Formula
isSingleton s = notContains (sum $ map (\x -> map (\y -> eq x y) s) s) false

-- example program

nlProgram = do
    a <- newAtom
    b <- newAtom
    return $ let set = insert a atoms
             in insert b set

