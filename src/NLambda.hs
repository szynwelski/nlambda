module NLambda where

import Nominal.Atom
import Nominal.Automaton.Base
import Nominal.Automaton.Deterministic
import Nominal.Automaton.Nondeterministic
import Nominal.Conditional
import Nominal.Either
import Nominal.Formula
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
w = variable "w"
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
d = atom "d"
e = atom "e"
cond = eq a b
at = ite cond a b
set1 = singleton at
set2 = fromList [a, b]
set3 = fromList [a, b, c]
sa = atoms
del = delete a sa
ts = triples sa sa sa

a1 = variant $ iterationVariable 0 1
a2 = variant $ iterationVariable 0 2
b1 = variant $ iterationVariable 1 1
b2 = variant $ iterationVariable 1 2

-- example program

nlProgram = do
    a <- newAtom
    b <- newAtom
    return $ let set = insert a atoms
             in insert b set


-- graph

g = atomsGraph $ filter (\(x,y) -> eq x a \/ eq y a) atomsPairs
gIn = atomsGraph $ filter (eq a . snd) atomsPairs
gOut = atomsGraph $ filter (eq a . fst) atomsPairs
gAB = addEdge (a,b) emptyGraph
bigraph = atomsGraph $ filter (\(x,y) -> (lt x a /\ lt y a) \/ (gt x a /\ gt y a)) atomsPairs
bigraphMonotonic = atomsGraph $ filter (\(x,y) -> (lt x y) /\ ((lt x a /\ lt y a) \/ (gt x a /\ gt y a))) atomsPairs

-- auto

f1 = le a b
f2 = le b c
f3 = le c d
f4 = le d e

result = simplify $ accepts (atomsDA (fromList [a,b,c]) (\x y -> ite (eq x y) a b) a (singleton c)) [a,b,c]
-- ((a /= b || a /= c) && (b /= c || a = b) && b = c) || (((a /= b && b = c) || (a = b && a = c)) && a = c)
-- (((a /= b && b = c) || (a = b && a = c)) && a = c) || (((a /= b || a /= c) && (b /= c || a = b)) && b = c)
result1 = eq b c /\ (neq a b \/ neq a c) /\ (neq b c \/ eq a b) -- -> false

toMinAuto = atomsDA (replicateAtomsUntil 3) (flip (:)) [] (filter (\[a1,a2,a3] -> eq a1 a2 \/ eq a1 a3) $ replicateAtoms 3)
parityAuto = atomsDA (fromList [0,1]) (\q _ -> mod (succ q) 2) 0 (singleton 0) :: Automaton Int Atom

result2 = simplify $ equivalentDA (differenceDA toMinAuto parityAuto) toMinAuto
result3 = simplify $ equivalentDA (differenceDA parityAuto toMinAuto) parityAuto
result4 = minimize toMinAuto
result5 = accepts toMinAuto [a,b,c]
gg = graph (square (states toMinAuto)) (map (\(s1,_,s2) -> (s1,s2)) $ pairsDelta (delta toMinAuto) (delta toMinAuto))
