module NLambda
(
module Nominal.Atom,
module Nominal.Automaton.Base,
module Nominal.Automaton.Deterministic,
module Nominal.Automaton.Nondeterministic,
module Nominal.Conditional,
module Nominal.Contextual,
module Nominal.Either,
module Nominal.Formula,
module Nominal.Graph,
module Nominal.Maybe,
module Nominal.Orbit,
module Nominal.Set,
module Nominal.Type,
module Nominal.Variable,
module Nominal.VariablesSpace,
module Nominal.Variants,
a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
)
where

import Nominal.Atom
import Nominal.Automaton.Base
import Nominal.Automaton.Deterministic
import Nominal.Automaton.Nondeterministic
import Nominal.Conditional
import Nominal.Contextual
import Nominal.Either
import Nominal.Formula
import Nominal.Graph
import Nominal.Maybe
import Nominal.Orbit
import Nominal.Set
import Nominal.Type (NominalType(eq), neq)
import Nominal.Variable (Variable, variable, variableName)
import Nominal.VariablesSpace
import Nominal.Variants (Variants, fromVariant, iteV, iteV', variant)
import Prelude hiding (or, and, not, sum, map, filter, maybe)

----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------
a = atom "a"
b = atom "b"
c = atom "c"
d = atom "d"
e = atom "e"
f = atom "f"
g = atom "g"
h = atom "h"
i = atom "i"
j = atom "j"
k = atom "k"
l = atom "l"
m = atom "m"
n = atom "n"
o = atom "o"
p = atom "p"
q = atom "q"
r = atom "r"
s = atom "s"
t = atom "t"
u = atom "u"
v = atom "v"
w = atom "w"
x = atom "x"
y = atom "y"
z = atom "z"

-- example program

nlProgram = do
    a <- newAtom
    b <- newAtom
    return $ let set = delete a atoms
             in delete b set
