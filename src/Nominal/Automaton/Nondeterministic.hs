module Nominal.Automaton.Nondeterministic where

import Nominal.Conditional
import Nominal.Formula
import Nominal.Maybe
import Nominal.Set
import Nominal.Type

---------------------------------------------------------------------------------------------------
-- Nondeterministic automaton
----------------------------------------------------------------------------------------------------

data NAutomaton a b = NAutomaton {delta :: Set (a, b, a), initialStates :: Set a, finalStates :: Set a} deriving Show

na :: Set (a, b, a) -> Set a -> Set a -> NAutomaton a b
na = NAutomaton

nAccepts :: (NominalType a, NominalType b) => NAutomaton a b -> [b] -> Formula
nAccepts a = isNotEmpty
           . intersection (finalStates a)
           . foldl
               (\s l -> mapFilter (\(s1, l', s2) -> ite (contains s s1 /\ eq l l') (just s2) nothing) (delta a))
               (initialStates a)

nMinimize :: NAutomaton a b -> NAutomaton (Set a) b
nMinimize a = undefined



