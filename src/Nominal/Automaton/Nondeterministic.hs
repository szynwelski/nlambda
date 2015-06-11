module Nominal.Automaton.Nondeterministic where

import Nominal.Automaton.Base
import Nominal.Automaton.Deterministic (isDeterministic)
import Nominal.Formula
import Nominal.Set
import Nominal.Type
import Nominal.Variants
import Prelude hiding (not)

----------------------------------------------------------------------------------------------------
-- Nondeterministic automaton
----------------------------------------------------------------------------------------------------

na :: (NominalType q, NominalType a) => Set q -> Set a -> Set (q, a, q) -> Set q -> Set q -> Automaton q a
na q a d i f = Automaton q a (intersection d $ triples q a q) i f

atomsNA :: NominalType q => Set q -> Set (q, Atom, q) -> Set q -> Set q -> Automaton q Atom
atomsNA q d i f = na q atoms d i f

isNondeterministic :: (NominalType q, NominalType a) => Automaton q a -> Formula
isNondeterministic = not . isDeterministic
