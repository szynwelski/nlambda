module Nominal.Automaton.Nondeterministic where

import Nominal.Atoms
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

-- | The constructor of a nondeterministic automaton.
na :: (Nominal q, Nominal a) => Set q -> Set a -> Set (q, a, q) -> Set q -> Set q -> Automaton q a
na = automaton

-- | The constructor of a nondeterministic automaton with additional not accepting state.
naWithTrashCan  :: (Nominal q, Nominal a) => Set q -> Set a -> Set (q, a, q) -> Set q -> Set q -> Automaton (Maybe q) a
naWithTrashCan = automatonWithTrashCan

-- | The constructor of a nondeterministic automaton with atoms as states.
atomsNA :: Nominal q => Set q -> Set (q, Atom, q) -> Set q -> Set q -> Automaton q Atom
atomsNA q = na q atoms

-- | The constructor of a nondeterministic automaton with atoms as states with additional not accepting state.
atomsNAWithTrashCan :: Nominal q => Set q -> Set (q, Atom, q) -> Set q -> Set q -> Automaton (Maybe q) Atom
atomsNAWithTrashCan q = naWithTrashCan q atoms

-- | Checks whether an automaton is nondeterministic.
isNondeterministic :: (Nominal q, Nominal a) => Automaton q a -> Formula
isNondeterministic = not . isDeterministic
