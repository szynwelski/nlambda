module Nominal.Automaton.Deterministic where

import Nominal.Atoms
import Nominal.Automaton.Base
import Nominal.Conditional
import Nominal.Formula
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Nominal.Variants

----------------------------------------------------------------------------------------------------
-- Deterministic automaton
----------------------------------------------------------------------------------------------------

-- | The constructor of a deterministic automaton.
da :: (Nominal q, Nominal a) => Set q -> Set a -> (q -> a -> q) -> q -> Set q -> Automaton q a
da q a d i = automaton q a (pairsWith (\s l -> (s,l,d s l)) q a) (singleton i)

-- | The constructor of a deterministic automaton with additional not accepting state.
daWithTrashCan :: (Nominal q, Nominal a) => Set q -> Set a -> (q -> a -> q) -> q -> Set q -> Automaton (Maybe q) a
daWithTrashCan q a d i = automatonWithTrashCan q a (pairsWith (\s l -> (s,l,d s l)) q a) (singleton i)

-- | The constructor of a deterministic automaton with atoms as states.
atomsDA  :: Nominal q => Set q -> (q -> Atom -> q) -> q -> Set q -> Automaton q Atom
atomsDA q = da q atoms

-- | The constructor of a deterministic automaton with atoms as states with additional not accepting state.
atomsDAWithTrashCan :: Nominal q => Set q -> (q -> Atom -> q) -> q -> Set q -> Automaton (Maybe q) Atom
atomsDAWithTrashCan q = daWithTrashCan q atoms

-- | Checks whether an automaton is deterministic.
isDeterministic :: (Nominal q, Nominal a) => Automaton q a -> Formula
isDeterministic aut = isSingleton (initialStates aut) /\
  forAll (`hasSizeLessThan` 2) (pairsWith (transit aut) (states aut) (alphabet aut))

-- | Returns a deterministic automaton that accepts the union of languages accepted by two deterministic automata.
unionDA :: (Nominal q1, Nominal q2, Nominal a) => Automaton q1 a -> Automaton q2 a -> Automaton (q1, q2) a
unionDA (Automaton q1 a d1 i1 f1) (Automaton q2 _ d2 i2 f2) = automaton q a d i f
    where q = pairs q1 q2
          d = pairsWithFilter (\(s1,l,s2) (s1',l',s2') -> maybeIf (eq l l') ((s1,s1'),l,(s2,s2'))) d1 d2
          i = pairs i1 i2
          f = pairs f1 q2 `union` pairs q1 f2

-- | Returns a deterministic automaton that accepts only words rejected by a given automaton.
complementDA :: Nominal q => Automaton q a -> Automaton q a
complementDA (Automaton q a d i f) = Automaton q a d i (q \\ f)

-- | Returns a deterministic automaton that accepts only words accepted by the first automaton and rejeceted by the second.
differenceDA :: (Nominal q1, Nominal q2, Nominal a) => Automaton q1 a -> Automaton q2 a -> Automaton (q1, q2) a
differenceDA aut1 aut2 = intersectionAutomaton aut1 (complementDA aut2)

-- | Checks whether two automata accepts the same languages.
equivalentDA :: (Nominal q1, Nominal q2, Nominal a) => Automaton q1 a -> Automaton q2 a -> Formula
equivalentDA aut1 aut2 = isEmptyAutomaton (differenceDA aut1 aut2) /\ isEmptyAutomaton (differenceDA aut2 aut1)
