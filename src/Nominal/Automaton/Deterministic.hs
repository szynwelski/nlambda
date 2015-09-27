module Nominal.Automaton.Deterministic where

import Nominal.Atom
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

da :: (NominalType q, NominalType a) => Set q -> Set a -> (q -> a -> q) -> q -> Set q -> Automaton q a
da q a d i f = automaton q a (pairsWith (\s l -> (s,l,d s l)) q a) (singleton i) f

isDeterministic :: (NominalType q, NominalType a) => Automaton q a -> Formula
isDeterministic aut = forall (`hasSizeLessThan` 2) (pairsWith (\s l -> transit aut s l) (states aut) (alphabet aut))

atomsDA  :: NominalType q => Set q -> (q -> Atom -> q) -> q -> Set q -> Automaton q Atom
atomsDA q d i f = da q atoms d i f

-- TODO check alphabet ? union ?
unionDA :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a -> Automaton (Maybe q1, Maybe q2) a
unionDA (Automaton q1 a d1 i1 f1) (Automaton q2 _ d2 i2 f2) = automaton q a d i f
    where q = pairs q1 q2
          d = pairsWithFilter (\(s1,l,s2) (s1',l',s2') -> maybeIf (eq l l') ((s1,s1'),l,(s2,s2'))) d1 d2
          i = pairs i1 i2
          f = union (pairs f1 q2) (pairs q1 f2)

complementDA :: NominalType q => Automaton q a -> Automaton q a
complementDA (Automaton q a d i f) = Automaton q a d i (q \\ f)

differenceDA :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a -> Automaton (q1, q2) a
differenceDA aut1 aut2 = intersectionAutomaton aut1 (complementDA aut2)

equivalentDA :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a -> Formula
equivalentDA aut1 aut2 = isEmptyAutomaton (differenceDA aut1 aut2) /\ isEmptyAutomaton (differenceDA aut2 aut1)
