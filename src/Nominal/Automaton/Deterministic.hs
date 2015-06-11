module Nominal.Automaton.Deterministic where

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
da q a d i f = Automaton q a d' (singleton i) f
    where d' = pairsWithFilter (\s l -> let s' = d s l in iF (contains q s') (just (s,l,s')) nothing) q a


isDeterministic :: (NominalType q, NominalType a) => Automaton q a -> Formula
isDeterministic aut = forall (flip hasSizeLessThan 2)
                             (pairsWith (\s l -> transit aut (singleton s) l) (states aut) (alphabet aut))

atomsDA  :: NominalType q => Set q -> (q -> Atom -> q) -> q -> Set q -> Automaton q Atom
atomsDA q d i f = da q atoms d i f

complementDA :: NominalType q => Automaton q a -> Automaton q a
complementDA (Automaton q a d i f) = Automaton q a d i (q \\ f)

differenceDA :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a -> Automaton (q1, q2) a
differenceDA aut1 aut2 = intersectionAutomaton aut1 (complementDA aut2)

equivalentDA :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a -> Formula
equivalentDA aut1 aut2 = isEmptyAutomaton (differenceDA aut1 aut2) /\ isEmptyAutomaton (differenceDA aut2 aut1)
