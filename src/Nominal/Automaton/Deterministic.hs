module Nominal.Automaton.Deterministic where

import Nominal.Conditional
import Nominal.Formula
import Nominal.Graph
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Nominal.Variants hiding (map)
import Prelude hiding (map, not)

---------------------------------------------------------------------------------------------------
-- Deterministic automaton
----------------------------------------------------------------------------------------------------

data DAutomaton q a = DAutomaton {states :: Set q, alphabet :: Set a, delta :: q -> a -> q,
                                  initialState :: q, finalStates :: Set q}

da :: Set q -> Set a -> (q -> a -> q) -> q -> Set q -> DAutomaton q a
da = DAutomaton

---------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance (Show q, Show a) => Show (DAutomaton q a) where
    show aut = "DAutomaton {states = " ++ show (states aut) ++ ", "
                        ++ "alphabet = " ++ show (alphabet aut) ++ ", "
                        ++ "initialState = " ++ show (initialState aut) ++ ", "
                        ++ "finalStates = " ++ show (finalStates aut) ++ "}"

instance (Conditional q, NominalType q, NominalType a) => Conditional (DAutomaton q a) where
    iF c (DAutomaton s1 a1 d1 i1 f1) (DAutomaton s2 a2 d2 i2 f2) =
        DAutomaton (iF c s1 s2) (iF c a1 a2) (iF c d1 d2) (iF c i1 i2) (iF c f1 f2)

-- TODO instance NominalType (DAutomaton q a)

---------------------------------------------------------------------------------------------------
-- Automaton operations
----------------------------------------------------------------------------------------------------

atomsDA  :: Set q -> (q -> Atom -> q) -> q -> Set q -> DAutomaton q Atom
atomsDA s d i f = da s atoms d i f

acceptsDA :: NominalType q => DAutomaton q a -> [a] -> Formula
acceptsDA aut = contains (finalStates aut) . foldl (delta aut) (initialState aut)

transitionGraph :: (NominalType q, NominalType a) => DAutomaton q a -> Graph q
transitionGraph aut = graph (states aut) (mapFilter transition $ pairs (states aut) (alphabet aut))
    where transition (q, l) = let q' = delta aut q l in iF (contains (states aut) q') (just (q,q')) nothing

isNotEmptyDA :: (NominalType q, NominalType a) => DAutomaton q a -> Formula
isNotEmptyDA aut = intersect (reachable (transitionGraph aut) (initialState aut)) (finalStates aut)

isEmptyDA :: (NominalType q, NominalType a) => DAutomaton q a -> Formula
isEmptyDA = not . isNotEmptyDA

complementDA :: NominalType q => DAutomaton q a -> DAutomaton q a
complementDA (DAutomaton s a d i f) = da s a d i (s \\ f)

pairsAutomaton :: (NominalType q1, NominalType q2) => DAutomaton q1 a -> DAutomaton q2 a -> Set (q1, q2)
    -> DAutomaton (q1, q2) a
pairsAutomaton (DAutomaton s1 a1 d1 i1 _) (DAutomaton s2 _ d2 i2 _) f =
    da (pairs s1 s2) a1 (\(q1,q2) a -> (d1 q1 a,d2 q2 a)) (i1,i2) f

-- FIXME nie działa jeśli delta(q) nie należy do stanów
unionDA :: (NominalType q1, NominalType q2) => DAutomaton q1 a -> DAutomaton q2 a -> DAutomaton (q1, q2) a
unionDA aut1 aut2 = pairsAutomaton aut1 aut2 $ union (pairs (states aut1) (finalStates aut2))
                                                     (pairs (finalStates aut1) (states aut2))

intersectionDA :: (NominalType q1, NominalType q2) => DAutomaton q1 a -> DAutomaton q2 a -> DAutomaton (q1, q2) a
intersectionDA aut1 aut2 = pairsAutomaton aut1 aut2 $ pairs (finalStates aut1) (finalStates aut2)

differenceDA :: (NominalType q1, NominalType q2) => DAutomaton q1 a -> DAutomaton q2 a -> DAutomaton (q1, q2) a
differenceDA aut1 aut2 = intersectionDA aut1 (complementDA aut2)

equivalentDA :: (NominalType q1, NominalType q2, NominalType a) => DAutomaton q1 a -> DAutomaton q2 a -> Formula
equivalentDA aut1 aut2 = isEmptyDA (differenceDA aut1 aut2) /\ isEmptyDA (differenceDA aut2 aut1)
