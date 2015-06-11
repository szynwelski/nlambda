module Nominal.Automaton.Base where

import Nominal.Conditional
import Nominal.Formula
import Nominal.Graph
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Prelude hiding (map, not)

----------------------------------------------------------------------------------------------------
-- Definition of automaton
----------------------------------------------------------------------------------------------------

data Automaton q a = Automaton {states :: Set q, alphabet :: Set a, delta :: Set (q, a, q),
                                initialStates :: Set q, finalStates :: Set q} deriving (Eq, Ord, Show)

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance (Conditional q, NominalType q, NominalType a) => Conditional (Automaton q a) where
    iF c (Automaton q1 a1 d1 i1 f1) (Automaton q2 a2 d2 i2 f2) =
        Automaton (iF c q1 q2) (iF c a1 a2) (iF c d1 d2) (iF c i1 i2) (iF c f1 f2)

instance (NominalType q, NominalType a) => NominalType (Automaton q a) where
    eq (Automaton q1 a1 d1 i1 f1) (Automaton q2 a2 d2 i2 f2) = eq q1 q2 /\ eq a1 a2 /\ eq d1 d2 /\ eq i1 i2 /\ eq f1 f2
    mapVariables fun (Automaton q a d i f) = Automaton (mapVariables fun q) (mapVariables fun a) (mapVariables fun d)
                                                       (mapVariables fun i) (mapVariables fun f)
    foldVariables fun acc (Automaton q a d i f) =
        foldVariables fun (foldVariables fun (foldVariables fun (foldVariables fun (foldVariables fun acc q) a) d) i) f
    simplify (Automaton q a d i f) = Automaton (simplify q) (simplify a) (simplify d) (simplify i) (simplify f)

----------------------------------------------------------------------------------------------------
-- Automaton operations
----------------------------------------------------------------------------------------------------

transit :: (NominalType q, NominalType a) => Automaton q a -> Set q -> a -> Set q
transit aut ss l = mapFilter (\(s1, l', s2) -> iF (contains ss s1 /\ eq l l') (just s2) nothing) (delta aut)

accepts :: (NominalType q, NominalType a) => Automaton q a -> [a] -> Formula
accepts aut = intersect (finalStates aut) . foldl (transit aut) (initialStates aut)

transitionGraph :: (NominalType q, NominalType a) => Automaton q a -> Graph q
transitionGraph aut = graph (states aut) (map (\(s1, _, s2) -> (s1, s2)) $ delta aut)

isNotEmptyAutomaton :: (NominalType q, NominalType a) => Automaton q a -> Formula
isNotEmptyAutomaton aut = intersect (reachableFromSet (transitionGraph aut) (initialStates aut)) (finalStates aut)

isEmptyAutomaton :: (NominalType q, NominalType a) => Automaton q a -> Formula
isEmptyAutomaton = not . isNotEmptyAutomaton

pairsAutomaton :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a
    -> Set a -> Set (q1, q2) -> Automaton (q1, q2) a
pairsAutomaton (Automaton q1 _ d1 i1 _) (Automaton q2 _ d2 i2 _) a f = Automaton (pairs q1 q2) a d (pairs i1 i2) f
    where d = pairsWithFilter (\(s1,l,s2) (s1',l',s2') -> when (eq l l') ((s1,s1'),l,(s2,s2'))) d1 d2

-- FIXME nie działa dla delta(q) nie należącego do stanów
unionAutomaton :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a
    -> Automaton (q1, q2) a
unionAutomaton aut1 aut2 = pairsAutomaton aut1 aut2 (union (alphabet aut1) (alphabet aut2))
                           (union (pairs (states aut1) (finalStates aut2)) (pairs (finalStates aut1) (states aut2)))

intersectionAutomaton :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a
    -> Automaton (q1, q2) a
intersectionAutomaton aut1 aut2 = pairsAutomaton aut1 aut2 (intersection (alphabet aut1) (alphabet aut2))
                           (pairs (finalStates aut1) (finalStates aut2))
