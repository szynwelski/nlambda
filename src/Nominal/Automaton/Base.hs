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
    ite c (Automaton q1 a1 d1 i1 f1) (Automaton q2 a2 d2 i2 f2) =
        Automaton (ite c q1 q2) (ite c a1 a2) (ite c d1 d2) (ite c i1 i2) (ite c f1 f2)

instance (NominalType q, NominalType a) => NominalType (Automaton q a) where
    eq (Automaton q1 a1 d1 i1 f1) (Automaton q2 a2 d2 i2 f2) = eq q1 q2 /\ eq a1 a2 /\ eq d1 d2 /\ eq i1 i2 /\ eq f1 f2
    mapVariables fun (Automaton q a d i f) = Automaton (mapVariables fun q) (mapVariables fun a) (mapVariables fun d)
                                                       (mapVariables fun i) (mapVariables fun f)
    foldVariables fun acc (Automaton q a d i f) =
        foldVariables fun (foldVariables fun (foldVariables fun (foldVariables fun (foldVariables fun acc q) a) d) i) f
    simplify (Automaton q a d i f) = Automaton (simplify q) (simplify a) (simplify d) (simplify i) (simplify f)

----------------------------------------------------------------------------------------------------
-- Automaton functions
----------------------------------------------------------------------------------------------------

transitFromStates :: (NominalType q, NominalType a) => Automaton q a -> (q -> Formula) -> a -> Set q
transitFromStates aut cf l = mapFilter (\(s1, l', s2) -> when (cf s1 /\ eq l l') s2) (delta aut)

transit :: (NominalType q, NominalType a) => Automaton q a -> q -> a -> Set q
transit aut s = transitFromStates aut (eq s)

transitSet :: (NominalType q, NominalType a) => Automaton q a -> Set q -> a -> Set q
transitSet aut ss = transitFromStates aut (contains ss)

accepts :: (NominalType q, NominalType a) => Automaton q a -> [a] -> Formula
accepts aut = intersect (finalStates aut) . foldl (transitSet aut) (initialStates aut)

transitionGraph :: (NominalType q, NominalType a) => Automaton q a -> Graph q
transitionGraph aut = graph (states aut) (map (\(s1, _, s2) -> (s1, s2)) $ delta aut)

isNotEmptyAutomaton :: (NominalType q, NominalType a) => Automaton q a -> Formula
isNotEmptyAutomaton aut = intersect (reachableFromSet (transitionGraph aut) (initialStates aut)) (finalStates aut)

isEmptyAutomaton :: (NominalType q, NominalType a) => Automaton q a -> Formula
isEmptyAutomaton = not . isNotEmptyAutomaton

pairsDelta :: (NominalType q1, NominalType q2, NominalType a) => Set (q1,a,q1) -> Set (q2,a,q2) -> Set ((q1,q2),a,(q1,q2))
pairsDelta d1 d2 = pairsWithFilter (\(s1,l,s2) (s1',l',s2') -> when (eq l l') ((s1,s1'),l,(s2,s2'))) d1 d2

pairsAutomaton :: (NominalType q1, NominalType q2, NominalType a) =>
    Automaton q1 a -> Automaton q2 a -> Set a -> Set (q1, q2) -> Automaton (q1, q2) a
pairsAutomaton (Automaton q1 _ d1 i1 _) (Automaton q2 _ d2 i2 _) a f =
    Automaton (pairs q1 q2) a (pairsDelta d1 d2) (pairs i1 i2) f

-- FIXME nie działa dla delta(q) nie należącego do stanów
unionAutomaton :: (NominalType q1, NominalType q2, NominalType a) =>
    Automaton q1 a -> Automaton q2 a -> Automaton (q1, q2) a
unionAutomaton aut1 aut2 = pairsAutomaton aut1 aut2 (union (alphabet aut1) (alphabet aut2))
                           (union (pairs (states aut1) (finalStates aut2)) (pairs (finalStates aut1) (states aut2)))

intersectionAutomaton :: (NominalType q1, NominalType q2, NominalType a) =>
    Automaton q1 a -> Automaton q2 a -> Automaton (q1, q2) a
intersectionAutomaton aut1 aut2 = pairsAutomaton aut1 aut2 (intersection (alphabet aut1) (alphabet aut2))
                           (pairs (finalStates aut1) (finalStates aut2))

----------------------------------------------------------------------------------------------------
-- Automaton minimization
----------------------------------------------------------------------------------------------------

--minimize :: Automaton q a -> Automaton (Set q) a
minimize aut@(Automaton q a d i f) = simplify equiv
    where relGraph = graph (square q) (map (\(s1,_,s2) -> (s1,s2)) $ pairsDelta d d)
          nf = q \\ f
          equiv = square q \\ reachableFromSet (reverseEdges relGraph) (union (pairs nf f) (pairs f nf))
          q' = map vertices (stronglyConnectedComponents $ graph q equiv)

