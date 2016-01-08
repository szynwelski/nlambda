module Nominal.Automaton.Base where

import Nominal.Conditional
import Nominal.Contextual
import Nominal.Formula
import Nominal.Graph
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Prelude hiding (filter, map, not)

----------------------------------------------------------------------------------------------------
-- Definition of automaton
----------------------------------------------------------------------------------------------------

-- | An automaton with a set of state with type __q__ accepting\/rejecting words from an alphabet with type __a__.
data Automaton q a = Automaton {states :: Set (Maybe q), alphabet :: Set a, delta :: Set (Maybe q, a, Maybe q),
                                initialStates :: Set (Maybe q), finalStates :: Set (Maybe q)} deriving (Eq, Ord, Show)

-- | An automaton constructor.
automaton :: (NominalType q, NominalType a) => Set q -> Set a -> Set (q, a, q) -> Set q -> Set q -> Automaton q a
automaton q a d i f = onlyReachable $ Automaton q' a (union d1 d2) i' f'
    where q' = insert Nothing $ map Just q
          d1 = mapFilter (\(s1,l,s2) -> maybeIf (contains q s1 /\ contains q s2) (Just s1,l,Just s2)) d
          d2 = map (\(s1,l) -> (s1,l,Nothing)) ((pairs q' a) \\ (map (\(s,l,_)-> (s,l)) d1  ))
          i' = map Just (intersection q i)
          f' = map Just (intersection q f)

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance (Conditional q, NominalType q, NominalType a) => Conditional (Automaton q a) where
    cond c (Automaton q1 a1 d1 i1 f1) (Automaton q2 a2 d2 i2 f2) =
        Automaton (cond c q1 q2) (cond c a1 a2) (cond c d1 d2) (cond c i1 i2) (cond c f1 f2)

instance (Contextual q, Contextual a, Ord q, Ord a) => Contextual (Automaton q a) where
    when ctx (Automaton q a d i f) = Automaton (when ctx q) (when ctx a) (when ctx d) (when ctx i) (when ctx f)

instance (NominalType q, NominalType a) => NominalType (Automaton q a) where
    eq (Automaton q1 a1 d1 i1 f1) (Automaton q2 a2 d2 i2 f2) = eq q1 q2 /\ eq a1 a2 /\ eq d1 d2 /\ eq i1 i2 /\ eq f1 f2
    mapVariables fun (Automaton q a d i f) = Automaton (mapVariables fun q) (mapVariables fun a) (mapVariables fun d)
                                                       (mapVariables fun i) (mapVariables fun f)
    foldVariables fun acc (Automaton q a d i f) =
        foldVariables fun (foldVariables fun (foldVariables fun (foldVariables fun (foldVariables fun acc q) a) d) i) f

----------------------------------------------------------------------------------------------------
-- Automaton functions
----------------------------------------------------------------------------------------------------

transitFromStates :: (NominalType q, NominalType a) => Automaton q a -> (Maybe q -> Formula) -> a -> Set (Maybe q)
transitFromStates aut cf l = mapFilter (\(s1, l', s2) -> maybeIf (cf s1 /\ eq l l') s2) (delta aut)

transit :: (NominalType q, NominalType a) => Automaton q a -> Maybe q -> a -> Set (Maybe q)
transit aut s = transitFromStates aut (eq s)

transitSet :: (NominalType q, NominalType a) => Automaton q a -> Set (Maybe q) -> a -> Set (Maybe q)
transitSet aut ss = transitFromStates aut (contains ss)

-- | Checks whether an automaton accepts a word.
accepts :: (NominalType q, NominalType a) => Automaton q a -> [a] -> Formula
accepts aut = intersect (finalStates aut) . foldl (transitSet aut) (initialStates aut)

transitionGraph :: (NominalType q, NominalType a) => Automaton q a -> Graph (Maybe q)
transitionGraph aut = graph (states aut) (map (\(s1, _, s2) -> (s1, s2)) $ delta aut)

onlyReachable :: (NominalType q, NominalType a) => Automaton q a -> Automaton q a
onlyReachable aut@(Automaton q a d i f) = Automaton q' a d' i (intersection q' f)
    where q' = reachableFromSet (transitionGraph aut) (initialStates aut)
          d' = mapFilter (\(s1,l,s2) -> maybeIf (contains q' s1) (s1,l,s2)) d

-- | Checks whether an automaton accepts any word.
isNotEmptyAutomaton :: (NominalType q, NominalType a) => Automaton q a -> Formula
isNotEmptyAutomaton = isNotEmpty . finalStates . onlyReachable

-- | Checks whether an automaton rejects all words.
isEmptyAutomaton :: (NominalType q, NominalType a) => Automaton q a -> Formula
isEmptyAutomaton = not . isNotEmptyAutomaton

pairsDelta :: (NominalType q1, NominalType q2, NominalType a) => Set (q1,a,q1) -> Set (q2,a,q2) -> Set ((q1,q2),a,(q1,q2))
pairsDelta d1 d2 = pairsWithFilter (\(s1,l,s2) (s1',l',s2') -> maybeIf (eq l l') ((s1,s1'),l,(s2,s2'))) d1 d2

-- | Returns an automaton that accepts the union of languages accepted by two automata.
unionAutomaton :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a -> Automaton (Either q1 q2) a
unionAutomaton (Automaton q1 a d1 i1 f1) (Automaton q2 _ d2 i2 f2) = (Automaton q a d i f)
    where eitherUnion s1 s2 = union (map (fmap Left) s1) (map (fmap Right) s2)
          q = eitherUnion q1 q2
          d = union (map (\(s1,l,s2)->(fmap Left s1,l,fmap Left s2)) d1) (map (\(s1,l,s2)->(fmap Right s1,l,fmap Right s2)) d2)
          i = eitherUnion i1 i2
          f = eitherUnion f1 f2

-- | Returns an automaton that accepts the intersection of languages accepted by two automata.
intersectionAutomaton :: (NominalType q1, NominalType q2, NominalType a) => Automaton q1 a -> Automaton q2 a -> Automaton (q1, q2) a
intersectionAutomaton (Automaton q1 a d1 i1 f1) (Automaton q2 _ d2 i2 f2) = (Automaton q a d i f)
    where intersectionPair x1 x2 = case (x1,x2) of (Just y1,Just y2) -> Just (y1,y2)
                                                   otherwise         -> Nothing
          q = pairsWith intersectionPair q1 q2
          d = pairsWithFilter (\(s1,l,s2) (s1',l',s2') -> maybeIf (eq l l') (intersectionPair s1 s1',l,intersectionPair s2 s2')) d1 d2
          i = pairsWith intersectionPair i1 i2
          f = pairsWith intersectionPair f1 f2

----------------------------------------------------------------------------------------------------
-- Automaton minimization
----------------------------------------------------------------------------------------------------

-- | Returns a minimal automaton accepting the same language as a given automaton.
minimize :: (NominalType q, NominalType a) => Automaton q a -> Automaton (Set (Maybe q)) a
minimize aut@(Automaton q a d i f) = automaton q' a d' i' f'
    where relGraph = graph (square q) (map (\(s1,_,s2) -> (s1,s2)) $ pairsDelta d d)
          nf = q \\ f
          equiv = square q \\ reachableFromSet (reverseEdges relGraph) (union (pairs nf f) (pairs f nf))
          q' = map vertices (stronglyConnectedComponents $ graph q equiv)
          d' = empty -- TODO
          i' = filter (intersect i) q'
          f' = filter (intersect f) q'
