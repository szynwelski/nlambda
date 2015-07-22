module Nominal.Graph where

import Data.Tuple (swap)
import Nominal.Atom
import Nominal.Conditional
import Nominal.Formula
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Nominal.Variants hiding (filter, map)
import Prelude hiding (filter, map, not, sum)

----------------------------------------------------------------------------------------------------
-- Graph
----------------------------------------------------------------------------------------------------

data Graph a = Graph {vertices :: Set a, edges :: Set (a, a)} deriving (Eq, Ord, Show)

instance NominalType a => Conditional (Graph a) where
    ite c (Graph vs1 es1) (Graph vs2 es2) = Graph (ite c vs1 vs2) (ite c es1 es2)

instance NominalType a => NominalType (Graph a) where
    eq (Graph vs1 es1) (Graph vs2 es2) = eq vs1 vs2 /\ eq es1 es2
    mapVariables f (Graph vs es) = Graph (mapVariables f vs) (mapVariables f es)
    foldVariables f acc (Graph vs es) = foldVariables f (foldVariables f acc vs) es
    simplify (Graph vs es) = Graph (simplify vs) (simplify es)

----------------------------------------------------------------------------------------------------
-- Forest
----------------------------------------------------------------------------------------------------

type Forest a = Set (Graph a)

----------------------------------------------------------------------------------------------------
-- Graph constructors
----------------------------------------------------------------------------------------------------

graph :: Set a -> Set (a,a) -> Graph a
graph = Graph

emptyGraph :: Graph a
emptyGraph = graph empty empty

atomsGraph :: Set (Atom, Atom) -> Graph Atom
atomsGraph = graph atoms

emptyAtomsGraph :: Graph Atom
emptyAtomsGraph = atomsGraph empty

clique :: NominalType a => Set a -> Graph a
clique vs = graph vs (squared vs)

atomsClique :: Graph Atom
atomsClique = atomsGraph atomsPairs

simpleClique :: NominalType a => Set a -> Graph a
simpleClique = removeLoops . clique

simpleAtomsClique :: Graph Atom
simpleAtomsClique = removeLoops atomsClique

monotonicGraph :: Graph Atom
monotonicGraph = atomsGraph $ filter (uncurry lt) atomsPairs

----------------------------------------------------------------------------------------------------
-- Graph operations
----------------------------------------------------------------------------------------------------

addVertex :: NominalType a => a -> Graph a -> Graph a
addVertex v (Graph vs es) = Graph (insert v vs) es

removeVertex :: NominalType a => a -> Graph a -> Graph a
removeVertex v (Graph vs es) = Graph (delete v vs) es

addEdge :: NominalType a => (a, a) -> Graph a -> Graph a
addEdge (v1, v2) (Graph vs es) = Graph (insert v1 $ insert v2 vs) (insert (v1, v2) es)

removeEdge :: NominalType a => (a, a) -> Graph a -> Graph a
removeEdge (v1, v2) (Graph vs es) = Graph vs (delete (v1, v2) es)

addLoops :: NominalType a => Graph a -> Graph a
addLoops (Graph vs es) = Graph vs $ union es (map (\v -> (v,v)) vs)

removeLoops :: NominalType a => Graph a -> Graph a
removeLoops (Graph vs es) = Graph vs $ filter (uncurry neq) es

reverseEdges :: NominalType a => Graph a -> Graph a
reverseEdges (Graph vs es) = Graph vs $ map swap es

undirected :: NominalType a => Graph a -> Graph a
undirected (Graph vs es) = Graph vs $ union es (map swap es)

subgraph :: NominalType a => Graph a -> Set a -> Graph a
subgraph (Graph vs es) vs' =
    Graph (vs' `intersection` vs)
          (mapFilter (\(v1, v2) -> when (contains vs' v1 /\ contains vs' v2) (v1, v2)) es)

----------------------------------------------------------------------------------------------------
-- Graph algorithms
----------------------------------------------------------------------------------------------------

hasLoop :: NominalType a => Graph a -> Formula
hasLoop = exists (uncurry eq) . edges

isSimple :: NominalType a => Graph a -> Formula
isSimple = not . hasLoop

containsEdge :: NominalType a => Graph a -> (a, a) -> Formula
containsEdge (Graph vs es) e = contains es e

preds :: NominalType a => Graph a -> a -> Set a
preds g v = mapFilter (\(a, b) -> when (eq b v) a) (edges g)

succs :: NominalType a => Graph a -> a -> Set a
succs g v = mapFilter (\(a, b) -> when (eq a v) b) (edges g)

neighbors :: NominalType a => Graph a -> a -> Set a
neighbors g v = union (succs g v) (preds g v)

transitiveClosure :: NominalType a => Graph a -> Graph a
transitiveClosure (Graph vs es) = Graph vs (edgesClosure es)
    where edgesClosure es = let es' = mapFilter (\((a, b), (c, d)) -> when (eq b c) (a, d)) $ squared es
                                es'' = simplify $ union es es'
                            in ite' (eq es es'') es (edgesClosure es'')

existsPath :: NominalType a => Graph a -> a -> a -> Formula
existsPath g v1 v2 = containsEdge (transitiveClosure g) (v1, v2)

isStronglyConnected :: NominalType a => Graph a -> Formula
isStronglyConnected g = eq (transitiveClosure g) (clique $ vertices g)

isWeaklyConnected :: NominalType a => Graph a -> Formula
isWeaklyConnected = isStronglyConnected . undirected

hasCycle :: NominalType a => Graph a -> Formula
hasCycle = hasLoop . transitiveClosure

reachable :: NominalType a => Graph a -> a -> Set a
reachable g v = insert v $ succs (transitiveClosure g) v

reachableFromSet :: NominalType a => Graph a -> Set a -> Set a
reachableFromSet g s = sum $ map (reachable g) s

weaklyConnectedComponent :: NominalType a => Graph a -> a -> Graph a
weaklyConnectedComponent g v = subgraph g $ union (reachable g v) (reachable (reverseEdges g) v)

weaklyConnectedComponents :: NominalType a => Graph a -> Forest a
weaklyConnectedComponents g = map (weaklyConnectedComponent g) (vertices g)

stronglyConnectedComponent :: NominalType a => Graph a -> a -> Graph a
stronglyConnectedComponent g v = subgraph g $ intersection (reachable g v) (reachable (reverseEdges g) v)

stronglyConnectedComponents :: NominalType a => Graph a -> Forest a
stronglyConnectedComponents g = map (stronglyConnectedComponent g) (vertices g)
