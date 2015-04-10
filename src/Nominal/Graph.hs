module Nominal.Graph where

import Data.Tuple (swap)
import Nominal.Conditional
import Nominal.Formula
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Nominal.Variants hiding (filter, map)
import Prelude hiding (filter, map, not)

----------------------------------------------------------------------------------------------------
-- Graph
----------------------------------------------------------------------------------------------------

data Graph a = Graph {vertices :: Set a, edges :: Set (a, a)} deriving (Eq, Ord, Show)

instance NominalType a => NominalType (Graph a) where
    eq (Graph vs1 es1) (Graph vs2 es2) = eq vs1 vs2 /\ eq es1 es2
    mapVariables f (Graph vs es) = Graph (mapVariables f vs) (mapVariables f es)
    foldVariables f acc (Graph vs es) = foldVariables f (foldVariables f acc vs) es
    simplify (Graph vs es) = Graph (simplify vs) (simplify es)


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

undirected :: NominalType a => Graph a => Graph a
undirected (Graph vs es) = Graph vs $ union es (map swap es)

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

----------------------------------------------------------------------------------------------------
-- Graph algorithms
----------------------------------------------------------------------------------------------------

hasLoop :: NominalType a => Graph a -> Formula
hasLoop = exists (uncurry eq) . edges

isSimple :: NominalType a => Graph a -> Formula
isSimple = not . hasLoop

containsEdge :: NominalType a => Graph a -> (a, a) -> Formula
containsEdge (Graph vs es) e = contains es e

preNeighbors :: NominalType a => Graph a -> a -> Set a
preNeighbors g v = mapFilter (\(a, b) -> iF (eq b v) (just a) nothing) (edges g)

succNeighbors :: NominalType a => Graph a -> a -> Set a
succNeighbors g v = mapFilter (\(a, b) -> iF (eq a v) (just b) nothing) (edges g)

neighbors :: NominalType a => Graph a -> a -> Set a
neighbors g v = union (succNeighbors g v) (preNeighbors g v)

edgesTransitiveClosure :: NominalType a => Set (a, a) -> Set (a, a)
edgesTransitiveClosure es = let es' = mapFilter (\((a, b), (c, d)) -> iF (eq b c) (just (a, d)) nothing) $ squared es
                                es'' = union es es'
                            in ite (eq es es'') es (edgesTransitiveClosure es'')

transitiveClosure :: NominalType a => Graph a -> Graph a
transitiveClosure (Graph vs es) = graph vs (edgesTransitiveClosure es)

existsPath :: NominalType a => Graph a -> a -> a -> Formula
existsPath g v1 v2 = containsEdge (transitiveClosure g) (v1, v2)

isConnected :: NominalType a => Graph a -> Formula
isConnected g = eq (transitiveClosure g) (clique $ vertices g)
