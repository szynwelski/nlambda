{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Nominal.Graph where

import Data.Tuple (swap)
import Nominal.Atoms
import Nominal.Conditional
import Nominal.Contextual
import Nominal.Formula
import Nominal.Maybe
import Nominal.Orbit
import Nominal.Set
import Nominal.Type
import Nominal.Variants hiding (fromList, map)
import Prelude hiding (filter, map, not, or, sum)
import GHC.Generics (Generic)

----------------------------------------------------------------------------------------------------
-- Graph
----------------------------------------------------------------------------------------------------

-- | A directed graph with vertices of type __a__ and set of pairs representing edges.
data Graph a = Graph {vertices :: Set a, edges :: Set (a,a)}
  deriving (Eq, Ord, Show, Read, Generic, BareNominalType, Conditional, Contextual)

----------------------------------------------------------------------------------------------------
-- Graph constructors
----------------------------------------------------------------------------------------------------

-- | A graph constructor.
graph :: Set a -> Set (a,a) -> Graph a
graph = Graph

-- | An empty graph.
emptyGraph :: Graph a
emptyGraph = graph empty empty

-- | A graph with atoms vertices.
atomsGraph :: Set (Atom, Atom) -> Graph Atom
atomsGraph = graph atoms

-- | An empty graph with an 'Atom' type of vertices.
emptyAtomsGraph :: Graph Atom
emptyAtomsGraph = atomsGraph empty

-- | A graph with complete set of edges. Every pair of vertices are connected.
clique :: NominalType a => Set a -> Graph a
clique vs = graph vs (square vs)

-- | A complete graph with atoms vertices.
atomsClique :: Graph Atom
atomsClique = atomsGraph atomsPairs

-- | A clique without loops.
simpleClique :: NominalType a => Set a -> Graph a
simpleClique = removeLoops . clique

-- | A clique with atoms vertices and without loops.
simpleAtomsClique :: Graph Atom
simpleAtomsClique = removeLoops atomsClique

-- | A group with atoms vertices where two atoms are connected if the first one is smaller than the second one.
monotonicGraph :: Graph Atom
monotonicGraph = atomsGraph $ filter (uncurry lt) atomsPairs

----------------------------------------------------------------------------------------------------
-- Graph operations
----------------------------------------------------------------------------------------------------

-- | Adds vertex to a graph.
addVertex :: NominalType a => a -> Graph a -> Graph a
addVertex v (Graph vs es) = Graph (insert v vs) es

-- | Removes vertex from a graph.
removeVertex :: NominalType a => a -> Graph a -> Graph a
removeVertex v (Graph vs es) = Graph (delete v vs) es

-- | Adds an edge to a graph
addEdge :: NominalType a => (a, a) -> Graph a -> Graph a
addEdge (v1, v2) (Graph vs es) = Graph (insert v1 $ insert v2 vs) (insert (v1, v2) es)

-- | Removes an edge from a graph
removeEdge :: NominalType a => (a, a) -> Graph a -> Graph a
removeEdge (v1, v2) (Graph vs es) = Graph vs (delete (v1, v2) es)

-- | Add loops for all vertices in a graph.
addLoops :: NominalType a => Graph a -> Graph a
addLoops (Graph vs es) = Graph vs $ union es (map (\v -> (v,v)) vs)

-- | Removes all loops from a graph.
removeLoops :: NominalType a => Graph a -> Graph a
removeLoops (Graph vs es) = Graph vs $ filter (uncurry neq) es

-- | Reverses all edges in a graph.
reverseEdges :: NominalType a => Graph a -> Graph a
reverseEdges (Graph vs es) = Graph vs $ map swap es

-- | Produces a set of edges containing edges that are obtained by composition of edges in a given set.
composeEdges :: NominalType a => Set (a,a) -> Set (a,a) -> Set (a,a)
composeEdges = pairsWithFilter (\(a, b) (c, d) -> maybeIf (eq b c) (a, d))

-- | Produces a graph with edges that are obtained by composition of edges in a given graph.
compose :: NominalType a => Graph a -> Graph a -> Graph a
compose (Graph vs1 es1) (Graph vs2 es2) = Graph (vs1 `union` vs2) (composeEdges es1 es2)

-- | Adds all reverse edges to existing edges in a graph.
undirected :: NominalType a => Graph a -> Graph a
undirected (Graph vs es) = Graph vs $ union es (map swap es)

-- | Returns a subgraph limited to a set of vertices.
subgraph :: NominalType a => Graph a -> Set a -> Graph a
subgraph (Graph vs es) vs' =
    Graph (vs' `intersection` vs)
          (mapFilter (\(v1, v2) -> maybeIf (contains vs' v1 /\ contains vs' v2) (v1, v2)) es)

----------------------------------------------------------------------------------------------------
-- Graph algorithms
----------------------------------------------------------------------------------------------------

-- | Checks whether a graph has a loop.
hasLoop :: NominalType a => Graph a -> Formula
hasLoop = exists (uncurry eq) . edges

-- | Checks whether a graph does not have a loop
isSimple :: NominalType a => Graph a -> Formula
isSimple = not . hasLoop

-- | Checks whether a graph has a given edge.
containsEdge :: NominalType a => Graph a -> (a, a) -> Formula
containsEdge (Graph vs es) = contains es

predsFunction :: NominalType a => Graph a -> (a -> Formula) -> Set a
predsFunction g cf = mapFilter (\(a, b) -> maybeIf (cf b) a) (edges g)

-- | Returns all predecessors of a vertex.
preds :: NominalType a => Graph a -> a -> Set a
preds g v = predsFunction g (eq v)

predsFromSet :: NominalType a => Graph a -> Set a -> Set a
predsFromSet g s = predsFunction g (contains s)

succsFunction :: NominalType a => Graph a -> (a -> Formula) -> Set a
succsFunction g cf = mapFilter (\(a, b) -> maybeIf (cf a) b) (edges g)

-- | Returns all successors of a vertex.
succs :: NominalType a => Graph a -> a -> Set a
succs g v = succsFunction g (eq v)

succsFromSet :: NominalType a => Graph a -> Set a -> Set a
succsFromSet g s = succsFunction g (contains s)

-- | Returns all neighbours of an element in a graph.
neighbors :: NominalType a => Graph a -> a -> Set a
neighbors g v = succs g v `union` preds g v

-- | Returns a transitive closure of a graph.
transitiveClosure :: NominalType a => Graph a -> Graph a
transitiveClosure (Graph vs es) = Graph vs (edgesClosure es)
    where edgesClosure es = let es' = union es (composeEdges es es)
                            in ite (eq es es') es (edgesClosure es')

-- | Checks whether there is a path from one vertex to the second one in a graph.
existsPath :: NominalType a => Graph a -> a -> a -> Formula
existsPath g v1 v2 = contains (reachable g v1) v2 \/ contains (reachable g v2) v1

-- | Checks whether a graph is strongly connected.
isStronglyConnected :: NominalType a => Graph a -> Formula
isStronglyConnected g = eq (transitiveClosure g) (clique $ vertices g)

-- | Checks whether a graph is weakly connected.
isWeaklyConnected :: NominalType a => Graph a -> Formula
isWeaklyConnected = isStronglyConnected . undirected

-- | Checks whether a graph has a cycle.
hasCycle :: NominalType a => Graph a -> Formula
hasCycle = hasLoop . transitiveClosure

-- | Checks whether a graph has a even-length cycle.
hasEvenLengthCycle :: NominalType a => Graph a -> Formula
hasEvenLengthCycle g = hasCycle (compose g g)

-- | Checks whether a graph has a odd-length cycle.
hasOddLengthCycle :: NominalType a => Graph a -> Formula
hasOddLengthCycle g = edges (reverseEdges g) `intersect` edges (transitiveClosure $ compose g g)

-- | Checks whether a graph (treated as undirected) is bipartite in the sense that it does not have odd-length cycle
isBipartite :: NominalType a => Graph a -> Formula
isBipartite = not . hasOddLengthCycle . undirected

-- | Returns all vertices reachable from a vertex in a graph.
reachable :: NominalType a => Graph a -> a -> Set a
reachable g v = reachableFromSet g (singleton v)

-- | Returns all vertices reachable from a set of vertices in a graph.
reachableFromSet :: NominalType a => Graph a -> Set a -> Set a
reachableFromSet g s = let s' = union s $ succsFromSet g s
                       in ite (eq s s') s (reachableFromSet g s')

-- | Returns a weakly connected component of a graph containing a vertex.
weaklyConnectedComponent :: NominalType a => Graph a -> a -> Graph a
weaklyConnectedComponent g v = subgraph g $ union (reachable g v) (reachable (reverseEdges g) v)

-- | Returns all weakly connected components of a graph.
weaklyConnectedComponents :: NominalType a => Graph a -> Set (Graph a)
weaklyConnectedComponents g = map (weaklyConnectedComponent g) (vertices g)

-- | Returns a strongly connected component of a graph containing a vertex.
stronglyConnectedComponent :: NominalType a => Graph a -> a -> Graph a
stronglyConnectedComponent g v = subgraph g $ intersection (reachable g v) (reachable (reverseEdges g) v)

-- | Returns all strongly connected components of a graph.
stronglyConnectedComponents :: NominalType a => Graph a -> Set (Graph a)
stronglyConnectedComponents g = map (stronglyConnectedComponent g) (vertices g)

-- | Checks whether a given function is the proper coloring of a graph.
isColoringOf :: (NominalType a, NominalType b) => (a -> b) -> Graph a -> Formula
isColoringOf c g = forAll (\(v1,v2) -> c v1 `neq` c v2) (edges g)

-- | Checks whether a graph has an equivariant k-coloring.
hasEquivariantColoring :: (Contextual a, NominalType a) => Graph a -> Int -> Formula
hasEquivariantColoring g k = member true (pairsWith (\os ps -> coloring os ps `isColoringOf` g) (replicateSet n orbits) (partitions n k))
    where orbits = setOrbits (vertices g)
          n = maxSize orbits
          -- k-size partitions of n-size set (Int -> Int -> Set [Int])
          partitions n 1 = singleton (replicate n 0)
          partitions n k | k < 1 || n < k = empty
          partitions n k | n == k = singleton [0..n-1]
          partitions n k = map (k-1:) $ partitions (n-1) (k-1) `union` pairsWith (:) (fromList [0..k-1]) (partitions (n-1) k)
          -- for a given list of orbits and assigned numbers returns number assigned for the orbit containing element
          coloring [] [] _ = variant 0
          coloring (o:os) (p:ps) a = ite (member a o) (variant p) (coloring os ps a)
