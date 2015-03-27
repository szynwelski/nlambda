module Nominal.Graph where

import Nominal.Conditional
import Nominal.Formula
import Nominal.Maybe
import Nominal.Set
import Nominal.Type
import Nominal.Variants hiding (filter, map)
import Prelude hiding (filter, map)

----------------------------------------------------------------------------------------------------
-- Graph
----------------------------------------------------------------------------------------------------

data Graph a = Graph {vertices :: Set a, edges :: Set (a, a)} deriving (Eq, Ord, Show)

instance NominalType a => NominalType (Graph a) where
    eq (Graph vs1 es1) (Graph vs2 es2) = eq vs1 vs2 /\ eq es1 es2
    mapVariables f (Graph vs es) = Graph (mapVariables f vs) (mapVariables f es)
    foldVariables f acc (Graph vs es) = foldVariables f (foldVariables f acc vs) es
    simplify (Graph vs es) = Graph (simplify vs) (simplify es)


graph :: Set a -> Set (a,a) -> Graph a
graph = Graph

emptyGraph :: Graph a
emptyGraph = graph empty empty

atomsGraph :: Set (Atom, Atom) -> Graph Atom
atomsGraph = graph atoms

emptyAtomsGraph :: Graph Atom
emptyAtomsGraph = atomsGraph empty

atomsClique :: Graph Atom
atomsClique = atomsGraph atomsPairs

monotonicGraph :: Graph Atom
monotonicGraph = atomsGraph $ filter (uncurry lt) atomsPairs

----------------------------------------------------------------------------------------------------
-- Graph algorithms
----------------------------------------------------------------------------------------------------

edgesTransitiveClosure :: NominalType a => Set (a, a) -> Set (a, a)
edgesTransitiveClosure es = let es' = mapFilter (\((a, b), (c, d)) -> iF (eq b c) (just (a, d)) nothing) $ pairs es es
                            in ite (eq es es') es (edgesTransitiveClosure es')

transitiveClosure :: NominalType a => Graph a -> Graph a
transitiveClosure (Graph vs es) = graph vs (edgesTransitiveClosure es)

existsEdge :: NominalType a => Graph a -> a -> a -> Formula
existsEdge (Graph vs es) v1 v2 = contains es (v1, v2)

existsPath :: NominalType a => Graph a -> a -> a -> Formula
existsPath g v1 v2 = existsEdge (transitiveClosure g) v1 v2

