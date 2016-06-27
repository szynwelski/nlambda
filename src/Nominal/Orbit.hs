module Nominal.Orbit where

import Data.List (elemIndex, delete)
import Data.Set (elems, empty, insert)
import Nominal.Atoms (Atom)
import Nominal.Atoms.Signature (minRelations)
import Nominal.Formula.Constructors (constraint)
import Nominal.Formula (Formula, (/\), (<==>), and, fromBool, isTrue)
import Nominal.Set (Set, filter, isSingleton, map, maxSize, replicateAtoms, replicateSetUntil, size, sum, unions)
import Nominal.Type (NominalType, Scope(..), eq, foldVariables, mapVariables)
import Nominal.Variants (Variants, fromVariant, variant, variantsRelation)
import Prelude hiding (and, filter, map, sum)

----------------------------------------------------------------------------------------------------
-- Support
----------------------------------------------------------------------------------------------------

-- | Returns all free atoms of an element. The result list is also support of an element, but not always the least one.
support :: NominalType a => a -> [Atom]
support = fmap variant . elems . foldVariables (Free, insert) empty

-- | Returns the least support of an element. From the result of 'support' function it removes atoms and check if it is still support.
leastSupport :: NominalType a => a -> [Atom]
leastSupport e = go e (support e) (support e)
    where go e supp [] = supp
          go e supp as = let lessSupp = delete (head as) supp
                         in if isTrue $ supports lessSupp e then go e lessSupp lessSupp else go e supp (tail as)

-- | Checks whether a list of atoms supports an element.
supports :: NominalType a => [Atom] -> a -> Formula
supports supp = isSingleton . orbit supp

-- | Checks whether an element is equivariant, i.e. it has empty support.
isEquivariant :: NominalType a => a -> Formula
isEquivariant = fromBool . null . leastSupport

----------------------------------------------------------------------------------------------------
-- Orbits
----------------------------------------------------------------------------------------------------

-- | Applies permutations of atoms to all atoms in an element.
groupAction :: NominalType a => (Atom -> Atom) -> a -> a
groupAction action = mapVariables (Free, fromVariant . action . variant)

-- | Returns an orbit of an element with a given support.
orbit :: NominalType a => [Atom] -> a -> Set a
orbit supp elem = map mapFun $ filter filterFun $ replicateAtoms elSuppSize
  where elSupp = support elem
        elSuppSize = length elSupp
        mapFun list = groupAction (\x -> maybe x (list !!) (elemIndex x elSupp)) elem
        relFun list rel = and [rel (list!!pred i) (list!!pred j) <==> rel (elSupp!!pred i) (elSupp!!pred j) | i<-[1..elSuppSize], j<-[1..elSuppSize], i/=j]
                       /\ and [rel (list!!pred i) (supp!!pred j) <==> rel (elSupp!!pred i) (supp!!pred j) | i<-[1..elSuppSize], j<-[1..length supp]]
        filterFun list = and $ fmap (relFun list) $ fmap (variantsRelation . constraint) minRelations

multiorbit :: NominalType a => a -> Set a
multiorbit elem = map mapFun $ replicateAtoms $ length elSupp
  where elSupp = support elem
        mapFun list = groupAction (\x -> maybe x (list !!) (elemIndex x elSupp)) elem

-- | For a given list of atoms and a set returns the closure of the set under all automorphisms of atoms that fix every element of the list.
hull :: NominalType a => [Atom] -> Set a -> Set a
hull supp = sum . map (orbit supp)

-- | Returns an orbit of an element in a set.
setOrbit :: NominalType a => Set a -> a -> Set a
setOrbit s = orbit $ leastSupport s

-- | Returns all orbits of a set.
setOrbits :: NominalType a => Set a -> Set (Set a)
setOrbits s = map (setOrbit s) s

-- | Returns a number of orbits of a set.
-- It uses 'size' function which is inefficient for large sets and will not return the answer for the infinite sets.
setOrbitsNumber :: NominalType a => Set a -> Variants Int
setOrbitsNumber = size . setOrbits

-- | Checks whether two elements are in the same orbit with a given support.
inTheSameOrbit :: NominalType a => [Atom] -> a -> a -> Formula
inTheSameOrbit supp e1 e2 = eq (orbit supp e1) (orbit supp e2)

-- | Checks whether two elements are in the same orbit of a set.
inTheSameSetOrbit :: NominalType a => Set a -> a -> a -> Formula
inTheSameSetOrbit s e1 e2 = eq (setOrbit s e1) (setOrbit s e2)

-- | Returns a set of all equivariant subsets of a given set
equivariantSubsets :: NominalType a => Set a -> Set (Set a)
equivariantSubsets s = map (unions . fmap (setOrbit s)) $ replicateSetUntil (maxSize $ setOrbits s) s

