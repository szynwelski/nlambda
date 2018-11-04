{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}

module Nominal.Orbit where

import Data.List (elemIndex, delete)
import Data.Set (elems, insert)
import Nominal.Atoms (Atom)
import Nominal.Atoms.Signature (minRelations)
import Nominal.Conditional (ite)
import Nominal.Contextual (Contextual)
import Nominal.Formula.Definition (constraint)
import Nominal.Formula (Formula, (/\), (<==>), and, fromBool, isTrue)
import Nominal.Maybe (NominalMaybe)
import Nominal.Meta (WithMeta)
import Nominal.Set (Set, element, empty, filter, intersect, isSingleton, map, maxSizeWith, member, replicateAtoms, replicateSetUntil, sizeWith, sum, unions)
import Nominal.Type (NominalType, NLambda_NominalType, eq)
import Nominal.Variable (Scope(..), freeVariables, mapVariables)
import Nominal.Variants (Variants, fromVariant, variant, variantsRelation)
import Prelude hiding (and, filter, map, sum)

----------------------------------------------------------------------------------------------------
-- Support
----------------------------------------------------------------------------------------------------

-- | Returns all free atoms of an element. The result list is also support of an element, but not always the least one.
support :: NominalType a => a -> [Atom]
support = fmap variant . elems . freeVariables

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

nlambda_groupAction :: NLambda_NominalType a => (WithMeta Atom -> WithMeta Atom) -> WithMeta a -> WithMeta a
nlambda_groupAction = undefined

-- | Returns an orbit of an element with a given support.
orbit :: NominalType a => [Atom] -> a -> Set a
orbit supp elem = map mapFun $ filter filterFun $ replicateAtoms elSuppSize
  where elSupp = support elem
        elSuppSize = length elSupp
        mapFun list = groupAction (\x -> maybe x (list !!) (elemIndex x elSupp)) elem
        relFun list rel = and [rel (list!!i) (list!!j) <==> rel (elSupp!!i) (elSupp!!j) | i<-[0..elSuppSize-1], j<-[0..elSuppSize-1], i<j]
                       /\ and [rel (list!!i) (supp!!j) <==> rel (elSupp!!i) (supp!!j) | i<-[0..elSuppSize-1], j<-[0..length supp - 1]]
        filterFun list = and $ fmap (relFun list . variantsRelation . constraint) minRelations

multiorbit :: NominalType a => a -> Set a
multiorbit elem = map mapFun $ replicateAtoms $ length elSupp
  where elSupp = support elem
        mapFun list = groupAction (\x -> maybe x (list !!) (elemIndex x elSupp)) elem

-- | For a given list of atoms and a set returns the closure of the set under all automorphisms of atoms that fix every element of the list.
hull :: NominalType a => [Atom] -> Set a -> Set a
hull supp = sum . map (orbit supp)

-- | Returns an orbit of an element in a set.
setOrbit :: NominalType a => Set a -> a -> Set a
setOrbit s e = ite (member e s) (orbit (leastSupport s) e) empty

-- | Returns all orbits of a set.
setOrbits :: NominalType a => Set a -> Set (Set a)
setOrbits s = map (setOrbit s) s

-- | Returns set of elements for all orbits of a set.
setOrbitsRepresentatives :: (Contextual a, NominalType a) => Set a -> Set (NominalMaybe a)
setOrbitsRepresentatives = map element . setOrbits

-- | Returns a number of orbits of a set.
-- It uses 'size' function which is inefficient for large sets and will not return the answer for the infinite sets.
setOrbitsNumber :: (Contextual a, NominalType a) => Set a -> Variants Int
setOrbitsNumber = sizeWith intersect . setOrbits

-- | Returns a maximum number of orbits of a set.
-- It uses 'maxSize' function which is inefficient for large sets and will not return the answer for the infinite sets.
setOrbitsMaxNumber :: (Contextual a, NominalType a) => Set a -> Int
setOrbitsMaxNumber = maxSizeWith intersect . setOrbits

-- | Checks whether two elements are in the same orbit with a given support.
inTheSameOrbit :: NominalType a => [Atom] -> a -> a -> Formula
inTheSameOrbit supp e1 e2 = eq (orbit supp e1) (orbit supp e2)

-- | Checks whether two elements are in the same orbit of a set.
inTheSameSetOrbit :: NominalType a => Set a -> a -> a -> Formula
inTheSameSetOrbit s e1 e2 = eq (setOrbit s e1) (setOrbit s e2)

-- | Returns a set of all equivariant subsets of a given set
equivariantSubsets :: (Contextual a, NominalType a) => Set a -> Set (Set a)
equivariantSubsets s = map (unions . fmap (setOrbit s)) $ replicateSetUntil (maxSizeWith intersect $ setOrbits s) s

