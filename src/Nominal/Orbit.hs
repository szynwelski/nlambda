module Nominal.Orbit where

import Data.List (elemIndex, delete)
import Data.Set (elems, empty, insert)
import Nominal.Atom (Atom)
import Nominal.Atoms.Type (relations)
import Nominal.Formula (Formula, (/\), (<==>), and, fromBool, isTrue)
import Nominal.Set (Set, filter, fromList, isSingleton, map, replicateAtoms, size)
import Nominal.Type (NominalType, Scope(..), eq, foldVariables, mapVariables)
import Nominal.Variants (Variants, fromVariant, variant, variantsRelation)
import Prelude hiding (and, filter, map)

----------------------------------------------------------------------------------------------------
-- Support
----------------------------------------------------------------------------------------------------

support :: NominalType a => a -> [Atom]
support = fmap variant . elems . foldVariables (Free, insert) empty

leastSupport :: NominalType a => a -> [Atom]
leastSupport e = go e (support e) (support e)
    where go e supp [] = supp
          go e supp as = let lessSupp = delete (head as) supp
                         in if isTrue $ supports lessSupp e then go e lessSupp lessSupp else go e supp (tail as)

leastSupportSize :: NominalType a => a -> Variants Int
leastSupportSize = size . fromList . leastSupport

supports :: NominalType a => [Atom] -> a -> Formula
supports supp = isSingleton . orbit supp

isEquivariant :: NominalType a => a -> Formula
isEquivariant = fromBool . null . leastSupport

----------------------------------------------------------------------------------------------------
-- Orbits
----------------------------------------------------------------------------------------------------

groupAction :: NominalType a => (Atom -> Atom) -> a -> a
groupAction action = mapVariables (Free, fromVariant . action . variant)

orbit :: NominalType a => [Atom] -> a -> Set a
orbit supp elem = map mapFun $ filter filterFun $ replicateAtoms elSuppSize
  where elSupp = support elem
        elSuppSize = length elSupp
        mapFun list = groupAction (\x -> maybe x (list !!) (elemIndex x elSupp)) elem
        relFun list rel = and [rel (list!!pred i) (list!!pred j) <==> rel (elSupp!!pred i) (elSupp!!pred j) | i<-[1..elSuppSize], j<-[1..elSuppSize], i/=j]
                       /\ and [rel (list!!pred i) (supp!!pred j) <==> rel (elSupp!!pred i) (supp!!pred j) | i<-[1..elSuppSize], j<-[1..length supp]]
        filterFun list = and $ fmap (relFun list) $ fmap variantsRelation relations

setOrbit :: NominalType a => Set a -> a -> Set a
setOrbit s = orbit $ leastSupport s

setOrbits :: NominalType a => Set a -> Set (Set a)
setOrbits s = map (setOrbit s) s

setOrbitsNumber :: NominalType a => Set a -> Variants Int
setOrbitsNumber = size . setOrbits

inTheSameOrbit :: NominalType a => [Atom] -> a -> a -> Formula
inTheSameOrbit supp e1 e2 = eq (orbit supp e1) (orbit supp e2)

inTheSameSetOrbit :: NominalType a => Set a -> a -> a -> Formula
inTheSameSetOrbit s e1 e2 = eq (setOrbit s e1) (setOrbit s e2)
