module Nominal.Set where

import Control.Arrow ((***))
import Data.Char (isDigit)
import Data.List.Utils (join)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Formula
import Nominal.Conditional
import Nominal.Type
import Nominal.Variants (Atom, Variants)
import qualified Nominal.Variants as V
import Prelude hiding (or, and, not, sum, map, filter)
import qualified Prelude as Prelude (filter, not)

----------------------------------------------------------------------------------------------------
-- Nominal Set
----------------------------------------------------------------------------------------------------

data Set a = Set {variablesNumber :: Int, mapFunction :: ([Variable] -> Map a Formula), setLevel :: Int}

incSetLevel :: NominalType a => Int -> a -> a
incSetLevel l = countSetLevel (succ l)

getIterVarsPrefix :: Int -> Char
getIterVarsPrefix = toEnum . (+ 97)

createIterVars :: Int -> Int -> [Variable]
createIterVars setLevel varsNumber = fmap (Variable . (getIterVarsPrefix setLevel :) . show) [1..varsNumber]

getIterVars :: NominalType a => Int -> a -> Set.Set Variable
getIterVars setLevel x = let varPrefix = (getIterVarsPrefix setLevel)
                         in Set.filter
                              ((\v -> (head v) == varPrefix && (any isDigit $ tail v)) . variableName)
                              (support x)

getElementsMap :: NominalType a => Set a -> Map a Formula
getElementsMap (Set vn mf l) = --Map.mapKeys (countSetLevel $ succ l)
                                (mf $ createIterVars l vn)

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance NominalType a => Eq (Set a) where
    s1 == s2 = getElementsMap s1 == getElementsMap s2

instance NominalType a => Ord (Set a) where
    compare s1 s2 = compare (getElementsMap s1) (getElementsMap s2)

instance (NominalType a, Show a) => Show (Set a) where
    show s = "{" ++ join ", " (fmap showElement (Map.assocs $ getElementsMap s)) ++ "}"
      where showElement (v,c) = let iterVars = getIterVars (setLevel s) v
                                    formula = Set.foldr (∃) c (getIterVars (setLevel s) c Set.\\ iterVars)
                                    variables = if Set.null iterVars
                                                  then ""
                                                  else " for " ++ (join "," (fmap show $ Set.elems iterVars)) ++ " ∊ 𝔸"
                                    condition = (if formula == T then "" else " " ++ show formula) ++ variables
                                in show v ++ (if null condition then "" else " :" ++ condition)

instance NominalType a => NominalType (Set a) where
    eq s1 s2 = (isSubsetOf s1 s2) /\ (isSubsetOf s2 s1)
    support s@(Set vn mf l) = let iterVars = Set.fromList (createIterVars l vn)
                              in Set.unions $ fmap
                                                (\(v, c) -> (support c) `Set.union` (support v) Set.\\ iterVars)
                                                (Map.assocs $ getElementsMap s)
    countSetLevel level (Set vn mf _) = Set vn mf level

----------------------------------------------------------------------------------------------------
-- Operations on set
----------------------------------------------------------------------------------------------------

empty :: Set a
empty = Set 0 (const Map.empty) 0

isNotEmpty :: NominalType a => Set a -> Formula
isNotEmpty s = or $ fmap (\c -> Set.foldr (∃) c $ getIterVars (setLevel s) c) (Map.elems $ getElementsMap s)

-- TODO variants
insert :: NominalType a => a -> Set a -> Set a
insert e (Set vn mf l) = Set vn ((Map.insertWith (/\) (incSetLevel l e) T) . mf) l

delete :: NominalType a => a -> Set a -> Set a
delete e (Set vn mf l) = filter (not . (eq e)) (Set vn ((Map.delete e) . mf) l)

-- TODO variants
map :: NominalType b => (a -> b) -> Set a -> Set b
map f (Set vn mf l) = Set vn ((Map.mapKeysWith (/\) (incSetLevel l . f)) . mf) l

filter :: (a -> Formula) -> Set a -> Set a
filter f (Set vn mf l) = Set vn ((Map.mapWithKey (\v c -> c /\ f v)) . mf) l

sum :: NominalType a => Set (Set a) -> Set a
sum s@(Set vn mf l) = let maxVn = (maximum . (fmap variablesNumber) . Map.keys . getElementsMap) s
                          mapFun vs = Map.unionsWith (\/) (fmap
                                                            (\(Set _ mf' _, c) -> Map.map (/\ c) (mf' $ drop vn vs))
                                                            (Map.assocs $ mf (take vn vs)))
                      in Set (vn + maxVn) mapFun l

atomSet :: Set Atom
atomSet = Set 1 (\[v] -> Map.singleton (V.variant v) T) 0

----------------------------------------------------------------------------------------------------
-- Additional functions
----------------------------------------------------------------------------------------------------

isEmpty :: NominalType a => Set a -> Formula
isEmpty = not . isNotEmpty

just :: NominalType a => a -> Set a
just e = insert e empty

insertAll :: NominalType a => [a] -> Set a -> Set a
insertAll es s = foldl (flip insert) s es

fromList :: NominalType a => [a] -> Set a
fromList es = insertAll es empty

deleteAll :: NominalType a => [a] -> Set a -> Set a
deleteAll es s = foldl (flip delete) s es

exists :: NominalType a => (a -> Formula) -> Set a -> Formula
exists f = isNotEmpty . (filter f)

forall :: NominalType a => (a -> Formula) -> Set a -> Formula
forall f = isEmpty . (filter $ \x -> not (f x))

union :: NominalType a => Set a -> Set a -> Set a
union s1 s2 = sum (insert s1 (just s2 ))

unions :: NominalType a => [Set a] -> Set a
unions = foldl union empty

contains :: NominalType a => Set a -> a -> Formula
contains s e = exists (eq e) s

notContains :: NominalType a => Set a -> a -> Formula
notContains s = not . contains s

isSubsetOf :: NominalType a => Set a -> Set a -> Formula
isSubsetOf s1 s2 = forall (contains s2) s1

isNotSubsetOf :: NominalType a => Set a -> Set a -> Formula
isNotSubsetOf s = not . isSubsetOf s

isProperSubsetOf :: NominalType a => Set a -> Set a -> Formula
isProperSubsetOf s1 s2 = (isSubsetOf s1 s2) /\ (isNotSubsetOf s2 s1)

isNotProperSubsetOf :: NominalType a => Set a -> Set a -> Formula
isNotProperSubsetOf s = not . isProperSubsetOf s

intersection :: NominalType a => Set a -> Set a -> Set a
intersection s1 s2 = filter (contains s1) s2

difference :: NominalType a => Set a -> Set a -> Set a
difference s1 s2 = filter (notContains s2) s1

infixl 9 \\
(\\) :: NominalType a => Set a -> Set a -> Set a
s1 \\ s2 = difference s1 s2

pairs :: (NominalType a, NominalType b) => Set a -> Set b -> Set (a, b)
pairs s1 s2 = sum $ map (\e1 -> map (\e2 -> (e1, e2)) s2) s1

triples :: (NominalType a, NominalType b, NominalType c) => Set a -> Set b -> Set c -> Set (a, b, c)
triples s1 s2 s3 = sum $ map (\(e1, e2) -> map (\e3 -> (e1, e2, e3)) s3) (pairs s1 s2)
