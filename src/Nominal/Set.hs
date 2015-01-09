module Nominal.Set where

import Data.List.Utils (join)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Formula
import Nominal.Conditional
import Nominal.Type
import Nominal.Variants (Atom, Variants, atom, toList)
import Prelude hiding (or, and, not, sum, map, filter)

----------------------------------------------------------------------------------------------------
-- Set elements
----------------------------------------------------------------------------------------------------

type SetElementCondition = (Set.Set Variable, Formula)

getSetElementCondition :: SetElementCondition -> Formula
getSetElementCondition (vs, c) = Set.foldr (∃) c vs

checkVariables :: NominalType a => a -> SetElementCondition -> SetElementCondition
checkVariables v (vs, c) = (Set.intersection valueFreeVars vs,
                            getSetElementCondition (existentialCondVars, c))
    where valueFreeVars = support v
          existentialCondVars = Set.intersection (support c) (vs Set.\\ valueFreeVars)

mergeSetElements :: NominalType a => a -> SetElementCondition -> SetElementCondition -> SetElementCondition
mergeSetElements v (vs1, c1) (vs2, c2) = checkVariables v (Set.union vs1 vs2, c1 \/ c2)


filterNotFalse :: Map a SetElementCondition -> Map a SetElementCondition
filterNotFalse = Map.filter ((/= F) . snd)

filterCondition :: NominalType a => SetElementCondition
                                    -> Map a SetElementCondition -> Map a SetElementCondition
filterCondition (vs1, c1) = Map.mapWithKey mapFun
    where mapFun v (vs2, c2) = checkVariables v (Set.union vs1 vs2, c1 /\ c2)

unionSetElements :: NominalType a => Map a SetElementCondition
                             -> Map a SetElementCondition -> Map a SetElementCondition
unionSetElements = Map.unionWithKey mergeSetElements

----------------------------------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------------------------------

data Set a = Set {setElements :: Map a SetElementCondition} deriving (Eq, Ord)

instance Show a => Show (Set a) where
    show s = "{" ++ (join ", " (fmap showSetElement (Map.assocs $ setElements s))) ++ "}"
      where showSetElement (v, (vs, c)) =
              let formula = if c == T then "" else " " ++ show c
                  variables = if Set.null vs
                                then ""
                                else " for " ++ (join "," (fmap show $ Set.elems vs)) ++ " ∊ 𝔸"
                  condition = formula ++ variables
              in show v ++ (if null condition then "" else " :" ++ condition)

instance NominalType a => NominalType (Set a) where
    eq s1 s2 = (isSubsetOf s1 s2) /\ (isSubsetOf s2 s1)
    support (Set es) = Set.unions $ fmap (\(v, (vs, _)) -> (support v) Set.\\ vs)
                                               (Map.assocs es)

instance NominalType a => Conditional (Set a) where
    iF f (Set es1) (Set es2) = Set $ filterNotFalse $ unionSetElements
                                       (filterCondition (Set.empty, f) es1)
                                       (filterCondition (Set.empty, not f) es2)

----------------------------------------------------------------------------------------------------
-- Operations on set
----------------------------------------------------------------------------------------------------

empty :: Set a
empty = Set Map.empty

isEmpty :: Set a -> Formula
isEmpty s = and (fmap (not . getSetElementCondition) (Map.elems $ setElements s))

insert :: NominalType a => a -> Set a -> Set a
insert e (Set es) = Set $ foldl insertVariant es vs
    where vs = toList $ variants e
          insertVariant s (v, c) = Map.insertWithKey mergeSetElements v (Set.empty, c) s

delete :: NominalType a => a -> Set a -> Set a
delete e = (filter (not . (eq e))) . Set . (Map.delete e) . setElements

map :: NominalType b => (a -> b) -> Set a -> Set b
map f = Set . Map.fromListWithKey mergeSetElements
            . Map.foldrWithKey (\v (vs, c) es -> (mapResults f v vs c) ++ es) []
            . setElements
    where mapResults f v vs c = fmap (\(v', c') -> (v', (Set.empty, c' /\ c))) (toList $ variants (f v))

sum :: NominalType a => Set (Set a) -> Set a
sum = Set . filterNotFalse . (foldr unionSetElements Map.empty) . (fmap filterSet) . Map.assocs . setElements
      where filterSet (elemSet, elemSetCond) = filterCondition elemSetCond (setElements elemSet)

atomsSet :: String -> Set Atom
atomsSet name = Set $ Map.singleton (atom name) (Set.singleton $ Variable name, T)

----------------------------------------------------------------------------------------------------
-- Additional functions
----------------------------------------------------------------------------------------------------

isNotEmpty :: Set a -> Formula
isNotEmpty = not . isEmpty

just :: NominalType a => a -> Set a
just e = insert e empty

insertAll :: NominalType a => [a] -> Set a -> Set a
insertAll es s = foldl (flip insert) s es

fromList :: NominalType a => [a] -> Set a
fromList es = insertAll es empty

deleteAll :: NominalType a => [a] -> Set a -> Set a
deleteAll es s = foldl (flip delete) s es

filter :: NominalType a => (a -> Formula) -> Set a -> Set a
filter f s = sum $ map (\x -> (iF (f x) (just x) empty)) s

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
difference s1 s2 = filter (notContains s1) s2

infixl 9 \\
(\\) :: NominalType a => Set a -> Set a -> Set a
s1 \\ s2 = difference s1 s2

pairs :: (NominalType a, NominalType b) => Set a -> Set b -> Set (a, b)
pairs s1 s2 = sum $ map (\e1 -> map (\e2 -> (e1, e2)) s2) s1

triples :: (NominalType a, NominalType b, NominalType c) => Set a -> Set b -> Set c -> Set (a, b, c)
triples s1 s2 s3 = sum $ map (\(e1, e2) -> map (\e3 -> (e1, e2, e3)) s3) (pairs s1 s2)
