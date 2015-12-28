module Nominal.Set (
Set,
empty,
isEmpty,
isNotEmpty,
insert,
delete,
map,
filter,
sum,
atoms,

singleton,
insertAll,
fromList,
mapFilter,
deleteAll,
exists,
forAll,
union,
unions,
contains,
notContains,
member,
notMember,
isSubsetOf,
isNotSubsetOf,
isProperSubsetOf,
isNotProperSubsetOf,
intersection,
intersect,
difference,
(Nominal.Set.\\),

pairs,
pairsWith,
pairsWithFilter,
square,
atomsPairs,
triples,
triplesWith,
triplesWithFilter,
atomsTriples,
mapList,
replicateSet,
replicateSetUntil,
replicateAtoms,
replicateAtomsUntil,
hasSizeLessThan,
hasSize,
size,
isSingleton) where

import Data.List ((\\))
import Data.List.Utils (join)
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Nominal.Atom (Atom, atom)
import Nominal.Conditional
import Nominal.Contextual
import Nominal.Formula
import Nominal.Maybe
import Nominal.Type (FoldVarFun, MapVarFun, NominalType(..), Scope(..), collectWith, getAllVariables, mapVariablesIf, neq, replaceVariables)
import qualified Nominal.Util.InsertionSet as ISet
import Nominal.Variable (Identifier, Variable, changeIterationLevel, clearIdentifier, getIterationLevel, hasIdentifierEquals,
                         hasIdentifierNotEquals, iterationVariablesList, iterationVariable, setIdentifier, variableName)
import Nominal.Variants (Variants, fromVariant, toList, variant)
import Prelude hiding (or, and, not, sum, map, filter)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)

----------------------------------------------------------------------------------------------------
-- Set elements
----------------------------------------------------------------------------------------------------

type SetElementCondition = (Set.Set Variable, Formula)

getCondition :: SetElementCondition -> Formula
getCondition (vs, c) = Set.foldr (∃) c vs

sumCondition :: SetElementCondition -> SetElementCondition -> SetElementCondition
sumCondition (vs1, c1) (vs2, c2) = (Set.union vs1 vs2, c1 \/ c2)

checkVariablesInElement :: NominalType a => (a, SetElementCondition) -> (a, SetElementCondition)
checkVariablesInElement (v, (vs, c)) =
    let iterVars = foldVariables (All, \var -> if Set.member var vs then ISet.insert var else id) ISet.empty v
        formulaVars = getAllVariables c
        c' = getCondition (Set.intersection formulaVars (vs Set.\\ ISet.toSet iterVars), c)
    in if ISet.null iterVars
       then (v, (Set.empty, c'))
       else let oldIterVars = ISet.toList iterVars
                newIterVars = iterationVariablesList (minimum $ fmap (Maybe.fromJust . getIterationLevel) $ Set.elems vs)
                                                     (length oldIterVars)
            in if oldIterVars == newIterVars
               then (v, (Set.fromList oldIterVars, c'))
               else let replaceMap = (Map.fromList $ zip oldIterVars newIterVars)
                    in (replaceVariables replaceMap v, (Set.fromList newIterVars, replaceVariables replaceMap c'))

checkVariables :: NominalType a => Map a SetElementCondition -> Map a SetElementCondition
checkVariables = Map.fromListWith sumCondition . Map.foldrWithKey (\v c es -> checkVariablesInElement (v, c) : es) []

filterSetElems :: NominalType a => (a -> SetElementCondition) -> Map a SetElementCondition -> Map a SetElementCondition
filterSetElems f = filterNotFalse . Map.mapWithKey (\v (vs, c) -> let fv = f v in (Set.union vs $ fst fv, c /\ snd fv))

filterNotFalse :: Map a SetElementCondition -> Map a SetElementCondition
filterNotFalse = Map.filter ((/= false) . snd)

----------------------------------------------------------------------------------------------------
-- Identifiers
----------------------------------------------------------------------------------------------------

checkIdentifiers :: (NominalType a, NominalType b) => Identifier -> (a, (b, SetElementCondition)) -> (a, (b, SetElementCondition))
checkIdentifiers id (oldV, (newV, cond)) =
    let otherVarsLevels = Set.toAscList $ collectWith (\var -> if hasIdentifierNotEquals id var then getIterationLevel var else Nothing) newV
        iterVarsLevels = Set.toAscList $ collectWith (\var -> if hasIdentifierEquals id var then getIterationLevel var else Nothing) (newV, cond)
        newIterVarsLevels = [0..] Data.List.\\ otherVarsLevels
        changeLevelsMap = Map.fromList $ zip iterVarsLevels newIterVarsLevels
    in mapVariablesIf (hasIdentifierEquals id) (clearIdentifier . changeIterationLevel changeLevelsMap) (oldV, (newV, cond))

getVariableId :: NominalType a => a -> IO Int
getVariableId v = do {r <- randomIO; return $ (length $ toList $ variants v) + r}

applyWithIdentifiers :: (NominalType a, NominalType b) => (a -> b) -> (a, SetElementCondition) -> [(a, (b, SetElementCondition))]
applyWithIdentifiers f (v, cond) =
    let id = unsafePerformIO $ getVariableId v
        (v', cond') = mapVariablesIf (flip Set.member $ fst cond) (setIdentifier id) (v, cond)
    in fmap (\(v'', c) -> checkIdentifiers id (v', (v'', fmap (/\ c) cond'))) (toList $ variants $ f v')

----------------------------------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------------------------------

data Set a = Set {setElements :: Map a SetElementCondition} deriving (Eq, Ord)

instance Show a => Show (Set a) where
    show s = "{" ++ (join ", " (fmap showSetElement (Map.assocs $ setElements s))) ++ "}"
      where showSetElement (v, (vs, c)) =
              let formula = if c == true then "" else " " ++ show c
                  variables = if Set.null vs
                                then ""
                                else " for " ++ (join "," (fmap show $ Set.elems vs)) ++ " ∊ 𝔸"
                  condition = formula ++ variables
              in show v ++ (if null condition then "" else " :" ++ condition)

instance NominalType a => Conditional (Set a) where
    ite c s1 s2 = union (filter (const c) s1) (filter (const $ not c) s2)

instance (Contextual a, Ord a) => Contextual (Set a) where
    when ctx (Set es) = Set $ filterNotFalse
                            $ Map.fromListWith sumCondition
                            $ fmap (\(v,(vs, c)) -> (when (ctx /\ c) v, when ctx (vs, c)))
                            $ Map.assocs es

mapWithout :: Set.Set Variable -> (Variable -> Variable) -> Variable -> Variable
mapWithout vs f x = if Set.member x vs then x else f x

mapSetVariables :: NominalType a => MapVarFun -> (a, SetElementCondition) -> (a, SetElementCondition)
mapSetVariables (All, f) se = mapVariables (All, f) se
mapSetVariables (Free, f) (v, (vs, c)) = mapVariables (Free, (mapWithout vs f)) (v, (vs, c))

foldWithout :: Set.Set Variable -> (Variable -> b -> b) -> Variable -> b -> b
foldWithout vs f x = if Set.member x vs then id else f x

foldSetVariables :: NominalType a => FoldVarFun b -> b -> (a, SetElementCondition) -> b
foldSetVariables (All, f) acc se = foldVariables (All, f) acc se
foldSetVariables (Free, f) acc (v, (vs, c)) = foldVariables (Free, (foldWithout vs f)) acc (v, (vs, c))

instance NominalType a => NominalType (Set a) where
    eq s1 s2 = (isSubsetOf s1 s2) /\ (isSubsetOf s2 s1)
    mapVariables f (Set es) = Set $ Map.fromListWith sumCondition $ fmap (mapSetVariables f) (Map.assocs es)
    foldVariables f acc (Set es) = foldl (foldSetVariables f) acc (Map.assocs es)

----------------------------------------------------------------------------------------------------
-- Similar instances
----------------------------------------------------------------------------------------------------

instance NominalType a => NominalType (Set.Set a) where
    eq s1 s2 = eq (fromList $ Set.elems s1) (fromList $ Set.elems s2)
    mapVariables f = Set.map (mapVariables f)
    foldVariables f acc = foldVariables f acc . Set.elems

instance (NominalType k, NominalType a) => NominalType (Map k a) where
    eq m1 m2 = eq (fromList $ Map.assocs m1) (fromList $ Map.assocs m2)
    mapVariables f = Map.fromList . mapVariables f . Map.assocs
    foldVariables f acc = foldVariables f acc . Map.assocs

----------------------------------------------------------------------------------------------------
-- Operations on set
----------------------------------------------------------------------------------------------------

empty :: Set a
empty = Set Map.empty

isNotEmpty :: Set a -> Formula
isNotEmpty (Set es) = or $ fmap getCondition $ Map.elems es

insert :: NominalType a => a -> Set a -> Set a
insert e (Set es) = Set $ foldr insertVariant es (toList $ variants e)
    where insertVariant (v, c) = Map.insertWith sumCondition v (Set.empty, c)

delete :: NominalType a => a -> Set a -> Set a
delete e = filter (not . (eq e)) . Set . (Map.delete e) . setElements

map :: (NominalType a, NominalType b) => (a -> b) -> Set a -> Set b
map f = Set . filterNotFalse
            . checkVariables
            . Map.fromListWith sumCondition
            . Map.foldrWithKey (mapAndMerge f) []
            . setElements
    where mapAndMerge f v cond rs = fmap snd (applyWithIdentifiers f (v, cond)) ++ rs

filter :: NominalType a => (a -> Formula) -> Set a -> Set a
filter f = Set . filterNotFalse
               . Map.fromListWith sumCondition
               . Map.foldrWithKey (filterAndMerge f) []
               . setElements
    where filterAndMerge f v cond rs = fmap (\(v', (c, cond')) -> (v', (fst cond', c /\ snd cond'))) (applyWithIdentifiers f (v, cond)) ++ rs

sum :: NominalType a => Set (Set a) -> Set a
sum = Set . checkVariables
          . Map.unionsWith sumCondition . fmap filterSetInSet . Map.assocs . setElements
    where filterSetInSet (elemSet, elemSetCond) = filterSetElems (const elemSetCond) (setElements elemSet)

atoms :: Set Atom
atoms = let iterVar = iterationVariable 0 1
        in Set $ Map.singleton (variant iterVar) (Set.singleton iterVar, true)

----------------------------------------------------------------------------------------------------
-- Additional functions
----------------------------------------------------------------------------------------------------

isEmpty :: Set a -> Formula
isEmpty = not . isNotEmpty

singleton :: NominalType a => a -> Set a
singleton e = insert e empty

insertAll :: NominalType a => [a] -> Set a -> Set a
insertAll es s = foldl (flip insert) s es

fromList :: NominalType a => [a] -> Set a
fromList es = insertAll es empty

mapFilter :: (NominalType a, NominalType b) => (a -> NominalMaybe b) -> Set a -> Set b
mapFilter f = map fromVariant . map fromJust . filter isJust . map f

deleteAll :: NominalType a => [a] -> Set a -> Set a
deleteAll es s = foldl (flip delete) s es

exists :: NominalType a => (a -> Formula) -> Set a -> Formula
exists f = isNotEmpty . (filter f)

forAll :: NominalType a => (a -> Formula) -> Set a -> Formula
forAll f = isEmpty . (filter $ \x -> not (f x))

union :: NominalType a => Set a -> Set a -> Set a
union s1 s2 = sum (insert s1 (singleton s2))

unions :: NominalType a => [Set a] -> Set a
unions = foldl union empty

contains :: NominalType a => Set a -> a -> Formula
contains s e = exists (eq e) s

notContains :: NominalType a => Set a -> a -> Formula
notContains s = not . contains s

member :: NominalType a => a -> Set a -> Formula
member = flip contains

notMember :: NominalType a => a -> Set a -> Formula
notMember = flip notContains

isSubsetOf :: NominalType a => Set a -> Set a -> Formula
isSubsetOf s1 s2 = forAll (contains s2) s1

isNotSubsetOf :: NominalType a => Set a -> Set a -> Formula
isNotSubsetOf s = not . isSubsetOf s

isProperSubsetOf :: NominalType a => Set a -> Set a -> Formula
isProperSubsetOf s1 s2 = (isSubsetOf s1 s2) /\ (isNotSubsetOf s2 s1)

isNotProperSubsetOf :: NominalType a => Set a -> Set a -> Formula
isNotProperSubsetOf s = not . isProperSubsetOf s

intersection :: NominalType a => Set a -> Set a -> Set a
intersection s1 s2 = filter (contains s1) s2

intersect :: NominalType a => Set a -> Set a -> Formula
intersect s1 s2 = isNotEmpty $ intersection s1 s2

difference :: NominalType a => Set a -> Set a -> Set a
difference s1 s2 = filter (notContains s2) s1

infixl 9 \\
(\\) :: NominalType a => Set a -> Set a -> Set a
s1 \\ s2 = difference s1 s2

pairs :: (NominalType a, NominalType b) => Set a -> Set b -> Set (a, b)
pairs = pairsWith (,)

pairsWith :: (NominalType a, NominalType b, NominalType c) => (a -> b -> c) -> Set a -> Set b -> Set c
pairsWith f s1 s2 = sum $ map (\e1 -> map (f e1) s2) s1

pairsWithFilter :: (NominalType a, NominalType b, NominalType c) => (a -> b -> NominalMaybe c) -> Set a -> Set b -> Set c
pairsWithFilter f s1 s2 = mapFilter id (pairsWith f s1 s2)

square :: NominalType a => Set a -> Set (a, a)
square s = pairs s s

atomsPairs :: Set (Atom, Atom)
atomsPairs  = square atoms

triples :: (NominalType a, NominalType b, NominalType c) => Set a -> Set b -> Set c -> Set (a, b, c)
triples = triplesWith (,,)

triplesWith :: (NominalType a, NominalType b, NominalType c, NominalType d)
    => (a -> b -> c -> d) -> Set a -> Set b -> Set c -> Set d
triplesWith f s1 s2 s3 = sum $ sum $ map (\e1 -> map (\e2 -> map (f e1 e2) s3) s2) s1

triplesWithFilter :: (NominalType a, NominalType b, NominalType c, NominalType d)
    => (a -> b -> c -> NominalMaybe d) -> Set a -> Set b -> Set c -> Set d
triplesWithFilter f s1 s2 s3 = mapFilter id (triplesWith f s1 s2 s3)

atomsTriples :: Set (Atom, Atom, Atom)
atomsTriples = triples atoms atoms atoms

mapList :: (NominalType a, NominalType b) => ([a] -> b) -> [Set a] -> Set b
mapList f sets = map f $ foldr (pairsWith (:)) (singleton []) sets

replicateSet :: NominalType a => Int -> Set a -> Set [a]
replicateSet n s = mapList id (replicate n s)

replicateSetUntil :: NominalType a => Int -> Set a -> Set [a]
replicateSetUntil n s = unions $ fmap (flip replicateSet s) [0..n]

replicateAtoms :: Int -> Set [Atom]
replicateAtoms n = replicateSet n atoms

replicateAtomsUntil :: Int -> Set [Atom]
replicateAtomsUntil n = replicateSetUntil n atoms

hasSizeLessThan :: NominalType a => Set a -> Int -> Formula
hasSizeLessThan s n = forAll id $ mapList (\xs -> let l = length xs in or [eq (xs!!i) (xs!!j) | i <- [0..l-1], j <- [0..l-1], i<j]) (replicate n s)

hasSize :: NominalType a => Set a -> Int -> Formula
hasSize s n = hasSizeLessThan s (succ n) /\ not (hasSizeLessThan s n)

size :: NominalType a => Set a -> Variants Int
size s = findSize s 1 where findSize s n = ite' (hasSizeLessThan s n) (variant $ pred n) (findSize s (succ n))

isSingleton :: NominalType a => Set a -> Formula
isSingleton s = hasSize s 1
