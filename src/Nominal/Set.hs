{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Nominal.Set (
Set,
-- ** Construction
empty,
atoms,
fromList,
singleton,
insert,
insertAll,
delete,
deleteAll,
-- ** Emptiness
isEmpty,
isNotEmpty,
-- ** Map and filter
map,
filter,
mapFilter,
sum,
exists,
forAll,
partition,
-- ** Membership
contains,
notContains,
member,
notMember,
-- ** Subsets
isSubsetOf,
isNotSubsetOf,
isProperSubsetOf,
isNotProperSubsetOf,
-- ** Combine
union,
unions,
intersection,
intersect,
disjoint,
difference,
(\\),
-- ** Pairs and triples
pairs,
pairsWith,
pairsWithFilter,
square,
atomsPairs,
differentAtomsPairs,
triples,
triplesWith,
triplesWithFilter,
atomsTriples,
mapList,
-- ** Replicate
replicateSet,
replicateSetUntil,
replicateAtoms,
replicateAtomsUntil,
-- ** Size
hasSizeLessThan,
hasSize,
size,
maxSize,
isSingleton,
-- ** Set of atoms properties
range,
openRange,
isLowerBound,
hasLowerBound,
isUpperBound,
hasUpperBound,
isMinimum,
hasMinimum,
isMaximum,
hasMaximum,
isInfimum,
isSupremum,
isConnected,
isOpen,
isClosed,
isCompact) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import qualified Data.List as List ((\\))
import Data.List.Utils (join)
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word (Word)
import Nominal.Atoms
import Nominal.Conditional
import Nominal.Contextual
import Nominal.Formula
import Nominal.Formula.Operators (getEquationsFromFormula)
import Nominal.Maybe
import qualified Nominal.Text.Symbols as Symbols
import Nominal.Type (FoldVarFun, MapVarFun, BareNominalType(..), NominalType(..), Scope(..), collectWith, getAllVariables, mapVariablesIf, neq, replaceVariables)
import qualified Nominal.Util.InsertionSet as ISet
import Nominal.Util.UnionFind (representatives)
import Nominal.Variable (Identifier, Variable, changeIterationLevel, clearIdentifier, getIterationLevel, hasIdentifierEquals,
                         hasIdentifierNotEquals, iterationVariablesList, iterationVariable, setIdentifier, variableName)
import Nominal.Variants (Variants, fromVariant, toList, variant)
import Prelude hiding (or, and, not, sum, map, filter)
import System.IO.Unsafe (unsafePerformIO)

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

checkEquality :: NominalType a => (a, SetElementCondition) -> (a, SetElementCondition)
checkEquality (v, (vs, c)) =
    if Set.null eqs
    then (v, (vs, c))
    else if Map.null eqsMap
         then (v, (vs, c))
         else checkVariablesInElement (replaceVariables eqsMap v, (vs', replaceVariables eqsMap c))
    where eqs = getEquationsFromFormula c
          (vs', eqsMap) = foldr checkVars (vs, Map.empty) $ representatives $ Set.elems eqs
          checkVars (x1, x2) (vs, m)
              | Set.member x1 vs = (Set.delete x1 vs, Map.insert x1 x2 m)
              | Set.member x2 vs = (Set.delete x2 vs, Map.insert x2 x1 m)
              | otherwise        = (vs, m)


----------------------------------------------------------------------------------------------------
-- Identifiers
----------------------------------------------------------------------------------------------------

checkIdentifiers :: (NominalType a, NominalType b) => Identifier -> (a, (b, SetElementCondition)) -> (a, (b, SetElementCondition))
checkIdentifiers id (oldV, (newV, cond)) =
    let otherVarsLevels = Set.toAscList $ collectWith (\var -> if hasIdentifierNotEquals id var then getIterationLevel var else Nothing) (oldV, newV)
        iterVarsLevels = Set.toAscList $ collectWith (\var -> if hasIdentifierEquals id var then getIterationLevel var else Nothing) (newV, cond)
        newIterVarsLevels = [0..] List.\\ otherVarsLevels
        changeLevelsMap = Map.fromList $ zip iterVarsLevels newIterVarsLevels
    in mapVariablesIf (hasIdentifierEquals id) (clearIdentifier . changeIterationLevel changeLevelsMap) (oldV, (newV, cond))

counter :: IORef Word
{-# NOINLINE counter #-}
counter = unsafePerformIO $ newIORef 0

getVariableId :: Set.Set Variable -> Word
{-# NOINLINE getVariableId #-}
getVariableId vs = unsafePerformIO $
  do
    i <- readIORef counter
    writeIORef counter (i + (fromIntegral $ Set.size vs) + 1)
    return i

applyWithIdentifiers :: (NominalType a, NominalType b) => (a -> b) -> (a, SetElementCondition) -> [(a, (b, SetElementCondition))]
applyWithIdentifiers f (v, cond) =
    let id = getVariableId $ fst $ cond
        (v', cond') = mapVariablesIf (flip Set.member $ fst cond) (setIdentifier id) (v, cond)
    in fmap (\(v'', c) -> checkIdentifiers id (v', (v'', fmap (/\ c) cond'))) (toList $ variants $ f v')

----------------------------------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------------------------------

-- | The set of elements, can be infinite.
newtype Set a = Set {setElements :: Map a SetElementCondition} deriving (Eq, Ord, Generic, NFData)

instance Show a => Show (Set a) where
    show s = "{" ++ (join ", " (fmap showSetElement (Map.assocs $ setElements s))) ++ "}"
      where showSetElement (v, (vs, c)) =
              let formula = if c == true then "" else " " ++ show c
                  variables = if Set.null vs
                                then ""
                                else " for " ++ (join "," (fmap show $ Set.elems vs)) ++ " " ++ Symbols.inSet ++ " " ++ Symbols.atoms
                  condition = formula ++ variables
              in show v ++ (if null condition then "" else " :" ++ condition)

instance NominalType a => Conditional (Set a) where
    cond c s1 s2 = union (filter (const c) s1) (filter (const $ not c) s2)

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

instance NominalType a => BareNominalType (Set a) where
    eq s1 s2 = (isSubsetOf s1 s2) /\ (isSubsetOf s2 s1)
    variants = variant
    mapVariables f (Set es) = Set $ Map.fromListWith sumCondition $ fmap (mapSetVariables f) (Map.assocs es)
    foldVariables f acc (Set es) = foldl (foldSetVariables f) acc (Map.assocs es)

----------------------------------------------------------------------------------------------------
-- Similar instances
----------------------------------------------------------------------------------------------------

instance NominalType a => BareNominalType (Set.Set a) where
    eq s1 s2 = eq (fromList $ Set.elems s1) (fromList $ Set.elems s2)
    variants = variant
    mapVariables f = Set.map (mapVariables f)
    foldVariables f acc = foldVariables f acc . Set.elems

instance (NominalType k, NominalType a) => BareNominalType (Map k a) where
    eq m1 m2 = eq (fromList $ Map.assocs m1) (fromList $ Map.assocs m2)
    variants = variant
    mapVariables f = Map.fromList . mapVariables f . Map.assocs
    foldVariables f acc = foldVariables f acc . Map.assocs

----------------------------------------------------------------------------------------------------
-- Basic operations on the set
----------------------------------------------------------------------------------------------------

-- | Returns an empty set.
empty :: Set a
empty = Set Map.empty

-- | Checks whether the set is not empty.
isNotEmpty :: Set a -> Formula
isNotEmpty (Set es) = or $ fmap getCondition $ Map.elems es

-- | Insert an element to a set.
insert :: NominalType a => a -> Set a -> Set a
insert e (Set es) = Set $ foldr insertVariant es (toList $ variants e)
    where insertVariant (v, c) = Map.insertWith sumCondition v (Set.empty, c)

-- | Delete an element from a set.
delete :: NominalType a => a -> Set a -> Set a
delete e = filter (not . (eq e)) . Set . (Map.delete e) . setElements

-- | Applies function to all elements of a set and returns a new set.
map :: (NominalType a, NominalType b) => (a -> b) -> Set a -> Set b
map f = Set . filterNotFalse
            . checkVariables
            . Map.fromListWith sumCondition
            . Map.foldrWithKey (mapAndMerge f) []
            . setElements
    where mapAndMerge f v cond rs = fmap snd (applyWithIdentifiers f (v, cond)) ++ rs

-- | Filter all elements that satisfy the predicate.
filter :: NominalType a => (a -> Formula) -> Set a -> Set a
filter f = Set . filterNotFalse
               . Map.fromListWith sumCondition
               . Map.foldrWithKey (filterAndMerge f) []
               . setElements
    where filterAndMerge f v cond rs = fmap (\(v', (c, cond')) -> checkEquality (v', (fst cond', c /\ snd cond')))
                                            (applyWithIdentifiers f (v, cond))
                                       ++ rs

-- | For a set of sets returns the union of these sets.
sum :: NominalType a => Set (Set a) -> Set a
sum = Set . checkVariables
          . Map.unionsWith sumCondition . fmap filterSetInSet . Map.assocs . setElements
    where filterSetInSet (elemSet, elemSetCond) = filterSetElems (const elemSetCond) (setElements elemSet)

-- | Returns the set of all atoms.
atoms :: Set Atom
atoms = let iterVar = iterationVariable 0 1
        in Set $ Map.singleton (variant iterVar) (Set.singleton iterVar, true)

----------------------------------------------------------------------------------------------------
-- Additional functions
----------------------------------------------------------------------------------------------------

-- | Checks whether the set is empty.
isEmpty :: Set a -> Formula
isEmpty = not . isNotEmpty

-- | Create a set with one given element.
singleton :: NominalType a => a -> Set a
singleton e = insert e empty

-- | Insert all elements from a list to a set.
insertAll :: NominalType a => [a] -> Set a -> Set a
insertAll es s = foldl (flip insert) s es

-- | Create a set from a list of elements.
fromList :: NominalType a => [a] -> Set a
fromList es = insertAll es empty

-- | Applies function to all elements of a set and extract value from 'Just' results and remove elements with 'Nothing' results.
mapFilter :: (NominalType a, NominalType b) => (a -> NominalMaybe b) -> Set a -> Set b
mapFilter f = map fromVariant . map fromJust . filter isJust . map f

-- | Delete all elements from a list from a set.
deleteAll :: NominalType a => [a] -> Set a -> Set a
deleteAll es s = foldl (flip delete) s es

-- | Returns a formula describing the membership of an element that satisfy the predicate.
exists :: NominalType a => (a -> Formula) -> Set a -> Formula
exists f = isNotEmpty . (filter f)

-- | Returns a formula describing the condition that all elements of a set satisfy the predicate.
forAll :: NominalType a => (a -> Formula) -> Set a -> Formula
forAll f = isEmpty . (filter $ \x -> not (f x))

-- | Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
partition :: NominalType a => (a -> Formula) -> Set a -> (Set a, Set a)
partition f s = let ss = map (\e -> (e, f e)) s
                in (map fst $ filter snd ss, map fst $ filter (not . snd) ss)

-- | Returns union of two given sets.
union :: NominalType a => Set a -> Set a -> Set a
union s1 s2 = sum (insert s1 (singleton s2))

-- | Returns union of sets from a list.
unions :: NominalType a => [Set a] -> Set a
unions = foldl union empty

-- | Returns a formula describing the membership of an element in a set.
contains :: NominalType a => Set a -> a -> Formula
contains s e = exists (eq e) s

-- | Returns a formula describing the lack of an element in a set.
notContains :: NominalType a => Set a -> a -> Formula
notContains s = not . contains s

-- | Returns a formula describing the membership of an element in a set.
member :: NominalType a => a -> Set a -> Formula
member = flip contains

-- | Returns a formula describing the lack of an element in a set.
notMember :: NominalType a => a -> Set a -> Formula
notMember = flip notContains

-- | Returns a formula describing that the first set is a subset of the second set.
isSubsetOf :: NominalType a => Set a -> Set a -> Formula
isSubsetOf s1 s2 = forAll (contains s2) s1

-- | Returns a formula describing that the first set is not a subset of the second set.
isNotSubsetOf :: NominalType a => Set a -> Set a -> Formula
isNotSubsetOf s = not . isSubsetOf s

-- | Returns a formula describing that the first set is a proper subset of the second set.
isProperSubsetOf :: NominalType a => Set a -> Set a -> Formula
isProperSubsetOf s1 s2 = (isSubsetOf s1 s2) /\ (isNotSubsetOf s2 s1)

-- | Returns a formula describing that the first set is not a proper subset of the second set.
isNotProperSubsetOf :: NominalType a => Set a -> Set a -> Formula
isNotProperSubsetOf s = not . isProperSubsetOf s

-- | Returns an intersetion of two sets.
intersection :: NominalType a => Set a -> Set a -> Set a
intersection s1 s2 = filter (contains s1) s2

-- | Checks whether two sets intersect.
intersect :: NominalType a => Set a -> Set a -> Formula
intersect s1 s2 = isNotEmpty $ intersection s1 s2

-- | Checks whether two sets are disjoint.
disjoint :: NominalType a => Set a -> Set a -> Formula
disjoint s1 s2 = not (intersect s1 s2)

-- | Returns a difference of two sets.
difference :: NominalType a => Set a -> Set a -> Set a
difference s1 s2 = filter (notContains s2) s1

-- | See 'difference'.
infixl 9 \\
(\\) :: NominalType a => Set a -> Set a -> Set a
s1 \\ s2 = difference s1 s2

-- | Creates a set of pairs of elements from two sets.
pairs :: (NominalType a, NominalType b) => Set a -> Set b -> Set (a, b)
pairs = pairsWith (,)

-- | Creates a set of results of applying a function to elements from two sets.
pairsWith :: (NominalType a, NominalType b, NominalType c) => (a -> b -> c) -> Set a -> Set b -> Set c
pairsWith f s1 s2 = sum $ map (\e1 -> map (f e1) s2) s1

-- | The composition of 'pairsWith' and 'mapFilter'.
pairsWithFilter :: (NominalType a, NominalType b, NominalType c) => (a -> b -> NominalMaybe c) -> Set a -> Set b -> Set c
pairsWithFilter f s1 s2 = mapFilter id (pairsWith f s1 s2)

-- | Creates a set of all pairs of elements from a set.
square :: NominalType a => Set a -> Set (a, a)
square s = pairs s s

-- | Creates a set of all atoms pairs.
atomsPairs :: Set (Atom, Atom)
atomsPairs  = square atoms

-- | Creates a set of all pairs of different atoms.
differentAtomsPairs :: Set (Atom, Atom)
differentAtomsPairs = filter (uncurry neq) atomsPairs

-- | Creates a set of triples of elements from three sets.
triples :: (NominalType a, NominalType b, NominalType c) => Set a -> Set b -> Set c -> Set (a, b, c)
triples = triplesWith (,,)

-- | Creates a set of results of applying a function to elements from three sets.
triplesWith :: (NominalType a, NominalType b, NominalType c, NominalType d)
    => (a -> b -> c -> d) -> Set a -> Set b -> Set c -> Set d
triplesWith f s1 s2 s3 = sum $ sum $ map (\e1 -> map (\e2 -> map (f e1 e2) s3) s2) s1

-- | The composition of 'triplesWith' and 'mapFilter'.
triplesWithFilter :: (NominalType a, NominalType b, NominalType c, NominalType d)
    => (a -> b -> c -> NominalMaybe d) -> Set a -> Set b -> Set c -> Set d
triplesWithFilter f s1 s2 s3 = mapFilter id (triplesWith f s1 s2 s3)

-- | Creates a set of all atoms triples.
atomsTriples :: Set (Atom, Atom, Atom)
atomsTriples = triples atoms atoms atoms

-- | Applies a function to all lists of elements from a list of sets, e.g.
--
-- >>> mapList id [atoms, fromList [a,b]]
-- {[a₁,a] : for a₁ ∊ 𝔸, [a₁,b] : for a₁ ∊ 𝔸}
mapList :: (NominalType a, NominalType b) => ([a] -> b) -> [Set a] -> Set b
mapList f sets = map f $ foldr (pairsWith (:)) (singleton []) sets

-- | Creates a set of lists of a given length of elements from a set, e.g.
--
-- >>> replicateSet 3 atoms
-- {[a₁,a₂,a₃] : for a₁,a₂,a₃ ∊ 𝔸}
replicateSet :: NominalType a => Int -> Set a -> Set [a]
replicateSet n s = mapList id (replicate n s)

-- | Creates a set of lists up to a given length of elements from a set, e.g.
--
-- >>> replicateSetUntil 3 atoms
-- {[], [a₁] : for a₁ ∊ 𝔸, [a₁,a₂] : for a₁,a₂ ∊ 𝔸, [a₁,a₂,a₃] : for a₁,a₂,a₃ ∊ 𝔸}
replicateSetUntil :: NominalType a => Int -> Set a -> Set [a]
replicateSetUntil n s = unions $ fmap (flip replicateSet s) [0..n]

-- |
-- > replicateAtoms n = replicateSet n atoms
replicateAtoms :: Int -> Set [Atom]
replicateAtoms n = replicateSet n atoms

-- |
-- > replicateAtomsUntil n = replicateSetUntil n atoms
replicateAtomsUntil :: Int -> Set [Atom]
replicateAtomsUntil n = replicateSetUntil n atoms

-- | Returns a formula describing condition that a set has a size less than a given number.
-- It is an inefficient function for large sets and will not return the answer for the infinite sets.
hasSizeLessThan :: NominalType a => Set a -> Int -> Formula
hasSizeLessThan s n = forAll id $ mapList (\xs -> let l = length xs in or [eq (xs!!i) (xs!!j) | i <- [0..l-1], j <- [0..l-1], i<j]) (replicate n s)

-- | Returns a formula describing condition that a set has a given size.
-- It is an inefficient function for large sets and will not return the answer for the infinite sets.
hasSize :: NominalType a => Set a -> Int -> Formula
hasSize s n = hasSizeLessThan s (succ n) /\ not (hasSizeLessThan s n)

-- | Returns a variants of numbers of the size of a set.
-- It is an inefficient function for large sets and will not return the answer for the infinite sets.
size :: NominalType a => Set a -> Variants Int
size s = findSize s 1 where findSize s n = ite (hasSizeLessThan s n) (variant $ pred n) (findSize s (succ n))

-- | Returns the maximum size of a set for all free atoms constraints.
-- It is an inefficient function for large sets and will not return the answer for the infinite sets.
maxSize :: NominalType a => Set a -> Int
maxSize s = findSize s 1 where findSize s n = if isTrue (hasSizeLessThan s n) then pred n else findSize s (succ n)

-- | Returns a formula describing condition that a set has exacly one element.
isSingleton :: NominalType a => Set a -> Formula
isSingleton s = hasSize s 1

----------------------------------------------------------------------------------------------------
-- Set of atoms properties
----------------------------------------------------------------------------------------------------

-- | The closed interval of atoms with given minimum and maximum.
range :: Atom -> Atom -> Set Atom
range l u = filter (\a -> ge a l /\ le a u) atoms

-- | The open interval of atoms with given infimum and suprememum.
openRange :: Atom -> Atom -> Set Atom
openRange l u = filter (\a -> gt a l /\ lt a u) atoms

-- | Checks whether a given atom is the lower bound of a set.
isLowerBound :: Atom -> Set Atom -> Formula
isLowerBound a s = forAll (le a) s

-- | Checks whether a set has the lower bound.
hasLowerBound :: Set Atom -> Formula
hasLowerBound s = exists (`isLowerBound` s) atoms

-- | Checks whether a given atom is the upper bound of a set.
isUpperBound :: Atom -> Set Atom -> Formula
isUpperBound a s = forAll (ge a) s

-- | Checks whether a set has the upper bound.
hasUpperBound :: Set Atom -> Formula
hasUpperBound s = exists (`isUpperBound` s) atoms

-- | Checks whether an atom is the minimum of a set.
isMinimum :: Atom -> Set Atom -> Formula
isMinimum a s = member a s /\ isLowerBound a s

-- | Checks whether a set has the minimum.
hasMinimum :: Set Atom -> Formula
hasMinimum s = exists (`isLowerBound` s) s

-- | Checks whether an atom is the maximum of a set.
isMaximum :: Atom -> Set Atom -> Formula
isMaximum a s = member a s /\ isUpperBound a s

-- | Checks whether a set has the maximum.
hasMaximum :: Set Atom -> Formula
hasMaximum s = exists (`isUpperBound` s) s

-- | Checks whether an atom is the infumum of a set.
isInfimum :: Atom -> Set Atom -> Formula
isInfimum a s = isMaximum a $ filter (`isLowerBound` s) atoms

-- | Checks whether an atom is the supremum of a set.
isSupremum :: Atom -> Set Atom -> Formula
isSupremum a s = isMinimum a $ filter (`isUpperBound` s) atoms

-- | Checks whether a set is connected.
isConnected :: Set Atom -> Formula
isConnected s = forAll (\a -> isUpperBound a s \/ isLowerBound a s) $ atoms \\ s

-- | Checks whether a set is open.
isOpen :: Set Atom -> Formula
isOpen s = forAll (\a -> exists (\(b,c) -> lt b a /\ lt a c /\ isSubsetOf (range b c) s) atomsPairs) s

-- | Checks whether a set is closed.
isClosed :: Set Atom -> Formula
isClosed s = isOpen $ atoms \\ s

-- | Checks whether a set is compact.
isCompact :: Set Atom -> Formula
isCompact s = isClosed s /\ hasUpperBound s /\ hasLowerBound s
