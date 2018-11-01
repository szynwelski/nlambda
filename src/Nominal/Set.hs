{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
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
-- ** Replicate
mapList,
mapFilterList,
replicateSet,
replicateDifferentSet,
replicateSetUntil,
replicateDifferentSetUntil,
replicateAtoms,
replicateDifferentAtoms,
replicateAtomsUntil,
replicateDifferentAtomsUntil,
-- ** Size
hasSizeLessThan,
hasSize,
listSize,
listSizeWith,
listMaxSize,
listMaxSizeWith,
size,
sizeWith,
maxSize,
maxSizeWith,
isSingleton,
-- ** Set elements
element,
toList,
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
isCompact,
-- ** Meta equivalents
empty_nlambda,
isNotEmpty_nlambda,
insert_nlambda,
delete_nlambda,
map_nlambda,
filter_nlambda,
sum_nlambda,
atoms_nlambda,
element_nlambda) where

import Control.Arrow ((***), first)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import qualified Data.List as List ((\\), partition, tails)
import Data.List.Utils (join)
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word (Word)
import GHC.Read (expectP)
import Nominal.Atoms
import Nominal.Atoms.Logic (exclusiveConditions)
import Nominal.Atoms.Signature (defaultConstant)
import Nominal.Conditional
import Nominal.Contextual
import Nominal.Formula
import Nominal.Formula.Definition (getConstraintsFromFormula, getEquationsFromFormula)
import Nominal.Maybe
import Nominal.Meta (NoMetaFunction(..), WithMeta)
import qualified Nominal.Text.Symbols as Symbols
import Nominal.Type (NominalType(..), NominalType_nlambda, neq)
import qualified Nominal.Util.InsertionSet as ISet
import Nominal.Util.UnionFind (representatives)
import Nominal.Util.Read (optional, readSepBy, skipSpaces, spaces, string)
import Nominal.Variable (FoldVarFun, Identifier, MapVarFun, Scope(..), Var(..), Variable, changeIterationLevel, clearIdentifier, collectWith, constantVar, freeVariables,
                         getAllVariables, getIterationLevel, hasIdentifierEquals, hasIdentifierNotEquals, isConstant,
                         iterationVariablesList, iterationVariable, mapVariablesIf, replaceVariables, renameWithFlatTree, setIdentifier)
import Nominal.Variants (Variants, fromVariant, readVariant, variant)
import qualified Nominal.Variants as V
import Prelude hiding (or, and, not, sum, map, filter)
import qualified Prelude as P
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadPrec (pfail)
import Text.Read (Lexeme(Punc), ReadPrec, (+++), (<++), lexP, readPrec, reset)

----------------------------------------------------------------------------------------------------
-- Set elements
----------------------------------------------------------------------------------------------------

type SetElementCondition = (Set.Set Variable, Formula)

{-# ANN getCondition NoMetaFunction #-}
getCondition :: SetElementCondition -> Formula
getCondition (vs, c) = Set.foldr (∃) c vs

sumCondition :: SetElementCondition -> SetElementCondition -> SetElementCondition
sumCondition (vs1, c1) (vs2, c2) = (Set.union vs1 vs2, c1 \/ c2)

{-# ANN checkVariablesInElement NoMetaFunction #-}
checkVariablesInElement :: NominalType a => (a, SetElementCondition) -> (a, SetElementCondition)
checkVariablesInElement (v, (vs, c)) =
    let iterVars = foldVariables (All, \var -> if Set.member var vs then ISet.insert var else id) ISet.empty v
        formulaVars = getAllVariables c
        c' = getCondition (Set.intersection formulaVars (vs Set.\\ ISet.toSet iterVars), c)
    in if ISet.null iterVars
       then (v, (Set.empty, c'))
       else let oldIterVars = ISet.toList iterVars
                newIterVars = iterationVariablesList (minimum ((Maybe.fromJust . getIterationLevel) <$> Set.elems vs))
                                                     (length oldIterVars)
            in if oldIterVars == newIterVars
               then (v, (Set.fromList oldIterVars, c'))
               else let replaceMap = (Map.fromList $ zip oldIterVars newIterVars)
                    in (replaceVariables replaceMap v, (Set.fromList newIterVars, replaceVariables replaceMap c'))

{-# ANN checkVariables NoMetaFunction #-}
checkVariables :: NominalType a => Map a SetElementCondition -> Map a SetElementCondition
checkVariables = Map.fromListWith sumCondition . Map.foldrWithKey (\v c es -> checkVariablesInElement (v, c) : es) []

{-# ANN filterSetElems NoMetaFunction #-}
filterSetElems :: NominalType a => (a -> SetElementCondition) -> Map a SetElementCondition -> Map a SetElementCondition
filterSetElems f = filterNotFalse . Map.mapWithKey (\v (vs, c) -> (Set.union vs *** (/\) c) $ f v)

filterNotFalse :: Map a SetElementCondition -> Map a SetElementCondition
filterNotFalse = Map.filter ((/= false) . snd)

{-# ANN checkEquality NoMetaFunction #-}
checkEquality :: NominalType a => (a, SetElementCondition) -> (a, SetElementCondition)
checkEquality (v, (vs, c)) =
    if Set.null eqs || Map.null eqsMap
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

{-# ANN checkIdentifiers NoMetaFunction #-}
checkIdentifiers :: (NominalType a, NominalType b) => Identifier -> (a, (b, SetElementCondition)) -> (a, (b, SetElementCondition))
checkIdentifiers id (oldV, (newV, cond)) =
    let otherVarsLevels = Set.toAscList $ collectWith (\var -> if hasIdentifierNotEquals id var then getIterationLevel var else Nothing) (oldV, newV)
        iterVarsLevels = Set.toAscList $ collectWith (\var -> if hasIdentifierEquals id var then getIterationLevel var else Nothing) (newV, cond)
        newIterVarsLevels = [0..] List.\\ otherVarsLevels
        changeLevelsMap = Map.fromList $ zip iterVarsLevels newIterVarsLevels
    in mapVariablesIf (hasIdentifierEquals id) (clearIdentifier . changeIterationLevel changeLevelsMap) (oldV, (newV, cond))

{-# ANN counter NoMetaFunction #-}
{-# NOINLINE counter #-}
counter :: IORef Word
counter = unsafePerformIO $ newIORef 0

{-# ANN getVariableId NoMetaFunction #-}
{-# NOINLINE getVariableId #-}
getVariableId :: Set.Set Variable -> Word
getVariableId vs = unsafePerformIO $
  do
    i <- readIORef counter
    writeIORef counter (i + fromIntegral (Set.size vs) + 1)
    return i

{-# ANN applyWithIdentifiers NoMetaFunction #-}
applyWithIdentifiers :: (NominalType a, NominalType b) => (a -> b) -> (a, SetElementCondition) -> [(a, (b, SetElementCondition))]
applyWithIdentifiers f (v, cond) =
    let id = getVariableId $ fst cond
        (v', cond') = mapVariablesIf (flip Set.member $ fst cond) (setIdentifier id) (v, cond)
    in fmap (\(v'', c) -> checkIdentifiers id (v', (v'', fmap (/\ c) cond'))) (V.toList $ variants $ f v')

----------------------------------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------------------------------

-- | The set of elements, can be infinite.
newtype Set a = Set {setElements :: Map a SetElementCondition} deriving (Eq, Ord)

instance Show a => Show (Set a) where
    show s = "{" ++ join ", " (fmap showSetElement (Map.assocs $ setElements s)) ++ "}"
      where showSetElement (v, (vs, c)) =
              let formula = if c == true then "" else " " ++ show c
                  variables = if Set.null vs
                                then ""
                                else spaces Symbols.for ++ join "," (show <$> Set.elems vs) ++ spaces Symbols.inSet ++ Symbols.atoms
                  condition = formula ++ variables
              in show v ++ (if null condition then "" else " " ++ Symbols.valueCondSep ++ condition)

readIterVars :: ReadPrec (Set.Set Variable)
readIterVars = do optional $ string Symbols.valueCondSep
                  skipSpaces
                  string $ Symbols.for
                  skipSpaces
                  vs <- readSepBy True "," readPrec
                  skipSpaces
                  string Symbols.inSet
                  skipSpaces
                  string Symbols.atoms
                  return $ Set.fromList vs

readElements :: (NominalType a, Read a) => ReadPrec [(a, SetElementCondition)]
readElements = do (v,c) <- readVariant
                  vs    <- readIterVars <++ return Set.empty
                  return $ fmap (\(v',c') -> (v', (vs, c/\c'))) $ V.toList $ variants v

readSet :: (NominalType a, Read a) => ReadPrec [[(a, SetElementCondition)]]
readSet = do expectP (Punc "{")
             setRest False +++ setNext
  where setRest started = do Punc c <- lexP
                             case c of
                               "}"           -> return []
                               "," | started -> setNext
                               _             -> pfail
        setNext = do x  <- reset readElements
                     xs <- setRest True
                     return (x:xs)

instance (NominalType a, Read a) => Read (Set a) where
    readPrec = do es <- readSet
                  return $ Set $ Map.fromListWith sumCondition $ concat es

instance NominalType a => Conditional (Set a) where
    cond c s1 s2 = filter (const c) s1 `union` filter (const $ not c) s2

instance (Contextual a, Ord a) => Contextual (Set a) where
    when ctx (Set es) = Set $ filterNotFalse
                            $ Map.fromListWith sumCondition
                            $ (\(v,(vs, c)) -> (when (ctx /\ c) v, when ctx (vs, c))) <$> Map.assocs es

{-# ANN mapWithout NoMetaFunction #-}
mapWithout :: Set.Set Variable -> (Variable -> Variable) -> Variable -> Variable
mapWithout vs f x = if Set.member x vs then x else f x

{-# ANN mapSetVariables NoMetaFunction #-}
mapSetVariables :: Var a => MapVarFun -> (a, SetElementCondition) -> (a, SetElementCondition)
mapSetVariables (All, f) se = mapVariables (All, f) se
mapSetVariables (Free, f) (v, (vs, c)) = mapVariables (Free, mapWithout vs f) (v, (vs, c))

{-# ANN foldWithout NoMetaFunction #-}
foldWithout :: Set.Set Variable -> (Variable -> b -> b) -> Variable -> b -> b
foldWithout vs f x = if Set.member x vs then id else f x

{-# ANN foldSetVariables NoMetaFunction #-}
foldSetVariables :: Var a => FoldVarFun b -> b -> (a, SetElementCondition) -> b
foldSetVariables (All, f) acc se = foldVariables (All, f) acc se
foldSetVariables (Free, f) acc (v, (vs, c)) = foldVariables (Free, foldWithout vs f) acc (v, (vs, c))

instance (Ord a, Var a) => Var (Set a) where
    mapVariables f (Set es) = Set $ Map.fromListWith sumCondition $ fmap (mapSetVariables f) (Map.assocs es)
    foldVariables f acc (Set es) = foldl (foldSetVariables f) acc (Map.assocs es)
    renameVariables = renameWithFlatTree

instance NominalType a => NominalType (Set a) where
    eq s1 s2 = isSubsetOf s1 s2 /\ isSubsetOf s2 s1
    variants = variant

----------------------------------------------------------------------------------------------------
-- Similar instances
----------------------------------------------------------------------------------------------------

instance NominalType a => NominalType (Set.Set a) where
    eq s1 s2 = eq (fromList $ Set.elems s1) (fromList $ Set.elems s2)
    variants = variant

instance (NominalType k, NominalType a) => NominalType (Map k a) where
    eq m1 m2 = eq (fromList $ Map.assocs m1) (fromList $ Map.assocs m2)
    variants = variant

----------------------------------------------------------------------------------------------------
-- Basic operations on the set
----------------------------------------------------------------------------------------------------

-- | Returns an empty set.
empty :: Set a
empty = Set Map.empty

-- | Checks whether the set is not empty.
isNotEmpty :: Set a -> Formula
isNotEmpty (Set es) = or (getCondition <$> Map.elems es)

-- | Insert an element to a set.
insert :: NominalType a => a -> Set a -> Set a
insert e (Set es) = Set $ foldr insertVariant es (V.toList $ variants e)
    where insertVariant (v, c) = Map.insertWith sumCondition v (Set.empty, c)

-- | Delete an element from a set.
delete :: NominalType a => a -> Set a -> Set a
delete e = filter (not . eq e) . Set . Map.delete e . setElements

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
-- Meta equivalents for basic operations on the set
----------------------------------------------------------------------------------------------------

empty_nlambda :: WithMeta (Set a)
empty_nlambda = undefined

isNotEmpty_nlambda :: WithMeta (Set a) -> WithMeta Formula
isNotEmpty_nlambda = undefined

insert_nlambda :: NominalType_nlambda a => WithMeta a -> WithMeta (Set a) -> WithMeta (Set a)
insert_nlambda = undefined

delete_nlambda :: NominalType_nlambda a => WithMeta a -> WithMeta (Set a) -> WithMeta (Set a)
delete_nlambda = undefined

map_nlambda :: (NominalType_nlambda a, NominalType_nlambda b) => (WithMeta a -> WithMeta b) -> WithMeta (Set a) -> WithMeta (Set b)
map_nlambda = undefined

filter_nlambda :: NominalType_nlambda a => (WithMeta a -> WithMeta Formula) -> WithMeta (Set a) -> WithMeta (Set a)
filter_nlambda = undefined

sum_nlambda :: NominalType_nlambda a => WithMeta (Set (Set a)) -> WithMeta (Set a)
sum_nlambda = undefined

atoms_nlambda :: WithMeta (Set Atom)
atoms_nlambda = undefined

element_nlambda :: (Contextual_nlambda a, NominalType_nlambda a) => WithMeta (Set a) -> WithMeta (NominalMaybe a)
element_nlambda = undefined

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

-- | Applies function to all elements of a set, extract value from 'Just' results and remove elements with 'Nothing' results.
mapFilter :: (NominalType a, NominalType b) => (a -> NominalMaybe b) -> Set a -> Set b
mapFilter f = map fromVariant . map fromJust . filter isJust . map f

-- | Delete all elements from a list from a set.
deleteAll :: NominalType a => [a] -> Set a -> Set a
deleteAll es s = foldl (flip delete) s es

-- | Returns a formula describing the membership of an element that satisfy the predicate.
exists :: NominalType a => (a -> Formula) -> Set a -> Formula
exists f = isNotEmpty . filter f

-- | Returns a formula describing the condition that all elements of a set satisfy the predicate.
forAll :: NominalType a => (a -> Formula) -> Set a -> Formula
forAll f = isEmpty . filter (not . f)

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
isProperSubsetOf s1 s2 = isSubsetOf s1 s2 /\ isNotSubsetOf s2 s1

-- | Returns a formula describing that the first set is not a proper subset of the second set.
isNotProperSubsetOf :: NominalType a => Set a -> Set a -> Formula
isNotProperSubsetOf s = not . isProperSubsetOf s

-- | Returns an intersetion of two sets.
intersection :: NominalType a => Set a -> Set a -> Set a
intersection s = filter (contains s)

-- | Checks whether two sets intersect.
intersect :: NominalType a => Set a -> Set a -> Formula
intersect s1 s2 = isNotEmpty $ intersection s1 s2

-- | Checks whether two sets are disjoint.
disjoint :: NominalType a => Set a -> Set a -> Formula
disjoint s1 s2 = not (s1 `intersect` s2)

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

-- | Applies a function to all lists of elements from a list of sets, extract value from 'Just' results and remove elements with 'Nothing' results, e.g.
--
-- >>> mapFilterList (\l -> maybeIf (eq a $ head l) l) [atoms, atoms]
-- {[a,a₁] : for a₁ ∊ 𝔸}
mapFilterList :: (NominalType a, NominalType b) => ([a] -> NominalMaybe b) -> [Set a] -> Set b
mapFilterList f sets = mapFilter f $ foldr (pairsWith (:)) (singleton []) sets

-- | Creates a set of lists of a given length of elements from a set, e.g.
--
-- >>> replicateSet 3 atoms
-- {[a₁,a₂,a₃] : for a₁,a₂,a₃ ∊ 𝔸}
replicateSet :: NominalType a => Int -> Set a -> Set [a]
replicateSet n s = mapList id (replicate n s)

-- | Creates a set of lists of a given length of different elements from a set, e.g.
--
-- >>> replicateDifferentSet 3 atoms
-- {[a₁,a₂,a₃] : a₁ ≠ a₂ ∧ a₁ ≠ a₃ ∧ a₂ ≠ a₃ for a₁,a₂,a₃ ∊ 𝔸}
replicateDifferentSet :: NominalType a => Int -> Set a -> Set [a]
replicateDifferentSet n s = mapFilterList (\l -> maybeIf (and [neq a b | (a:as) <- List.tails l, b <- as]) l) (replicate n s)

-- | Creates a set of lists up to a given length of elements from a set, e.g.
--
-- >>> replicateSetUntil 3 atoms
-- {[], [a₁] : for a₁ ∊ 𝔸, [a₁,a₂] : for a₁,a₂ ∊ 𝔸, [a₁,a₂,a₃] : for a₁,a₂,a₃ ∊ 𝔸}
replicateSetUntil :: NominalType a => Int -> Set a -> Set [a]
replicateSetUntil n s = unions $ fmap (`replicateSet` s) [0..n]

-- | Creates a set of lists up to a given length of different elements from a set, e.g.
--
-- >>> replicateDifferentSetUntil 3 atoms
-- {[], [a₁] : for a₁ ∊ 𝔸, [a₁,a₂] : a₁ ≠ a₂ for a₁,a₂ ∊ 𝔸, [a₁,a₂,a₃] : a₁ ≠ a₂ ∧ a₁ ≠ a₃ ∧ a₂ ≠ a₃ for a₁,a₂,a₃ ∊ 𝔸}
replicateDifferentSetUntil :: NominalType a => Int -> Set a -> Set [a]
replicateDifferentSetUntil n s = unions $ fmap (`replicateDifferentSet` s) [0..n]

-- |
-- > replicateAtoms n = replicateSet n atoms
replicateAtoms :: Int -> Set [Atom]
replicateAtoms n = replicateSet n atoms

-- |
-- > replicateDifferentAtoms n = replicateDifferentSet n atoms
replicateDifferentAtoms :: Int -> Set [Atom]
replicateDifferentAtoms n = replicateDifferentSet n atoms

-- |
-- > replicateAtomsUntil n = replicateSetUntil n atoms
replicateAtomsUntil :: Int -> Set [Atom]
replicateAtomsUntil n = replicateSetUntil n atoms

-- |
-- > replicateDifferentAtomsUntil n = replicateDifferentSetUntil n atoms
replicateDifferentAtomsUntil :: Int -> Set [Atom]
replicateDifferentAtomsUntil n = replicateDifferentSetUntil n atoms

----------------------------------------------------------------------------------------------------
-- Size of the set
----------------------------------------------------------------------------------------------------

-- | Returns a formula describing condition that a set has a size less than a given number.
-- It is an inefficient function for large sets and will not return the answer for the infinite sets.
hasSizeLessThan :: NominalType a => Set a -> Int -> Formula
hasSizeLessThan s n = forAll id $ mapList (\xs -> let l = length xs in or [eq (xs!!i) (xs!!j) | i <- [0..l-1], j <- [0..l-1], i<j]) (replicate n s)

-- | Returns a formula describing condition that a set has a given size.
-- It is an inefficient function for large sets and will not return the answer for the infinite sets.
hasSize :: NominalType a => Set a -> Int -> Formula
hasSize s n = hasSizeLessThan s (succ n) /\ not (hasSizeLessThan s n)

-- | Returns a formula describing condition that a set has exacly one element.
isSingleton :: NominalType a => Set a -> Formula
isSingleton s = hasSize s 1

-- | Returns a variants of numbers of the size of a list with given equality relation.
listSizeWith :: (a -> a -> Formula) -> [a] -> Variants Int
listSizeWith eq = simplify . go
    where go [] = variant 0
          go (e:l) = let s = go l in ite (or $ fmap (eq e) l) s (V.map (+1) s)

-- | Returns a variants of numbers of the size of a list.
listSize :: NominalType a => [a] -> Variants Int
listSize = listSizeWith eq

-- | Returns the maximum size of a list for all free atoms constraints with given equality relation.
listMaxSizeWith :: (a -> a -> Formula) -> [a] -> Int
listMaxSizeWith _ [] = 0
listMaxSizeWith eq (e:l) = let s = listMaxSizeWith eq l in if isTrue (or $ fmap (eq e) l) then s else s+1

-- | Returns the maximum size of a list for all free atoms constraints.
listMaxSize :: NominalType a => [a] -> Int
listMaxSize = listMaxSizeWith eq

-- | Returns a list of possible values in the set or 'Nothing' if set has infinite number of values.
setValues :: (Contextual a, NominalType a) => Set a -> Maybe [a]
setValues s = if P.any Maybe.isNothing values
              then Nothing
              else Just $ concatMap Maybe.fromJust values
    where values = fmap elemValues $ Map.assocs $ setElements s

-- | Returns a list of possible values for set element or 'Nothing' if set element has infinite number of values.
elemValues :: (Contextual a, NominalType a) => (a, SetElementCondition) -> Maybe [a]
elemValues (v, (vs, c)) = if all (Set.null . Set.intersection vs . freeVariables) values
                          then Just values
                          else Nothing
    where vars = Set.elems $ Set.union vs (freeVariables v)
          conds = exclusiveConditions vars
          values = fmap (`when` v) $ P.filter (/= false) $ fmap (simplifyFormula . (c /\)) conds

-- | Returns a variants of numbers of the size of a set with given equality relation.
-- It will not return the answer for the infinite sets.
sizeWith :: (Contextual a, NominalType a) => (a -> a -> Formula) -> Set a -> Variants Int
sizeWith eq s = Maybe.maybe (findSize s 1) (listSizeWith eq) (setValues s)
    where findSize s n = ite (hasSizeLessThan s n) (variant $ pred n) (findSize s (succ n))

-- | Returns a variants of numbers of the size of a set.
-- It is an inefficient function for large sets and will not return the answer for the infinite sets.
size :: (Contextual a, NominalType a) => Set a -> Variants Int
size = sizeWith eq

-- | Returns the maximum size of a set for all free atoms constraints with given equality relation.
-- It will not return the answer for the infinite sets.
maxSizeWith :: (Contextual a, NominalType a) => (a -> a -> Formula) -> Set a -> Int
maxSizeWith eq s = Maybe.maybe (findSize s 1) (listMaxSizeWith eq) (setValues s)
    where findSize s n = if isTrue (hasSizeLessThan s n) then pred n else findSize s (succ n)

-- | Returns the maximum size of a set for all free atoms constraints.
-- It is an inefficient function for large sets and will not return the answer for the infinite sets.
maxSize :: (Contextual a, NominalType a) => Set a -> Int
maxSize = maxSizeWith eq

----------------------------------------------------------------------------------------------------
-- Finding set element
----------------------------------------------------------------------------------------------------

-- | Returns some given element of a set or 'Nothing' if the set is empty.
-- The function report error if the set has condition with constraint between free variable and iteration variable
element :: (Contextual a, NominalType a) => Set a -> NominalMaybe a
element s
    | null notEmpty = nothing
    | P.not $ null bad = error "Cannot get element from set with constraint between free variable and iteration variable"
    | otherwise = ite condition (V.fromList $ first Just <$> variants) nothing
    where notEmpty = Map.assocs $ Map.filter (P.not . isFalse . snd) (setElements s)
          withConstraints = fmap (\(v, (vs, c)) -> (v, vs, c, getConstraintsFromFormula c)) notEmpty
          (good, bad) = List.partition (\(v, vs, c, cs) -> P.all (checkConstraint vs) cs) withConstraints
          checkConstraint vs (r, x1, x2) = isConstant x1 || isConstant x2 || (Set.member x1 vs == Set.member x2 vs)
          getResult [] cond = ([], cond)
          getResult ((v, vs, c, cs):rest) cond = let variants = getVariants v vs (not cond /\ c) (freeVars vs cs)
                                                     newCond = cond \/ or (snd <$> variants)
                                                     (restVariants, finalCond) = getResult rest newCond
                                                 in  if isTrue newCond
                                                     then (variants, true)
                                                     else (variants ++ restVariants, finalCond)
          freeVars vs cs = Set.elems $ Set.fromList (concatMap (\(r, x1, x2) -> [x1, x2]) cs) Set.\\ vs
          getVariants val vs cond free = Maybe.mapMaybe (\ctx -> let c = ctx /\ cond
                                                                 in if isFalse c
                                                                    then Nothing
                                                                    else Just $ getElemValue val vs c) (Set.toList $ Set.fromList $ exclusiveConditions free)
          getModel cond vs = Map.filterWithKey (\x -> const $ Set.member x vs) (model cond)
                             `Map.union`
                             Map.fromList ((\x -> (x, constantVar defaultConstant)) <$> Set.elems vs)
          getElemValue val vs cond = simplify $ replaceVariables (getModel cond vs) (val, cond)
          (variants, condition) = getResult good false

-- | Converts set without conditions and iteration variables to list
toList :: Set a -> [a]
toList (Set es) = go $ Map.assocs es
    where go [] = []
          go ((v, (vs, c)):rest) = if Set.null vs && c == true
                                   then v : go rest
                                   else error "Cannot convert set with condition or iteration variables to list"

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
isLowerBound a = forAll (le a)

-- | Checks whether a set has the lower bound.
hasLowerBound :: Set Atom -> Formula
hasLowerBound s = exists (`isLowerBound` s) atoms

-- | Checks whether a given atom is the upper bound of a set.
isUpperBound :: Atom -> Set Atom -> Formula
isUpperBound a = forAll (ge a)

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
