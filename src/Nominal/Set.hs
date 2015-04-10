module Nominal.Set where

import Data.List.Utils (join)
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Nominal.Conditional
import Nominal.Formula
import Nominal.Maybe
import Nominal.Type (NominalType(..), collectWith, mapVariablesIf, replaceVariables)
import qualified Nominal.Util.InsertionSet as ISet
import Nominal.Variable (Timestamp, Variable, changeIterationLevel, clearTimestamp, getIterationLevel,
                         getTimestampNotEqual, hasNoTimestamp, hasTimestampEquals, iterationVariablesList,
                         iterationVariable, setIterationLevel, setTimestamp, variableName)
import Nominal.Variants (Atom, Variants, atom, fromVariant, toList, variant)
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
checkVariablesInElement (v, _) | v `seq` False = undefined
checkVariablesInElement (v, (vs, c)) =
    let iterVars = foldVariables (\var -> if Set.member var vs then ISet.insert var else id) ISet.empty v
        formulaVars = foldVariables Set.insert Set.empty c
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
filterNotFalse = Map.filter ((/= F) . snd)

----------------------------------------------------------------------------------------------------
-- Timestamps
----------------------------------------------------------------------------------------------------

checkTimestamps :: NominalType a => Timestamp -> a -> SetElementCondition-> (a, SetElementCondition)
checkTimestamps t v cond = let newLevel = Set.size $ collectWith (getTimestampNotEqual t) v
                               iterVarsNames = Set.map variableName $ fst cond
                               v' = mapVariablesIf (\var -> hasNoTimestamp var && Set.member (variableName var) iterVarsNames)
                                                   (changeIterationLevel succ) v
                           in mapVariablesIf (hasTimestampEquals t) (clearTimestamp . setIterationLevel newLevel) (v', cond)

applyWithTimestamps :: (NominalType a, NominalType b) => (a -> b) -> a -> SetElementCondition -> [(b, SetElementCondition)]
applyWithTimestamps f v cond = let t = unsafePerformIO getPOSIXTime
                                   (v', cond') = mapVariablesIf (flip Set.member $ fst cond) (setTimestamp t) (v, cond)
                               in fmap (\(v'', c) -> checkTimestamps t v'' (fmap (/\ c) cond')) (toList $ variants $ f v')

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

instance NominalType a => Conditional (Set a) where
    iF c s1 s2 = union (filter (const c) s1) (filter (const $ not c) s2)

instance NominalType a => NominalType (Set a) where
    eq s1 s2 = (isSubsetOf s1 s2) /\ (isSubsetOf s2 s1)
    mapVariables f (Set es) = Set $ Map.fromListWith sumCondition $ mapVariables f $ Map.assocs es
    foldVariables fun acc (Set es) = foldVariables fun acc $ Map.assocs es
    simplify (Set es) = Set $ Map.fromListWith sumCondition $ simplify $ Map.assocs es

----------------------------------------------------------------------------------------------------
-- Similar instances
----------------------------------------------------------------------------------------------------

instance NominalType a => NominalType (Set.Set a) where
    eq s1 s2 = eq (fromList $ Set.elems s1) (fromList $ Set.elems s2)
    mapVariables f = Set.map (mapVariables f)
    foldVariables fun acc = foldVariables fun acc . Set.elems
    simplify = Set.map simplify

instance (NominalType k, NominalType a) => NominalType (Map k a) where
    eq m1 m2 = eq (fromList $ Map.assocs m1) (fromList $ Map.assocs m2)
    mapVariables f = Map.fromList . mapVariables f . Map.assocs
    foldVariables fun acc = foldVariables fun acc . Map.assocs
    simplify = Map.fromList . simplify . Map.assocs

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
map f = Set . checkVariables
            . Map.fromListWith sumCondition
            . Map.foldrWithKey (mapAndMerge f) []
            . setElements
    where mapAndMerge f v cond rs = (applyWithTimestamps f v cond) ++ rs

filter :: NominalType a => (a -> Formula) -> Set a -> Set a
filter f = Set . filterNotFalse
               . Map.fromListWith sumCondition
               . Map.foldrWithKey (filterAndMerge f) []
               . setElements
    where filterAndMerge f v cond rs = fmap (\(c, cond') -> (v, (fst cond', c /\ snd cond'))) (applyWithTimestamps f v cond) ++ rs

sum :: NominalType a => Set (Set a) -> Set a
sum = Set . checkVariables
          . Map.unionsWith sumCondition . fmap filterSetInSet . Map.assocs . setElements
    where filterSetInSet (elemSet, elemSetCond) = filterSetElems (const elemSetCond) (setElements elemSet)

atoms :: Set Atom
atoms = let iterVar = iterationVariable 0 1
        in Set $ Map.singleton (variant iterVar) (Set.singleton iterVar, T)

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

forall :: NominalType a => (a -> Formula) -> Set a -> Formula
forall f = isEmpty . (filter $ \x -> not (f x))

union :: NominalType a => Set a -> Set a -> Set a
union s1 s2 = sum (insert s1 (singleton s2))

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
pairs = pairsWith (,)

pairsWith :: (NominalType a, NominalType b, NominalType c) => (a -> b -> c) -> Set a -> Set b -> Set c
pairsWith f s1 s2 = sum $ map (\e1 -> map (f e1) s2) s1

squared :: NominalType a => Set a -> Set (a, a)
squared s = pairs s s

atomsPairs :: Set (Atom, Atom)
atomsPairs  = squared atoms

triples :: (NominalType a, NominalType b, NominalType c) => Set a -> Set b -> Set c -> Set (a, b, c)
triples = triplesWith (,,)

triplesWith :: (NominalType a, NominalType b, NominalType c, NominalType d)
               => (a -> b -> c -> d) -> Set a -> Set b -> Set c -> Set d
triplesWith f s1 s2 s3 = sum $ sum $ map (\e1 -> map (\e2 -> map (f e1 e2) s3) s2) s1

atomsTriples :: Set (Atom, Atom, Atom)
atomsTriples = triples atoms atoms atoms

fromFunction :: (NominalType a, NominalType b) => (a -> b) -> Set a -> Set (a,b)
fromFunction f s = map (\x -> (x, f x)) s
