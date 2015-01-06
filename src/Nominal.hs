module Nominal where

import Data.List.Utils (join)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Formula
import Formula.Solver (unsafeSolve, solve)
import Prelude hiding (or, and, not, sum, map, filter)

----------------------------------------------------------------------------------------------------
-- Conditional
----------------------------------------------------------------------------------------------------

class Conditional a where
    iF :: Formula -> a -> a -> a

instance Conditional Formula where
    iF f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

instance Conditional b => Conditional (a -> b) where
    iF c f1 f2 = \x -> iF c (f1 x) (f2 x)

instance (Conditional a) => Conditional [a] where
    iF c l1 l2 = zipWith (iF c) l1 l2

instance Conditional () where
    iF c _ _ = ()

instance (Conditional a, Conditional b) => Conditional (a, b) where
    iF c (a1, b1) (a2, b2) = ((iF c a1 a2), (iF c b1 b2))

instance (Conditional a, Conditional b, Conditional c) => Conditional (a, b, c) where
    iF c (a1, b1, c1) (a2, b2, c2) = ((iF c a1 a2), (iF c b1 b2), (iF c c1 c2))

----------------------------------------------------------------------------------------------------
-- Variants
----------------------------------------------------------------------------------------------------

data Variants a = Variants (Map a Formula) deriving (Eq, Ord)

variant :: a -> Variants a
variant x = Variants $ Map.singleton x T

instance Show a => Show (Variants a) where
    show (Variants vs) = join " | " (fmap showVariant $ Map.assocs vs)
      where showVariant (v, c) = show v ++ if c == T then "" else " : " ++ show c

instance FormulaEq a => FormulaEq (Variants a) where
    eq (Variants vs1) (Variants vs2) = or [(eq v1 v2) /\ c1 /\ c2 | (v1, c1) <- Map.assocs vs1,
                                                                    (v2, c2) <- Map.assocs vs2]
    freeVariables (Variants vs) = freeVariables $ Map.keys vs

instance Ord a => Conditional (Variants a) where
    iF c (Variants vs1) (Variants vs2) = Variants $ unionVariants c vs1 vs2
      where filterWith c vs = Map.map (/\ c) vs
            unionVariants c vs1 vs2 = Map.unionWith (\/) (filterWith c vs1) (filterWith (not c) vs2)

iFv :: Ord a => Formula -> a -> a -> Variants a
iFv c x1 x2 = iF c (variant x1) (variant x2)

----------------------------------------------------------------------------------------------------
-- Atom
----------------------------------------------------------------------------------------------------

type Atom = Variants Variable

atom :: String -> Atom
atom = variant . Variable

----------------------------------------------------------------------------------------------------
-- Set elements
----------------------------------------------------------------------------------------------------

type SetElementCondition = (Set.Set Variable, Formula)

getSetElementCondition :: SetElementCondition -> Formula
getSetElementCondition (vs, c) = Set.foldr (∃) c vs

checkVariables :: FormulaEq a => a -> SetElementCondition -> SetElementCondition
checkVariables v (vs, c) = (Set.intersection valueFreeVars vs,
                            getSetElementCondition (existentialCondVars, c))
    where valueFreeVars = freeVariables v
          existentialCondVars = Set.intersection (freeVariables c) (vs Set.\\ valueFreeVars)

mergeSetElements :: FormulaEq a => a -> SetElementCondition -> SetElementCondition -> SetElementCondition
mergeSetElements v (vs1, c1) (vs2, c2) = checkVariables v (Set.union vs1 vs2, c1 \/ c2)


filterNotFalse :: Map a SetElementCondition -> Map a SetElementCondition
filterNotFalse = Map.filter ((/= F) . snd)

filterCondition :: FormulaEq a => SetElementCondition
                                    -> Map a SetElementCondition -> Map a SetElementCondition
filterCondition (vs1, c1) = Map.mapWithKey mapFun
    where mapFun v (vs2, c2) = checkVariables v (Set.union vs1 vs2, c1 /\ c2)

unionSetElements :: (FormulaEq a, Ord a) => Map a SetElementCondition
                             -> Map a SetElementCondition -> Map a SetElementCondition
unionSetElements = Map.unionWithKey mergeSetElements

----------------------------------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------------------------------

data Set a = Set {setElements :: Map a SetElementCondition} deriving (Eq, Ord)

atomsSet :: String -> Set Atom
atomsSet name = Set $ Map.singleton (atom name) (Set.singleton $ Variable name, T)

instance Show a => Show (Set a) where
    show s = "{" ++ (join ", " (fmap showSetElement (Map.assocs $ setElements s))) ++ "}"
      where showSetElement (v, (vs, c)) =
              let formula = if c == T then "" else " " ++ show c
                  variables = if Set.null vs
                                then ""
                                else " for " ++ (join "," (fmap show $ Set.elems vs)) ++ " ∊ 𝔸"
                  condition = formula ++ variables
              in show v ++ (if null condition then "" else " :" ++ condition)

instance (FormulaEq a, Ord a) => FormulaEq (Set a) where
    eq s1 s2 = (isSubsetOf s1 s2) /\ (isSubsetOf s2 s1)
    freeVariables (Set es) = Set.unions $ fmap (\(v, (vs, _)) -> (freeVariables v) Set.\\ vs)
                                               (Map.assocs es)

instance (FormulaEq a, Ord a) => Conditional (Set a) where
    iF f (Set es1) (Set es2) = Set $ filterNotFalse $ unionSetElements
                                       (filterCondition (Set.empty, f) es1)
                                       (filterCondition (Set.empty, not f) es2)

----------------------------------------------------------------------------------------------------
-- nLambda
----------------------------------------------------------------------------------------------------

empty :: Set a
empty = Set Map.empty

isEmpty :: Set a -> Formula
isEmpty s = and (fmap (not . getSetElementCondition) (Map.elems $ setElements s))

insert :: (FormulaEq a, Ord a) => a -> Set a -> Set a
insert e = Set . (Map.insertWithKey mergeSetElements e (Set.empty, T)) . setElements

delete :: (FormulaEq a, Ord a) => a -> Set a -> Set a
delete e = (filter (not . (eq e))) . Set . (Map.delete e) . setElements

map :: (FormulaEq b, Ord b) => (a -> b) -> Set a -> Set b
map f = Set . Map.fromListWithKey mergeSetElements
            . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []
            . setElements

sum :: (FormulaEq a, Ord a) => Set (Set a) -> Set a
sum = Set . filterNotFalse . (foldr unionSetElements Map.empty) . (fmap filterSet) . Map.assocs . setElements
      where filterSet (elemSet, elemSetCond) = filterCondition elemSetCond (setElements elemSet)

----------------------------------------------------------------------------------------------------
-- Additional functions
----------------------------------------------------------------------------------------------------

isNotEmpty :: Set a -> Formula
isNotEmpty = not . isEmpty

singleton :: (FormulaEq a, Ord a) => a -> Set a
singleton e = insert e empty

insertAll :: (FormulaEq a, Ord a) => [a] -> Set a -> Set a
insertAll es s = foldl (flip insert) s es

fromList :: (FormulaEq a, Ord a) => [a] -> Set a
fromList es = insertAll es empty

deleteAll :: (FormulaEq a, Ord a) => [a] -> Set a -> Set a
deleteAll es s = foldl (flip delete) s es

filter :: (FormulaEq a, Ord a) => (a -> Formula) -> Set a -> Set a
filter f s = sum $ map (\x -> (iF (f x) (singleton x) empty)) s

exists :: (FormulaEq a, Ord a) => (a -> Formula) -> Set a -> Formula
exists f = isNotEmpty . (filter f)

forall :: (FormulaEq a, Ord a) => (a -> Formula) -> Set a -> Formula
forall f = isEmpty . (filter $ \x -> not (f x))

union :: (FormulaEq a, Ord a) => Set a -> Set a -> Set a
union s1 s2 = sum (insert s1 (singleton s2 ))

unions :: (FormulaEq a, Ord a) => [Set a] -> Set a
unions = foldl union empty

contains :: (FormulaEq a, Ord a) => Set a -> a -> Formula
contains s e = exists (eq e) s

notContains :: (FormulaEq a, Ord a) => Set a -> a -> Formula
notContains s = not . contains s

isSubsetOf :: (FormulaEq a, Ord a) => Set a -> Set a -> Formula
isSubsetOf s1 s2 = forall (contains s2) s1

isNotSubsetOf :: (FormulaEq a, Ord a) => Set a -> Set a -> Formula
isNotSubsetOf s = not . isSubsetOf s

isProperSubsetOf :: (FormulaEq a, Ord a) => Set a -> Set a -> Formula
isProperSubsetOf s1 s2 = (isSubsetOf s1 s2) /\ (isNotSubsetOf s2 s1)

isNotProperSubsetOf :: (FormulaEq a, Ord a) => Set a -> Set a -> Formula
isNotProperSubsetOf s = not . isProperSubsetOf s

intersection :: (FormulaEq a, Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = filter (contains s1) s2

difference :: (FormulaEq a, Ord a) => Set a -> Set a -> Set a
difference s1 s2 = filter (notContains s1) s2

infixl 9 \\
(\\) :: (FormulaEq a, Ord a) => Set a -> Set a -> Set a
s1 \\ s2 = difference s1 s2

pairs :: (FormulaEq a, Ord a, FormulaEq b, Ord b) => Set a -> Set b -> Set (a, b)
pairs s1 s2 = sum $ map (\e1 -> map (\e2 -> (e1, e2)) s2) s1

triples :: (FormulaEq a, Ord a, FormulaEq b, Ord b, FormulaEq c, Ord c)
           => Set a -> Set b -> Set c -> Set (a, b, c)
triples s1 s2 s3 = sum $ map (\(e1, e2) -> map (\e3 -> (e1, e2, e3)) s3) (pairs s1 s2)

----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------

a = atom "a"
b = atom "b"
c = atom "c"
cond = eq a b
at = iF cond a b
set1 = fromList [a, b]
set2 = singleton at
sa = atomsSet "a"
sb = atomsSet "b"
sc = atomsSet "c"

