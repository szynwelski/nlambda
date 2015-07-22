module Nominal.Formula.Simplifier where

import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set hiding (foldr, filter)
import Nominal.Formula.Definition
import Nominal.Formula.Instances
import qualified Nominal.Util.UnionFind as UF
import Nominal.Variable (Variable, isQuantificationVariable, quantificationVariable)
import Prelude hiding (foldl, map, null)

--import Debug.Trace

----------------------------------------------------------------------------------------------------
-- Simplify constraint
----------------------------------------------------------------------------------------------------

simplifyConstraint :: Relation -> Variable -> Variable -> Formula
--simplifyConstraint r x1 x2 | trace ("simplifyConstraint " ++ show r ++ " " ++ show x1 ++ " " ++ show x2) False = undefined
simplifyConstraint r x1 x2
    | x1 == x2 = if r == LessThan || r == GreaterThan || r == NotEquals then F else T
    | x1 > x2 = Constraint (symmetricRelation r) x2 x1
    | otherwise = Constraint r x1 x2

----------------------------------------------------------------------------------------------------
-- Simplify conjunction
----------------------------------------------------------------------------------------------------

------------------------------------------------------
---- f1 /\ (f2 /\ f3) ~~> f1 /\ f2 /\ f3          ----
------------------------------------------------------

fromAnd :: Formula -> Set Formula
fromAnd (And fs) = fs
fromAnd f = singleton f

andUnion :: Set Formula -> Set Formula
--andUnion fs | trace ("andUnion " ++ show fs) False = undefined
andUnion fs = unions $ fmap fromAnd $ elems fs

------------------------------------------------------
---- (x = y) /\ f ~~> (x = y) /\ f[y:=x]          ----
------------------------------------------------------

isEquation :: Formula -> Bool
isEquation (Constraint Equals _ _) = True
isEquation _ = False

checkEquationsInAnd :: Set Formula -> Set Formula
--checkEquationsInAnd fs | trace ("checkEquationsInAnd " ++ show fs) False = undefined
checkEquationsInAnd fs | not $ null fs1 = union eqs (map (replaceFormulaVariables eqMap) fs2)
    where (fs1, fs2) = partition isEquation fs
          pairs = UF.assocs $ foldl (\uf (Constraint Equals x1 x2) -> UF.union x1 x2 uf) UF.empty fs1
          eqs = fromList $ fmap (\(x1, x2) -> simplifyConstraint Equals x1 x2) $ pairs
          eqMap = Map.fromList pairs
checkEquationsInAnd fs = fs

------------------------------------------------------
---- (x >= y) /\ (x <= y) ~~> x = y              -----
---- (x < y) /\ (x <= y)  ~~> x < y              -----
---- (x > y) /\ (x < y)   ~~> False         etc. -----
------------------------------------------------------

andRelations :: Relation -> Relation -> Maybe Relation
andRelations r1 r2
    | r1 == r2 = Just r1
    | r1 > r2 = andRelations r2 r1
andRelations LessThan      LessEquals    = Just LessThan
andRelations LessThan      NotEquals     = Just LessThan
andRelations LessEquals    NotEquals     = Just LessThan
andRelations Equals        LessEquals    = Just Equals
andRelations Equals        GreaterEquals = Just Equals
andRelations LessEquals    GreaterEquals = Just Equals
andRelations NotEquals     GreaterEquals = Just GreaterThan
andRelations NotEquals     GreaterThan   = Just GreaterThan
andRelations GreaterEquals GreaterThan   = Just GreaterThan
andRelations _             _             = Nothing

andConstraints :: Formula -> Formula -> Formula
andConstraints (Constraint r1 x1 y1) (Constraint r2 x2 y2)
    | x1 == x2 && y1 == y2 = maybe F (\r -> Constraint r x1 y1) (andRelations r1 r2)
andConstraints T f = f
andConstraints f T = f

sameVarsInConstraints :: Formula -> Formula -> Bool
sameVarsInConstraints (Constraint _ x1 x2) (Constraint _ y1 y2) = (x1, x2) == (y1, y2)
sameVarsInConstraints _ _ = False

isConstraint :: Formula -> Bool
isConstraint (Constraint _ _ _) = True
isConstraint _ = False

checkAndConstraints :: Set Formula -> Set Formula
--checkAndConstraints fs | trace ("checkAndConstraints " ++ show fs) False = undefined
checkAndConstraints fs | not $ null fs1 =
    union (fromList $ fmap (foldr andConstraints T) $ groupBy sameVarsInConstraints $ elems fs1) fs2
    where (fs1, fs2) = partition isConstraint fs
checkAndConstraints fs = fs

------------------------------------------------------
---- f /\ not f /\ ...         ~~> False          ----
---- f /\ (f \/ f') /\ ...     ~~> f /\ ...       ----
---- f /\ (not f \/ f') /\ ... ~~> f /\ f' /\ ... ----
------------------------------------------------------

orContains :: Formula -> Formula -> Bool
orContains (Or fs) f = member f fs
orContains _ _ = False

orDelete :: Formula -> Formula -> Formula
orDelete f (Or fs) = checkOrSize $ delete f fs

addToAnd :: Set Formula -> Formula -> Set Formula
--addToAnd fs f | trace ("add " ++ show f ++ " to and " ++ show fs) False = undefined
addToAnd fs f | member (simplifyNot f) fs = singleton F
addToAnd fs f | not $ null fs1 = addToAnd fs2 f
    where (fs1, fs2) = partition (flip orContains f) fs
addToAnd fs f | not $ null fs1 = addToAnd (union (map (orDelete nf) fs1) fs2) f
    where nf = simplifyNot f
          (fs1, fs2) = partition (flip orContains $ nf) fs
addToAnd fs f@(Or _) | not $ null fs1 = fs
    where (fs1, fs2) = partition (orContains f) fs
addToAnd fs f@(Or _) | not $ null fs1 = addToAnd fs (orDelete (simplifyNot $ findMin fs1) f)
        where (fs1, fs2) = partition (orContains f . simplifyNot) fs
addToAnd fs f = insert f fs

checkAndPairs :: Set Formula -> Set Formula
--checkAndPairs fs | trace ("checkAndPairs " ++ show fs) False = undefined
checkAndPairs fs | null fs = empty
checkAndPairs fs = foldl addToAnd (singleton f) fs1
    where (f, fs1) = deleteFindMin fs

------------------------------------------------------
---- True /\ f   ~~> f                            ----
---- False /\ f  ~~> False                        ----
------------------------------------------------------

checkBoolInAnd :: Set Formula -> Set Formula
--checkBoolInAnd fs | trace ("checkBoolInAnd " ++ show fs) False = undefined
checkBoolInAnd fs = if member F fs then singleton F else delete T fs

------------------------------------------------------
---- /\ {}  ~~> True                              ----
---- /\ {f}  ~~> f                                ----
---- /\ {f1, f2, ...}  ~~> f1 /\ f2 /\ ...        ----
------------------------------------------------------

checkSize :: Formula -> (Set Formula -> Formula) -> Set Formula -> Formula
checkSize df lf fs
--    | trace ("checkSize " ++ show fs) False = undefined
    | null fs = df
    | size fs == 1 = findMin fs
    | otherwise = lf fs

checkAndSize :: Set Formula -> Formula
checkAndSize = checkSize T And

checkOrSize :: Set Formula -> Formula
checkOrSize = checkSize F Or

------------------------------------------------------
---- simplification function                      ----
------------------------------------------------------

andSimplifier :: Set Formula -> Set Formula
andSimplifier = checkAndPairs . checkAndConstraints . checkEquationsInAnd . andUnion . checkBoolInAnd

simplifyAnd :: Set Formula -> Formula
--simplifyAnd fs | trace ("simplifyAnd " ++ show fs) False = undefined
simplifyAnd fs = checkAndSize $ findFixedPoint andSimplifier fs
--simplifyAnd = checkAndSize . findFixedPoint andSimplifier

----------------------------------------------------------------------------------------------------
-- Simplify disjunction
----------------------------------------------------------------------------------------------------

simplifyOr :: Set Formula -> Formula
--simplifyOr fs | trace ("simplifyOr " ++ show fs) False = undefined
simplifyOr fs = simplifyNot $ simplifyAnd $ map simplifyNot fs

----------------------------------------------------------------------------------------------------
-- Simplify negation
----------------------------------------------------------------------------------------------------

simplifyNot :: Formula -> Formula
--simplifyNot f | trace ("simplifyNot " ++ show f) False = undefined
simplifyNot T = F
simplifyNot F = T
simplifyNot (Constraint LessThan x1 x2) = Constraint GreaterEquals x1 x2
simplifyNot (Constraint LessEquals x1 x2) = Constraint GreaterThan x1 x2
simplifyNot (Constraint Equals x1 x2) = Constraint NotEquals x1 x2
simplifyNot (Constraint NotEquals x1 x2) = Constraint Equals x1 x2
simplifyNot (Constraint GreaterThan x1 x2) = Constraint LessEquals x1 x2
simplifyNot (Constraint GreaterEquals x1 x2) = Constraint LessThan x1 x2
simplifyNot (Not f) = f
simplifyNot (Or fs) = And $ map simplifyNot fs
simplifyNot (And fs) = Or $ map simplifyNot fs
simplifyNot (Exists x f) = ForAll x $ simplifyNot f
simplifyNot (ForAll x f) = Exists x $ simplifyNot f

----------------------------------------------------------------------------------------------------
-- Simplify existential quantification
----------------------------------------------------------------------------------------------------

-----------------------------------------------------------
---- exists x (f1 /\ f2(x)) ~~> f1 /\ (exists x f2(x)) ----
---- exists x (f1 \/ f2(x)) ~~> f1 \/ (exists x f2(x)) ----
-----------------------------------------------------------

simplifyExistsWithAndOr :: Variable -> Formula -> Formula
--simplifyExistsWithAndOr x f | trace ("simplifyExistsWithAndOr " ++ show x ++ " " ++ show f) False = undefined
simplifyExistsWithAndOr x (And fs) | not (null fs1 || null fs2) = simplifyAnd $ insert (simplifyExists x $ checkAndSize fs1) fs2
    where (fs1, fs2) = partition (member x . freeVariables) fs
simplifyExistsWithAndOr x (Or fs) | not (null fs1 || null fs2) = simplifyOr $ insert (simplifyExists x $ checkOrSize fs1) fs2
    where (fs1, fs2) = partition (member x . freeVariables) fs
simplifyExistsWithAndOr x f = Exists x f


simplifyExists :: Variable -> Formula -> Formula
--simplifyExists x f | trace ("simplifyExists " ++ show x ++ ": " ++ show f) False = undefined
simplifyExists _ T = T
simplifyExists _ F = F
simplifyExists x f | not $ member x (freeVariables f) = f
simplifyExists _ (Constraint _ _ _) = T
simplifyExists x f | not (isQuantificationVariable x) =
    let qv = quantificationVariable $ succ $ getQuantificationLevel f
    in simplifyExistsWithAndOr qv (replaceFormulaVariable x qv f)
simplifyExists x f =  simplifyExistsWithAndOr x f

----------------------------------------------------------------------------------------------------
-- Simplify universal quantification
----------------------------------------------------------------------------------------------------

simplifyForAll :: Variable -> Formula -> Formula
--simplifyForAll x f | trace ("forall " ++ show x ++ ": " ++ show f) False = undefined
simplifyForAll x f = simplifyNot $ simplifyExists x $ simplifyNot f


----------------------------------------------------------------------------------------------------
-- Auxiliary function
----------------------------------------------------------------------------------------------------

findFixedPoint :: (Show a, Eq a) => (a -> a) -> a -> a
--findFixedPoint _ x | trace ("fix point: " ++ show x) False = undefined
findFixedPoint f x = let fx = f x in if fx == x then x else findFixedPoint f fx

foldFormulaVariables :: (Variable -> a -> a) -> a -> Formula -> a
foldFormulaVariables _ acc T = acc
foldFormulaVariables _ acc F = acc
foldFormulaVariables fun acc (Constraint _ x1 x2) = fun x2 $ fun x1 acc
foldFormulaVariables fun acc (And fs) = foldl (foldFormulaVariables fun) acc fs
foldFormulaVariables fun acc (Or fs) = foldl (foldFormulaVariables fun) acc fs
foldFormulaVariables fun acc (Not f) = foldFormulaVariables fun acc f
foldFormulaVariables fun acc (Exists x f) = foldFormulaVariables fun (fun x acc) f
foldFormulaVariables fun acc (ForAll x f) = foldFormulaVariables fun (fun x acc) f

mapFormulaVariables :: (Variable -> Variable) -> Formula -> Formula
mapFormulaVariables _ T = T
mapFormulaVariables _ F = F
mapFormulaVariables fun (Constraint r x1 x2) = simplifyConstraint r (fun x1) (fun x2)
mapFormulaVariables fun (And fs) = simplifyAnd $ map (mapFormulaVariables fun) fs
mapFormulaVariables fun (Or fs) = simplifyOr $ map (mapFormulaVariables fun) fs
mapFormulaVariables fun (Not f) = simplifyNot $ mapFormulaVariables fun f
mapFormulaVariables fun (Exists x f) = simplifyExists (fun x) (mapFormulaVariables fun f)
mapFormulaVariables fun (ForAll x f) = simplifyForAll (fun x) (mapFormulaVariables fun f)

replaceFormulaVariable :: Variable -> Variable -> Formula -> Formula
replaceFormulaVariable oldVar newVar = mapFormulaVariables (\var -> if oldVar == var then newVar else var)

replaceFormulaVariables :: Map Variable Variable -> Formula -> Formula
replaceFormulaVariables vMap = mapFormulaVariables (\var -> Map.findWithDefault var var vMap)

getQuantificationLevel :: Formula -> Int
getQuantificationLevel T = 0
getQuantificationLevel F = 0
getQuantificationLevel (Constraint _ _ _) = 0
getQuantificationLevel (And fs) = maximum $ fmap getQuantificationLevel $ elems fs
getQuantificationLevel (Or fs) = maximum $ fmap getQuantificationLevel $ elems fs
getQuantificationLevel (Not f) = getQuantificationLevel f
getQuantificationLevel (Exists x f) = succ $ getQuantificationLevel f
getQuantificationLevel (ForAll x f) = succ $ getQuantificationLevel f

freeVariables :: Formula -> Set Variable
freeVariables T = empty
freeVariables F = empty
freeVariables (Constraint _ x1 x2) = fromList [x1, x2]
freeVariables (And fs) = unions $ fmap freeVariables $ elems fs
freeVariables (Or fs) = unions $ fmap freeVariables $ elems fs
freeVariables (Not f) = freeVariables f
freeVariables (Exists x f) = delete x (freeVariables f)
freeVariables (ForAll x f) = delete x (freeVariables f)

