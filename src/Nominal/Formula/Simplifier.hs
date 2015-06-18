module Nominal.Formula.Simplifier where

import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set hiding (foldr)
import Nominal.Formula.Definition
import Nominal.Formula.Instances
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
checkEquationsInAnd fs = union (fromMap eqMap) (map (replaceFormulaVariables eqMap) fs2)
    where (fs1, fs2) = partition isEquation fs
          eqMap = foldl findEquationsMap Map.empty fs1
          findEquationsMap eqMap (Constraint Equals x1 x2) =
            let x = min (Map.findWithDefault x1 x1 eqMap) (Map.findWithDefault x1 x2 eqMap)
            in Map.insert x1 x $ Map.insert x2 x eqMap
          fromMap = fromList . fmap (\(x1, x2) -> simplifyConstraint Equals x2 x1) . Map.assocs

------------------------------------------------------
---- (x >= y) /\ (x <= y) ~~> x = y              -----
---- (x < y) /\ (x <= y)  ~~> x < y              -----
---- (x > y) /\ (x < y)   ~~> False         etc. -----
------------------------------------------------------

andRelations :: Relation -> Relation -> Maybe Relation
andRelations r1 r2
    | r1 == r2 = Just r1
    | r1 > r2 = andRelations r2 r1
andRelations LessThan    LessEquals    = Just LessThan
andRelations LessThan    NotEquals     = Just LessThan
andRelations LessEquals  NotEquals     = Just LessThan
andRelations Equals      LessEquals    = Just Equals
andRelations Equals      GreaterEquals = Just Equals
andRelations LessEquals  GreaterEquals = Just Equals
andRelations NotEquals   GreaterEquals = Just GreaterThan
andRelations NotEquals   GreaterThan   = Just GreaterThan
andRelations GreaterThan GreaterEquals = Just GreaterThan
andRelations _           _             = Nothing

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

------------------------------------------------------
---- simplification function                      ----
------------------------------------------------------

andSimplifier :: Set Formula -> Set Formula
andSimplifier = checkBoolInAnd . checkAndPairs . checkAndConstraints . checkEquationsInAnd . andUnion

simplifyAnd :: Set Formula -> Formula
simplifyAnd = checkAndSize . findFixedPoint andSimplifier

----------------------------------------------------------------------------------------------------
-- Simplify disjunction
----------------------------------------------------------------------------------------------------

------------------------------------------------------
---- f1 \/ (f2 \/ f3) ~~> f1 \/ f2 \/ f3          ----
------------------------------------------------------

fromOr :: Formula -> Set Formula
fromOr (Or fs) = fs
fromOr f = singleton f

orUnion :: Set Formula -> Set Formula
--orUnion fs | trace ("orUnion " ++ show fs) False = undefined
orUnion fs = unions $ fmap fromOr $ elems fs

------------------------------------------------------
---- (x > y) \/ (x < y)  ~~> x /= y              -----
---- (x < y) \/ (x <= y) ~~> x <= y              -----
---- (x > y) \/ (x <= y) ~~> True           etc. -----
------------------------------------------------------

orRelations :: Relation -> Relation -> Maybe Relation
orRelations r1 r2
    | r1 == r2 = Just r1
    | r1 > r2 = orRelations r2 r1
orRelations LessThan      LessEquals    = Just LessEquals
orRelations LessThan      Equals        = Just LessEquals
orRelations LessEquals    Equals        = Just LessEquals
orRelations LessThan      NotEquals     = Just NotEquals
orRelations LessThan      GreaterThan   = Just NotEquals
orRelations NotEquals     GreaterThan   = Just NotEquals
orRelations Equals        GreaterEquals = Just GreaterEquals
orRelations Equals        GreaterThan   = Just GreaterEquals
orRelations GreaterEquals GreaterThan   = Just GreaterEquals
orRelations _             _             = Nothing

orConstraints :: Formula -> Formula -> Formula
orConstraints (Constraint r1 x1 y1) (Constraint r2 x2 y2)
    | x1 == x2 && y1 == y2 = maybe T (\r -> Constraint r x1 y1) (orRelations r1 r2)
orConstraints F f = f
orConstraints f F = f

checkOrConstraints :: Set Formula -> Set Formula
--checkOrConstraints fs | trace ("checkOrConstraints " ++ show fs) False = undefined
checkOrConstraints fs | not $ null fs1 =
    union (fromList $ fmap (foldr orConstraints F) $ groupBy sameVarsInConstraints $ elems fs1) fs2
    where (fs1, fs2) = partition isConstraint fs
checkOrConstraints fs = fs

------------------------------------------------------
---- f \/ not f \/ ...         ~~> True           ----
---- f \/ (f /\ f') \/ ...     ~~> f \/ ...       ----
---- f \/ (not f /\ f') \/ ... ~~> f \/ f' \/ ... ----
------------------------------------------------------

andContains :: Formula -> Formula -> Bool
andContains (And fs) f = member f fs
andContains _ _ = False

andDelete :: Formula -> Formula -> Formula
andDelete f (And fs) = checkAndSize $ delete f fs

addToOr :: Set Formula -> Formula -> Set Formula
--addToOr fs f | trace ("add " ++ show f ++ " to or " ++ show fs) False = undefined
addToOr fs f | member (simplifyNot f) fs = singleton T
addToOr fs f | not $ null fs1 = addToOr fs2 f
    where (fs1, fs2) = partition (flip andContains f) fs
addToOr fs f | not $ null fs1 = addToOr (union (map (andDelete nf) fs1) fs2) f
    where nf = simplifyNot f
          (fs1, fs2) = partition (flip andContains $ nf) fs
addToOr fs f@(And _) | not $ null fs1 = fs
    where (fs1, fs2) = partition (andContains f) fs
addToOr fs f@(And _) | not $ null fs1 = addToOr fs (andDelete (simplifyNot $ findMin fs1) f)
        where (fs1, fs2) = partition (andContains f . simplifyNot) fs
addToOr fs f = insert f fs

checkOrPairs :: Set Formula -> Set Formula
--checkOrPairs fs | trace ("checkOrPairs " ++ show fs) False = undefined
checkOrPairs fs | null fs = empty
checkOrPairs fs = foldl addToOr (singleton f) fs1
    where (f, fs1) = deleteFindMin fs

------------------------------------------------------
---- True \/ f   ~~> True                         ----
---- False \/ f  ~~> f                            ----
------------------------------------------------------

checkBoolInOr :: Set Formula -> Set Formula
--checkBoolInOr fs | trace ("checkBoolInOr " ++ show fs) False = undefined
checkBoolInOr fs = if member T fs then singleton T else delete F fs

------------------------------------------------------
---- \/ {}  ~~> False                             ----
---- \/ {f}  ~~> f                                ----
---- \/ {f1, f2, ...}  ~~> f1 \/ f2 \/ ...        ----
------------------------------------------------------

checkOrSize :: Set Formula -> Formula
checkOrSize = checkSize F Or

------------------------------------------------------
---- simplification function                      ----
------------------------------------------------------

orSimplifier :: Set Formula -> Set Formula
orSimplifier = checkBoolInOr . checkOrPairs . checkOrConstraints . orUnion

simplifyOr :: Set Formula -> Formula
--simplifyOr fs | trace ("simplifyOr " ++ show fs) False = undefined
simplifyOr fs = checkOrSize $ findFixedPoint orSimplifier fs
--simplifyOr = checkOrSize . findFixedPoint orSimplifier

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
simplifyNot (Or fs) = simplifyAnd $ map simplifyNot fs
simplifyNot (And fs) = simplifyOr $ map simplifyNot fs
simplifyNot (ForAll x f) = simplifyExists x $ simplifyNot f
simplifyNot (Exists x f) = simplifyForAll x $ simplifyNot f

----------------------------------------------------------------------------------------------------
-- Simplify universal quantification
----------------------------------------------------------------------------------------------------

simplifyForAll :: Variable -> Formula -> Formula
--simplifyForAll x f | trace ("forall " ++ show x ++ ": " ++ show f) False = undefined
simplifyForAll _ T = T
simplifyForAll _ F = F
simplifyForAll x f | not $ member x (freeVariables f) = f
simplifyForAll _ (Constraint _ _ _) = F
simplifyForAll x f | not (isQuantificationVariable x) =
    let qv = quantificationVariable $ succ $ getQuantificationLevel f
    in simplifyQuantifiedAndOr $ ForAll qv (replaceFormulaVariable x qv f)
simplifyForAll x f =  simplifyQuantifiedAndOr $ ForAll x f

----------------------------------------------------------------------------------------------------
-- Simplify existential quantification
----------------------------------------------------------------------------------------------------

simplifyExists :: Variable -> Formula -> Formula
--simplifyExists x f | trace ("simplifyExists " ++ show x ++ ": " ++ show f) False = undefined
simplifyExists _ T = T
simplifyExists _ F = F
simplifyExists _ (Constraint _ _ _) = T
simplifyExists x (And fs) | not $ null fs2 =
    simplifyAnd $ fromList [simplifyExists x (checkAndSize fs1), (checkAndSize fs2)]
    where (fs1, fs2) = partition (member x . freeVariables) fs
simplifyExists x (Or fs) | not $ null fs2 =
    simplifyOr $ fromList [simplifyExists x (checkOrSize fs1), (checkOrSize fs2)]
    where (fs1, fs2) = partition (member x . freeVariables) fs
--simplifyExists _ _ | trace ("simplifyExistsCheck") False = undefined
simplifyExists x f | not $ member x (freeVariables f) = f
simplifyExists x f = if isQuantificationVariable x
                     then Exists x f
                     else let qv = quantificationVariable $ succ $ getQuantificationLevel f
                          in Exists qv (replaceFormulaVariable x qv f)

----------------------------------------------------------------------------------------------------
-- Simplify quantified conjunction/disjunction formula
----------------------------------------------------------------------------------------------------

-----------------------------------------------------------
---- exists x (f1 /\ f2(x)) ~~> f1 /\ (exists x f2(x)) ----
---- exists x (f1 \/ f2(x)) ~~> f1 \/ (exists x f2(x)) ----
---- forall x (f1 /\ f2(x)) ~~> f1 /\ (forall x f2(x)) ----
---- forall x (f1 \/ f2(x)) ~~> f1 \/ (forall x f2(x)) ----
-----------------------------------------------------------

simplifyQuantifiedAndOr' :: (Variable -> Formula -> Formula) -> Variable
                            -> (Set Formula -> Formula) -> Set Formula -> Formula
simplifyQuantifiedAndOr' qf x lf fs | not (null fs1 || null fs2) =
    lf $ fromList [qf x (checkSize undefined lf fs1), (checkSize undefined lf fs2)]
    where (fs1, fs2) = partition (member x . freeVariables) fs
simplifyQuantifiedAndOr' qf x lf fs = qf x (lf fs)

simplifyQuantifiedAndOr :: Formula -> Formula
--simplifyQuantifiedAndOr f | trace ("simplifyQuantifiedAndOr " ++ show f) False = undefined
simplifyQuantifiedAndOr (Exists x (And fs)) = simplifyQuantifiedAndOr' simplifyExists x simplifyAnd fs
simplifyQuantifiedAndOr (Exists x (Or fs)) = simplifyQuantifiedAndOr' simplifyExists x simplifyOr fs
simplifyQuantifiedAndOr (ForAll x (And fs)) = simplifyQuantifiedAndOr' simplifyForAll x simplifyAnd fs
simplifyQuantifiedAndOr (ForAll x (Or fs)) = simplifyQuantifiedAndOr' simplifyForAll x simplifyOr fs
simplifyQuantifiedAndOr f = f

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
foldFormulaVariables fun acc (ForAll x f) = foldFormulaVariables fun (fun x acc) f
foldFormulaVariables fun acc (Exists x f) = foldFormulaVariables fun (fun x acc) f

mapFormulaVariables :: (Variable -> Variable) -> Formula -> Formula
mapFormulaVariables _ T = T
mapFormulaVariables _ F = F
mapFormulaVariables fun (Constraint r x1 x2) = simplifyConstraint r (fun x1) (fun x2)
mapFormulaVariables fun (And fs) = simplifyAnd $ map (mapFormulaVariables fun) fs
mapFormulaVariables fun (Or fs) = simplifyOr $ map (mapFormulaVariables fun) fs
mapFormulaVariables fun (Not f) = simplifyNot $ mapFormulaVariables fun f
mapFormulaVariables fun (ForAll x f) = simplifyForAll (fun x) (mapFormulaVariables fun f)
mapFormulaVariables fun (Exists x f) = simplifyExists (fun x) (mapFormulaVariables fun f)

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
getQuantificationLevel (ForAll x f) = succ $ getQuantificationLevel f
getQuantificationLevel (Exists x f) = succ $ getQuantificationLevel f

freeVariables :: Formula -> Set Variable
freeVariables T = empty
freeVariables F = empty
freeVariables (Constraint _ x1 x2) = fromList [x1, x2]
freeVariables (And fs) = unions $ fmap freeVariables $ elems fs
freeVariables (Or fs) = unions $ fmap freeVariables $ elems fs
freeVariables (Not f) = freeVariables f
freeVariables (ForAll x f) = delete x (freeVariables f)
freeVariables (Exists x f) = delete x (freeVariables f)

