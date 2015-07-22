module Nominal.Formula.Instances where

import Data.List.Utils (join)
import Data.Set (elems, size)
import Nominal.Formula.Definition

----------------------------------------------------------------------------------------------------
-- Formula instances
----------------------------------------------------------------------------------------------------

-- Show

showFormula :: Formula -> String
showFormula f@(And fs) = "(" ++ show f ++ ")"
showFormula f@(Or fs) = "(" ++ show f ++ ")"
showFormula (Exists x f) = "∃" ++ show x ++ "(" ++ show f ++ ")"
showFormula (ForAll x f) = "∀" ++ show x ++ "(" ++ show f ++ ")"
showFormula f = show f

instance Show Formula where
    show T = "true"
    show F = "false"
    show (Constraint r x1 x2) = show x1 ++ " " ++ show r ++ " " ++ show x2
    show (And fs) = join " ∧ " $ fmap showFormula $ elems fs
    show (Or fs) = join " ∨ " $ fmap showFormula $ elems fs
    show (Not f) = "¬(" ++ show f ++ ")"
    show (Exists x f) = "∃" ++ show x ++ " " ++ show f
    show (ForAll x f) = "∀" ++ show x ++ " " ++ show f

-- Ord

compareEquivalentPairs :: (Ord a) => (a, a) -> (a, a) -> Ordering
compareEquivalentPairs (x11, x12) (x21, x22) =
    compareSortedPairs
        (if x11 <= x12 then (x11, x12) else (x12, x11))
        (if x21 <= x22 then (x21, x22) else (x22, x21))

compareSortedPairs :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareSortedPairs (x11, x12) (x21, x22) =
    let compareFirst = compare x11 x21
    in if compareFirst == EQ
         then compare x12 x22
         else compareFirst

symmetricRelation :: Relation -> Relation
symmetricRelation LessThan = GreaterThan
symmetricRelation LessEquals = GreaterEquals
symmetricRelation GreaterThan = LessThan
symmetricRelation GreaterEquals = LessEquals
symmetricRelation Equals = Equals
symmetricRelation NotEquals = NotEquals

symmetricRelations :: Relation -> Relation -> Bool
symmetricRelations r1 r2 = r1 == symmetricRelation r2

instance Ord Formula where
    compare T T = EQ
    compare T _ = GT
    compare _ T = LT

    compare F F = EQ
    compare F _ = GT
    compare _ F = LT

    compare (Constraint r1 x1 y1) (Constraint r2 x2 y2)
        | r1 == r2 && (r1 == Equals || r1 == NotEquals) = compareEquivalentPairs (x1, y1) (x2, y2)
    compare (Constraint r1 x1 y1) (Constraint r2 x2 y2) = if r1 == r2
                                                            then compareSortedPairs (x1, y1) (x2, y2)
                                                            else if symmetricRelations r1 r2
                                                                   then compareSortedPairs (x1, y1) (y2, x1)
                                                                   else compare r1 r2
    compare (Constraint _ _ _) _ = GT
    compare _ (Constraint _ _ _) = LT

    compare (And fs1) (And fs2) = compare fs1 fs2
    compare (And _) _ = GT
    compare _ (And _) = LT

    compare (Or fs1) (Or fs2) = compare fs1 fs2
    compare (Or _) _ = GT
    compare _ (Or _) = LT

    compare (Not f1) (Not f2) = compare f1 f2
    compare (Not _) _ = GT
    compare _ (Not _) = LT

    compare (Exists x1 f1) (Exists x2 f2) =  compareSortedPairs (x1, f1) (x2, f2)
    compare (Exists _ _) _ = GT
    compare _ (Exists _ _) = LT

    compare (ForAll x1 f1) (ForAll x2 f2) =  compareSortedPairs (x1, f1) (x2, f2)

-- Eq

instance Eq Formula where
    f1 == f2 = (compare f1 f2) == EQ

