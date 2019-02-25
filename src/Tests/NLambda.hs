{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
module Tests.NLambda where

import Data.Map (Map)
import NLambda hiding (toList)
import Nominal.Atoms.Logic (exclusiveConditions)
import Nominal.Variable (iterationVariableWithId)
import Nominal.Variants (toList)
import qualified Prelude as P
import Prelude hiding (or, and, not, sum, map, filter, maybe)

[x,y,z] = fmap variable ["x", "y", "z"]
[a,b,c,d,e,f] = fmap atom ["a", "b", "c", "d", "e", "f"]

formulas :: [Formula]
formulas = [lt a b /\ lt b c /\ lt c a, lt a b /\ lt b c /\ lt a c, eq a b /\ eq b c /\ eq a c, eq a b /\ eq b c /\ neq a c]

----------------------------------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------------------------------

test1 :: [Formula]
test1 = [true, false, fromBool True, fromBool False,
         true /\ true, true /\ false, false /\ true, false /\ false, true \/ true, true \/ false, false \/ true, false \/ false,
         true ==> true, true ==> false, false ==> true, false ==> false, true <== true, true <== false, false <== true, false <== false,
         true <==> true, true <==> false, false <==> true, false <==> false]

test2 :: [Atom]
test2 = [a,b,c,d,e,f, constant 1, constant (1/2)]

test3 :: [Formula]
test3 = [eq a a, eq a b, eq b a, eq b b, lt c c, lt c d, lt d c, le d d, le c c, le c d, le d c, le d d,
         gt e e, gt e f, gt f e, ge f f, ge e e, ge e f, ge f e, ge f f]

test4 :: [Formula]
test4 = fmap simplifyFormula formulas

test5 :: [Bool]
test5 = fmap isTrue formulas ++ fmap isFalse formulas

test6 :: [Maybe Bool]
test6 = fmap solve formulas

test7 :: [Map Variable Variable]
test7 = fmap model $ P.filter (P.not . isFalse) formulas

test8 :: [Formula]
test8 = [(∃) x (atom "x" `eq` constant 0), (∀) x (atom "x" `eq` constant 0)]

test9 :: [Formula]
test9 = exclusiveConditions [x, y, z]

test10 :: [Atom]
test10 = ite (eq a b) id (const c) d : fmap (\f -> ite f a b) formulas

test11 :: [Formula]
test11 = fmap simplify formulas

test12 :: [Formula]
test12 = fmap (when $ eq a b) formulas

test13 :: [Variants Int]
test13 = [iteV (eq a b) 1 2, variant 3]

test14 :: ()
test14 = fromVariant $ variant ()

test15 :: [NominalEither Atom Atom]
test15 = [right a, left b]

test16 :: [Variants Atom]
test16 = fmap fromEither test15

test17 :: [Formula]
test17 = fmap isLeft test15 ++ fmap isRight test15

test18 :: [NominalMaybe Atom]
test18 = [nothing, just a]

test19 :: [Variants Atom]
test19 = [fromJust $ just a, maybe a id $ just a]

test20 :: [Formula]
test20 = fmap isJust test18 ++ fmap isNothing test18

test21 :: [Formula]
test21 = [eq (1::Int) 1, eq ["a"] ["b"], eq (just a) (just a), neq (a,b,c) (c,b,a), eq true false, eq a b, neq (left a) (right a)]

test22 :: [Int]
test22 = [f a, f (1::Int), f true, f [a,b,c], f $ just d, f $ ite (eq a b) (left a) (right b)]
    where f :: NominalType a => a -> Int
          f = length . toList . variants

test23 :: [Set Atom]
test23 = [empty, atoms, insert a empty, insert b empty, insert b $ insert a empty, insert a atoms, delete a empty, delete a atoms]

test24 :: [Formula]
test24 = fmap isEmpty test23 ++ fmap isNotEmpty test23

test25 :: [Set Atom]
test25 = [map id empty, map id atoms, map (const a) atoms]

test26 :: Set (Set (Atom, Atom))
test26 = map (\x -> map (\y -> (x,y)) atoms) atoms

test27 :: [Set Atom]
test27 = [filter (eq a) atoms, filter (neq a) atoms, filter (lt a) atoms]

test28 :: [Set Atom]
test28 = [singleton a, insertAll [a,b,c] atoms, fromList [a,b,c], deleteAll [a,b,c] atoms]

test29 :: [Formula]
test29 = [exists (eq a) atoms, exists (neq a) atoms, exists (eq a) empty, exists (neq a) empty,
          forAll (eq a) atoms, forAll (neq a) atoms, forAll (eq a) empty, forAll (neq a) empty, forAll (eq a) (singleton a)]

test30 :: [Set Atom]
test30 = [mapFilter just atoms, mapFilter (const nothing) atoms, mapFilter (\x -> maybeIf (eq a x) x) atoms]

test31 :: [(Set Atom, Set Atom)]
test31 = [partition (eq a) atoms, partition (lt a) atoms]

test32 :: [Set Atom]
test32 = [union empty atoms, union (singleton a) (singleton b), union atoms atoms, union (singleton a) atoms]

test33 :: [Formula]
test33 = [contains empty a, contains atoms a, notContains atoms a, contains (delete a atoms) a, notContains (delete a atoms) a]

test34 :: [Formula]
test34 = [member a empty, member a atoms, notMember a atoms, member a $ delete a atoms, notMember a $ delete a atoms]

test35 :: [Formula]
test35 = [isSubsetOf empty (empty::Set Atom), isSubsetOf empty atoms, isSubsetOf atoms empty, isSubsetOf atoms atoms,
          isNotSubsetOf empty (empty::Set Atom), isNotSubsetOf empty atoms, isNotSubsetOf atoms empty, isNotSubsetOf atoms atoms]

test36 :: [Formula]
test36 = [isProperSubsetOf empty (empty::Set Atom), isProperSubsetOf empty atoms, isProperSubsetOf atoms empty, isProperSubsetOf atoms atoms,
          isNotProperSubsetOf empty (empty::Set Atom), isNotProperSubsetOf empty atoms, isNotProperSubsetOf atoms empty, isNotProperSubsetOf atoms atoms]

test37 :: [Set Atom]
test37 = [intersection atoms empty, intersection atoms atoms, intersection (filter (neq a) atoms) (filter (neq b) atoms),
          difference atoms empty, difference atoms atoms, difference (filter (neq a) atoms) (filter (neq b) atoms),
          atoms \\ empty, atoms \\ atoms, (filter (neq a) atoms) \\ (filter (neq b) atoms)]

test38 :: [Formula]
test38 = [intersect atoms empty, intersect atoms atoms, intersect (filter (neq a) atoms) (filter (neq b) atoms),
          disjoint atoms empty, disjoint atoms atoms, disjoint (filter (neq a) atoms) (filter (neq b) atoms)]

test39 :: [Set (Atom, Atom)]
test39 = [pairs atoms atoms, pairsWith (,) atoms empty, atomsPairs, square atoms, differentAtomsPairs, pairsWithFilter (curry just) atoms atoms]

test40 :: [Set (Atom, Atom, Atom)]
test40 = [triples atoms atoms atoms, triplesWith (,,) atoms empty atoms, atomsTriples, triplesWithFilter (\x y z -> just (x, y, z)) atoms atoms atoms]

test41 :: [Set [Atom]]
test41 = [mapList id [atoms, fromList [a,b]], mapFilterList (\l -> maybeIf (eq a $ head l) l) [atoms, atoms]]
