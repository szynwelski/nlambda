{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
module Tests.NLambda where

import Data.Map (Map)
import NLambda
import Nominal.Atoms.Logic (exclusiveConditions)
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
test2 = [a,b,c,d,e,f]

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
test10 = fmap (\f -> ite f a b) formulas

test11 :: [Formula]
test11 = fmap simplify formulas

test12 :: [Formula]
test12 = fmap (when $ eq a b) formulas
