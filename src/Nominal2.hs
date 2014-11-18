{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Nominal where

import Prelude hiding (or, and, not, sum, map, filter)
import Formula
import Data.List.Utils (join)


----------------------------------------------------------------------------------------------------

variables = [ c : s | s <- "": variables, c <- ['a'..'z']]

----------------------------------------------------------------------------------------------------

data Cond a = Cond {value :: a, condition :: Formula}

instance Show a => Show (Cond a) where
    show (Cond v c) = show v ++ " : " ++ show c
--    show (Cond v c) = show v ++ (if (isTrue c) then "" else " : " ++ show c)

instance Functor Cond where
    fmap f (Cond v c) = Cond (f v) c

instance Monad Cond where
    return = cond
    b >>= f = condIf (condition b) (f $ value b)

condIf :: Formula -> Cond a -> Cond a
condIf c b = Cond (value b) (condition b /\ c)

cond :: a -> Cond a
cond v = Cond v T

instance FormulaEq a => FormulaEq (Cond a) where
    eq (Cond v1 c1) (Cond v2 c2) = (eq v1 v2) /\ c1 /\ c2

----------------------------------------------------------------------------------------------------

class Alternatives a b | a -> b where
    getAlternatives :: a -> [Cond b]
    createAlternatives :: [Cond b] -> a

alternativesIf :: (Alternatives a b) => a -> Formula -> a
alternativesIf a c = createAlternatives (fmap (condIf c) (getAlternatives a))

showAlternatives :: (Show b, Alternatives a b) => a -> String
showAlternatives as = join " | " (fmap show (getAlternatives as))

----------------------------------------------------------------------------------------------------

data a :-> b = (:->) {functions :: [Cond (a -> b)]}

instance Alternatives (a :-> b) (a -> b) where
    getAlternatives = functions
    createAlternatives = (:->)


--(#) fs xs = createAlternatives (zipWith (\cf cx -> (Cond ((value cf) (value cx)) ((condition cf) /\ (condition cx)))) (functions fs) (getAlternatives xs))

--(#) fs x = createAlternatives (fmap (\cf ->  ((value cf) x)) (functions fs))

--nominalF f = (:->) [cond f]
nominalF f xs = createAlternatives (zipWith (\cf cx -> (Cond ((value cf) (value cx)) ((condition cf) /\ (condition cx)))) [cond f] (getAlternatives xs))

----------------------------------------------------------------------------------------------------
--data NominalAtom = NominalAtom {atoms :: [Cond Atom]}
--
--instance Alternatives NominalAtom Atom where
--    getAlternatives = atoms
--    createAlternatives = NominalAtom
--
--instance Show NominalAtom where
--    show = showAlternatives
--
--nominalA atomName = NominalAtom [cond (Atom atomName)]
------------------------------------------------------------------------------------------------------
--data Set a = Set {elements :: [Cond a]}
--
--instance Show a => Show (Set a) where
--    show s = "{" ++ (join ", " (fmap show (elements s))) ++ "}"
--
--data NominalSet a = NominalSet {sets :: [Cond (Set a)]}
--
--instance Alternatives (NominalSet a) (Set a) where
--    getAlternatives = sets
--    createAlternatives = NominalSet
--
--instance Show a => Show (NominalSet a) where
--    show = showAlternatives
--
--nominalS set = NominalSet [cond set]
--
--emptySet :: (NominalSet a)
--emptySet = NominalSet [Cond (Set []) T]
--
--add1 :: (Alternatives a b) => a -> (NominalSet b) -> (NominalSet b)
--add1 es ss = createAlternatives (zipWith (\cs ce -> (Cond (Set (ce : (elements (value cs)))) (condition cs))) (sets ss) (getAlternatives es))
--
--add :: Alternatives a b => a :-> (NominalSet b :-> NominalSet b)
--add = nominalF (\x -> nominalF (add1 x))
--
------------------------------------------------------------------------------------------------------
--
--a = NominalAtom [Cond (Atom "a") T]
--fs = (:->) [Cond id T]


















