{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveFunctor, StandaloneDeriving #-}

module Sample where

import Meta
import GHC.Generics

----------------------------------------------------------------------------
-- Test Show
----------------------------------------------------------------------------

--data Atom = A deriving Show
--data Btom = B deriving Show
--data List a = List a deriving Show
--
--test :: [String]
--test = [show A, show B, show (List A), show ([]::[Atom]), show [A], show [A,A], show (A,A), show (A,A,A), show (A,B)]

----------------------------------------------------------------------------
-- Test Functor
----------------------------------------------------------------------------

--data Atom a = A a | B deriving (Show, Generic1, MetaLevel, Functor)
--
--test :: [Atom Int]
--test = [fmap id (A 1), fmap id B, 1 <$ (A 2), 1 <$ B]

--test = (fmap (+1) [1,2,3], fmap (+1) (Just 0), fmap (+1) (1,2))

----------------------------------------------------------------------------
-- Test Eq
----------------------------------------------------------------------------

--data Atom = A | B deriving (Show, Eq)
--
--test :: [Bool]
--test = [A == A, A == B, B == A, B == B, A /= A, A /= B, B /= A, B /= B]

--data Atom a = A | B a deriving (Show, Eq)
--
--test :: [Bool]
--test = [(A :: Atom Int) == A, A == B 1, B 1 == A, B 1 == B 1, (A :: Atom Int) /= A, A /= B 1, B 1 /= A, B 1 /= B 1]

----------------------------------------------------------------------------
-- Test Ord
----------------------------------------------------------------------------

--data Atom = A | B deriving (Show, Eq, Ord)
--
--test :: [Ordering]
--test = fmap (uncurry compare) [(A,A), (A,B), (B,A), (B,B)]

--data Atom a = A a | B a | C deriving (Show, Eq)
--deriving instance Ord a => Ord (Atom a)
--
--test :: [Atom Integer]
--test = fmap (uncurry max) [(A 1, A 1), (A 1, A 2), (A 2, A 1), (A 2, B 1), (A 2, B 1), (A 10, C), (C, B 10), (C, C)]

----------------------------------------------------------------------------
-- Test Applicative
----------------------------------------------------------------------------

--data Atom a = A a | B deriving (Show, Generic1, MetaLevel, Functor)
--instance Applicative Atom where
--    pure = A
--    A f <*> a = fmap f a
--    B <*> _ = B
--
--test :: [Atom Int]
--test = [pure 1, A succ <*> A 1, A succ <*> B, B <*> A 0, A 1 <* A 2, B *> B, A 0 <* B, A 0 *> A 2, B *> A 0]

--test :: [[Bool]]
--test = [pure True, [] <*> [True], [not] <*> [True, False, True], [True, True] *> [False, False], [False] <* [True, False]]

----------------------------------------------------------------------------
-- Test Bounded
----------------------------------------------------------------------------

--data Atom = A | B | C | D deriving (Show, Bounded)
--
--test :: [Atom]
--test = [minBound, maxBound]

--data Atom a = A a deriving (Show, Bounded)
--
--test :: [Atom Word]
--test = [minBound, maxBound]

----------------------------------------------------------------------------
-- Test Enum - NOT WORKING!
----------------------------------------------------------------------------

--data Atom = A | B | C | D | E | F deriving (Show, Enum)
--
--test :: [[Atom]]
--test = [[succ B], [pred D], [toEnum 1], [toEnum $ fromEnum E], enumFrom C, enumFromThen A C, enumFromTo C E, enumFromThenTo A C D]

----------------------------------------------------------------------------
-- Test Num & Floating & Fractional
----------------------------------------------------------------------------

--data Atom = A deriving Show
--instance Num Atom where
--    A + A = A
--    A - A = A
--    A * A = A
--    negate A = A
--    abs A = A
--    signum A = A
--    fromInteger _ = A
--instance Fractional Atom where
--    A / A = A
--    recip A = A
--    fromRational _ = A
--instance Floating Atom where
--    pi = A
--    exp A = A
--    log A = A
--    sqrt A = A
--    A ** A = A
--    logBase A A = A
--    sin A = A
--    cos A = A
--    tan A = A
--    asin A = A
--    acos A = A
--    atan A = A
--    sinh A = A
--    cosh A = A
--    tanh A = A
--    asinh A = A
--    acosh A = A
--    atanh A = A
--
--test :: [Atom]
--test = [A + A, A - A, A * A, negate A, abs A, signum A, fromInteger 0]
--test = [A / A, recip A, fromRational 1]
--test = [pi, exp A, log A, sqrt A, A ** A, logBase A A, sin A, cos A, tan A, asin A, acos A, atan A, sinh A, cosh A, tanh A, asinh A, acosh A, atanh A]
