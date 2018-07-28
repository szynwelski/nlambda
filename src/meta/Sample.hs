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
