{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveFunctor #-}

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
--test = [show A, show B, show (List A), show [A], show (A,A), show (A,A,A), show (A,B)]

----------------------------------------------------------------------------
-- Test Functor
----------------------------------------------------------------------------

--data Atom a = A a | B deriving (Show, Generic1, MetaLevel, Functor)
--
--test :: [Atom Int]
--test = [fmap id (A 1), fmap id B, 1 <$ (A 2), 1 <$ B]

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

data Atom = A | B deriving (Show, Eq, Ord)

test :: [Ordering]
test = fmap (uncurry compare) [(A,A), (A,B), (B,A), (B,B)]
