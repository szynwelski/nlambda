{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveFunctor, DeriveFoldable, StandaloneDeriving #-}

module Sample where

import Meta
import GHC.Generics
import Data.Foldable (fold, foldr', foldl', toList)

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
-- Test Functor
----------------------------------------------------------------------------

--data Atom a = A a | B deriving (Show, Generic1, MetaLevel, Functor)
--
--test :: [Atom Int]
--test = [fmap id (A 1), fmap id B, 1 <$ (A 2), 1 <$ B]

--test = (fmap (+1) [1,2,3], fmap (+1) (Just 0), fmap (+1) (1,2))

----------------------------------------------------------------------------
-- Test Foldable
----------------------------------------------------------------------------

--data Atom a = A a (Atom a) | B deriving (Show, Generic1, MetaLevel, Foldable)
--
--test :: [Ordering]
--test = [fold (A LT (A EQ (A GT B))), foldMap (uncurry compare) (A (1,0) (A (1,1) (A (1,2) B)))]
--test :: [Int]
--test = [foldr (+) 0 B, foldr (+) 0 (A 1 B), foldr (+) 0 (A 1 (A 2 (A 3 B))),
--        foldr' (*) 1 B, foldr' (*) 1 (A 1 B), foldr' (*) 1 (A 1 (A 2 (A 3 B))),
--        foldl (+) 0 B, foldl (+) 0 (A 1 B), foldl (+) 0 (A 1 (A 2 (A 3 B))),
--        foldl' (*) 1 B, foldl' (*) 1 (A 1 B), foldl' (*) 1 (A 1 (A 2 (A 3 B))),
--        foldr1 (+) (A 1 B), foldr1 (+) (A 1 (A 2 (A 3 B))),
--        foldl1 (+) (A 1 B), foldl1 (+) (A 1 (A 2 (A 3 B))),
--        length B, length (A 1 B), length (A 1 (A 2 (A 3 B))),
--        maximum (A 1 B), maximum (A 1 (A 2 B)), maximum (A 1 (A 2 (A 3 B))),
--        minimum (A 1 B), minimum (A 1 (A 2 B)), minimum (A 1 (A 2 (A 3 B))),
--        sum B, sum (A 1 B), sum (A 1 (A 2 B)), sum (A 1 (A 2 (A 3 B))),
--        product B, product (A 1 B), product (A 1 (A 2 B)), product (A 1 (A 2 (A 3 B)))]
--        ++ toList B ++ toList (A 1 B) ++ toList (A 1 (A 2 (A 3 B)))
--test :: [Bool]
--test = [null B, null (A 1 B), null (A 1 (A 2 B)), null (A 1 (A 2 (A 3 B))),
--        elem 0 B, elem 0 (A 1 B), elem 1 (A 1 (A 2 B)), elem 0 (A 1 (A 2 (A 3 B)))]

--test :: [Ordering]
--test = [fold [LT,EQ,GT], foldMap (uncurry compare) [(1,0),(1,1),(1,2)]]
--test :: [Int]
--test = [foldr (+) 0 [], foldr (+) 0 [1], foldr (+) 0 [1,2,3],
--        foldr' (*) 1 [], foldr' (*) 1 [1], foldr' (*) 1 [1,2,3],
--        foldl (+) 0 [], foldl (+) 0 [1], foldl (+) 0 [1,2,3],
--        foldl' (*) 1 [], foldl' (*) 1 [1], foldl' (*) 1 [1,2,3],
--        foldr1 (+) [1], foldr1 (+) [1,2,3],
--        foldl1 (+) [1], foldl1 (+) [1,2,3],
--        length [], length [1], length [1,2,3],
--        maximum [1], maximum [1,2], maximum [1,2,3],
--        minimum [1], minimum [1,2], minimum [1,2,3],
--        sum [], sum [1], sum [1,2], sum [1,2,3],
--        product [], product [1], product [1,2], product [1,2,3]]
--        ++ toList [] ++ toList [1] ++ toList [1,2,3]
--test :: [Bool]
--test = [null [], null [1], null [1,2], null [1,2,3],
--        elem 0 [], elem 0 [1], elem 1 [1,2], elem 0 [1,2,3]]


----------------------------------------------------------------------------
-- Test Monoid
----------------------------------------------------------------------------

--data Atom a = A a | B deriving Show
--instance Monoid a => Monoid (Atom a) where
--    mempty = B
--    B `mappend` a = a
--    a `mappend` B = a
--    A a1 `mappend` A a2 = A (a1 `mappend` a2)
--
--test :: [Atom Ordering]
--test = [mempty, A EQ `mappend` A LT, A EQ `mappend` B, B `mappend` A GT, B `mappend` B, mconcat [], mconcat [A LT, A EQ, A GT]]

--test :: [[Bool]]
--test = [mempty, mappend [True] [False], mconcat [[True],[False]]]

----------------------------------------------------------------------------
-- Test Applicative && Monad
----------------------------------------------------------------------------

--data Atom a = A a | B deriving (Show, Generic1, MetaLevel, Functor)
--instance Applicative Atom where
--    pure = A
--    A f <*> a = fmap f a
--    B <*> _ = B
--instance Monad Atom where
--    (A x) >>= k = k x
--    B >>= _ = B
--
--test :: [Atom Int]
--test = [pure 1, A succ <*> A 1, A succ <*> B, B <*> A 0, A 1 <* A 2, B *> B, A 0 <* B, A 0 *> A 2, B *> A 0]
--test = [A 0 >>= A, B >>= A, A 1 >> A 0, A 1 >> B, B >> A 0, B >> B, return 0]
--test = [do {x <- A 0; return $ x + 1}, do {x <- B; return $ x + 1}]

--test :: [[Bool]]
--test = [pure True, [] <*> [True], [not] <*> [True, False, True], [True, True] *> [False, False], [False] <* [True, False]]
--test = [[] >>= (:[]), [True,False] >>= (:[]), [True, False] >> [], [] >> [True, False], [True, True] >> [False, False], return True, return False]
--test = [do {x <- []; return $ not x}, do {x <- [True]; return $ not x}, do {x <- [False, True]; return $ not x}]

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
-- Test Enum - FIXME
----------------------------------------------------------------------------

--data Atom = A | B | C | D | E | F deriving (Show, Enum)
--
--test :: [[Atom]]
--test = [[succ B], [pred D], [toEnum 1], [toEnum $ fromEnum E], enumFrom C, enumFromThen A C, enumFromTo C E, enumFromThenTo A C D]

----------------------------------------------------------------------------
-- Test Num & Floating & Fractional & Real & Integral
-- TODO RealFrac and RealFloat
----------------------------------------------------------------------------

--data Atom = A deriving (Show, Eq, Ord)--, Enum) -- FIXME
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
--instance Real Atom where
--    toRational A = 0
--instance Integral Atom where
--  quot A A = A
--  rem A A = A
--  div A A = A
--  mod A A = A
--  quotRem A A = (A,A)
--  divMod A A = (A,A)
--  toInteger A = 0
--
--test :: [Atom]
--test = [A + A, A - A, A * A, negate A, abs A, signum A, fromInteger 0]
--test = [A / A, recip A, fromRational 1]
--test = [pi, exp A, log A, sqrt A, A ** A, logBase A A, sin A, cos A, tan A, asin A, acos A, atan A, sinh A, cosh A, tanh A, asinh A, acosh A, atanh A]
--test = [fromRational $ toRational A]
--test = [quot A A, rem A A, div A A, mod A A, fst $ quotRem A A, snd $ quotRem A A, fst $ divMod A A, snd $ divMod A A, fromInteger $ toInteger A]
