{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving, TupleSections #-}

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

--test :: [Bool]
--test = [[] == ([]::[Int]), [1,2,3] == [1,2,3], [1] == [2], Nothing /= (Nothing::Maybe Bool), Just True /= Just True, Just True /= Just False, Just True /= Nothing]

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
--test :: [Bool]
--test = fmap (uncurry (<=)) [((A 1,A 2),(A 1,A 2)),((A 1,A 2),(A 2,A 1))]
--       ++ fmap (uncurry (<)) [([],[A 1]),([A 1],[]),([A 1,A 2,B 3],[A 1,A 2,B 3])]
--       ++ fmap (uncurry (>)) [(Nothing, Just $ A 1), (Just $ A 1, Just $ B 2),(Nothing, Nothing)]
--       ++ fmap (uncurry (>=)) [(Left $ A 1, Left $ A 1), (Left $ A 1, Right $ A 1), (Right $ A 1, Right $ B 2)]

----------------------------------------------------------------------------
-- Test Functor
----------------------------------------------------------------------------

--data Atom a = A a | B deriving (Show, Generic1, MetaLevel, Functor)
--
--test :: [Atom Int]
--test = [fmap id (A 1), fmap id B, 1 <$ (A 2), 1 <$ B]
--test :: ([Maybe (Atom Int)], Maybe (Atom Int, Atom Int), (Atom Int, Atom Int))
--test = (fmap Just [B, A 1], fmap (,B) (Just $ A 1), fmap (const B) (A 1, A 2))

----------------------------------------------------------------------------
-- Test Foldable & Num
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

--data Atom a = A a deriving (Show, Eq, Ord)
--instance Num a => Num (Atom a) where
--    A x + A y = A (x + y)
--    A x * A y = A (x * y)
--    abs (A x) = A (abs x)
--    signum (A x) = A (signum x)
--    negate (A x) = A (negate x)
--    fromInteger x = A (fromInteger x)
--
--test :: [Ordering]
--test = [fold [LT,EQ,GT], foldMap (uncurry compare) [(A 1,A 0),(A 1,A 1),(A 1,A 2)]]

-- FIXME replaceVars - inconsistent types:  A A_nlambda
--test :: [Atom Int]
--test = [foldr (+) (A 0) [], foldr (+) (A 0) [A 1], foldr (+) (A 0) [A 1,A 2,A 3],
--        foldr' (*) (A 1) [], foldr' (*) 1 [A 1], foldr' (*) (A 1) [A 1,A 2,A 3],
--        foldl (+) (A 0) [], foldl (+) (A 0) [A 1], foldl (+) (A 0) [A 1,A 2,A 3],
--        foldl' (*) (A 1) [], foldl' (*) (A 1) [A 1], foldl' (*) (A 1) [A 1,A 2,A 3],
--        foldr1 (+) [A 1], foldr1 (+) [A 1,A 2,A 3],
--        foldl1 (+) [A 1], foldl1 (+) [A 1,A 2,A 3],
--        A $ length [], A $ length [A 1], A $ length [A 1,A 2,A 3],
--        maximum [A 1], maximum [A 1,A 2], maximum [A 1,A 2,A 3],
--        minimum [A 1], minimum [A 1,A 2], minimum [A 1,A 2,A 3],
--        sum [], sum [A 1], sum [A 1,A 2], sum [A 1,A 2,A 3],
--        product [], product [A 1], product [A 1,A 2], product [A 1,A 2,A 3]]
--        ++ toList [] ++ toList [A 1] ++ toList [A 1,A 2,A 3]

-- FIXME replaceVars - inconsistent types:  A A_nlambda
--test :: [Bool]
--test = [null [], null [A 1], null [A 1,A 2], null [A 1,A 2,A 3],
--        elem (A 0) [], elem (A 0) [A 1], elem (A 1) [A 1,A 2], elem (A 0) [A 1,A 2,A 3]]

----------------------------------------------------------------------------
-- Test Traversable
----------------------------------------------------------------------------

-- FIXME convertMetaFun - can't unify meta types: [Applicative_nlambda f] [MetaLevel f]
--data Atom a = A a (Atom a) | B deriving (Show, Generic1, MetaLevel, Functor, Foldable, Traversable)

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

--data Atom a = A a deriving Show
--instance Monoid a => Monoid (Atom a) where
--    mempty = A mempty
--    (A x) `mappend` (A y) = A (x `mappend` y)
--
--test :: [[Atom Bool]]
--test = [mempty, mappend [A True] [A False], mconcat [[A True],[A False]]]

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

--data Atom a = A a deriving (Show)
--notA :: Atom Bool -> Atom Bool
--notA (A True) = A False
--notA (A False) = A True
--
--test :: [[Atom Bool]]

-- FIXME replaceVars - inconsistent types
--test = [pure $ A True, [] <*> [A True], [notA] <*> [A True, A False, A True], [A True, A True] *> [A False, A False], [A False] <* [A True, A False]]
--test = [[] >>= (:[]), [A True,A False] >>= (:[]), [A True, A False] >> [], [] >> [A True, A False], [A True, A True] >> [A False, A False], return $ A True, return $ A False]

-- FIXME replaceVars - inconsistent types
--test = [do {x <- []; return $ notA x}, do {x <- [A True]; return $ notA x}, do {x <- [A False, A True]; return $ notA x}]

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
-- Test Enum
----------------------------------------------------------------------------

--data Atom = A | B | C | D | E | F deriving (Show, Enum)

--FIXME undefined reference
--test :: [[Atom]]
--test = [[succ B], [pred D], [toEnum 1], [toEnum $ fromEnum E], enumFrom C, enumFromThen A C, enumFromTo C E, enumFromThenTo A C D]

----------------------------------------------------------------------------
-- Test Num & Floating & Fractional & Real & Integral
-- TODO RealFrac and RealFloat
----------------------------------------------------------------------------

--data Atom = A deriving (Show, Eq, Ord)--, Enum) -- FIXME undefined reference
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
