{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, DeriveFoldable #-}

module Test where

import Meta (MetaLevel)
import Var (Var, Variable)
import GHC.Generics
import Data.List (sort)

data Wrapper a = Wrapper a deriving (Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable)

instance Show a => Show (Wrapper a) where
    show (Wrapper x) = "W " ++ show x

data Optional a = Optional a | Null deriving (Show, Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable)

data Pair a b = Pair a b deriving (Show, Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable)

data List a = Element a (List a) | Empty deriving (Show, Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable)

fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = Element x $ fromList xs

----------------------------------------------------------------------------
-- Test Show
----------------------------------------------------------------------------

--test :: Variable -> Variable -> Variable -> [String]
--test x y z = [show (), show x, show y, show (x,y), show (x,y,z), show ([]::[Int]), show [x], show [x,y], show [x,y,z],
--              show $ Wrapper y, show $ Wrapper $ Wrapper y,
--              show (Optional x), show (Null::Optional Char), show (Optional x, Null::Optional String),
--              show (Optional x, Optional y, Null::Optional (), Optional z),
--              show (Pair x y), show (Pair x 1), show (Pair x [y]), show (Pair x (Pair y z)),
--              show $ fromList ([]::[Variable]), show $ fromList [x], show $ fromList [x,y], show $ fromList [x,y,z]]

----------------------------------------------------------------------------
-- Test Eq
----------------------------------------------------------------------------

--test :: Variable -> Variable -> Variable -> [Bool]
--test x y z = [x == x, x == y, x == z, [x] == [y], [x,y] == [x,x], [x,y,z] /= [x,y,z],
--              Wrapper x == Wrapper x, Wrapper x == Wrapper y, Wrapper x /= Wrapper z,
--              Optional x == Null, Optional x == Optional x, Optional x == Optional y, Optional [x] /= Optional [z],
--              Pair x y == Pair y x, Pair x 1 == Pair x 1, Pair x (y,z,1) /= Pair x (x,z,1),
--              fromList [] == fromList [x], fromList [x] == fromList [x], fromList [x,x,z] /= fromList [x,y,z]]

----------------------------------------------------------------------------
-- Test Ord
----------------------------------------------------------------------------

--test :: Variable -> Variable -> Variable -> [Ordering]
--test x y z = fmap (uncurry compare) [(x,x), (x,y), (x,z)]
--test x y z = fmap (uncurry compare) [([x],[y]), ([x,y],[x,x]), ([x,y,z],[x,y,z])]
--test x y z = fmap (uncurry compare) [(Wrapper x,Wrapper x), (Wrapper x,Wrapper y), (Wrapper x,Wrapper z)]
--test x y z = fmap (uncurry compare) [(Optional x,Null), (Optional x,Optional x), (Optional x,Optional y), (Optional z,Optional x)]

--test :: Variable -> Variable -> Variable -> [Bool]
--test x y z = [Pair x y < Pair y x, Pair x 1 > Pair x 1, Pair x (y,z,1) <= Pair x (x,z,1)]

--test :: Variable -> Variable -> Variable -> [List Variable]
--test x y z = sort [fromList [], fromList [x], fromList [x], fromList [y], fromList [z], fromList [x,x,z], fromList [x,y,z]]

----------------------------------------------------------------------------
-- Test Functor
----------------------------------------------------------------------------

--test :: Variable -> Variable -> Variable -> [[Variable]]
--test x y z = [fmap id [x,y,z], fmap (const z) [x,y], fmap id [], fmap (const y) [1,2,3]]

--test :: Variable -> Variable -> Variable -> [Maybe Variable]
--test x y z = [fmap id Nothing, fmap id (Just x), fmap (const x) (Just y), fmap (const y) (Just z)]

--test :: Variable -> Variable -> Variable -> [Wrapper Variable]
--test x y z = [fmap id (Wrapper x), fmap id (Wrapper y), fmap id (Wrapper z), fmap (const z) (Wrapper x), fmap (const y) (Wrapper 1)]

--test :: Variable -> Variable -> Variable -> [Optional Variable]
--test x y z = [fmap id (Optional x), fmap id (Optional y), fmap id (Optional z), fmap (const z) Null, fmap (const y) (Optional 1)]

--test :: Variable -> Variable -> Variable -> [Pair Variable Variable]
--test x y z = [fmap id (Pair x y), fmap (const z) (Pair x y), fmap (const z) (Pair x 1)]

--test :: Variable -> Variable -> Variable -> [List Variable]
--test x y z = [fmap id (fromList [x,y,z]), fmap (const x) (fromList [x,y,z]), y <$ (fromList [x,y,z])]

----------------------------------------------------------------------------
-- Test Foldable
----------------------------------------------------------------------------

--test :: Variable -> Variable -> Variable -> [[Variable]]
--test x y z = [f [x,y,z], f (Just x), f (Left x), f (Right x), f (Wrapper x), f (Pair x y), f (Optional x), f Null, f $ fromList [x,y,z]]
--    where f = foldr (:) []

--test :: Variable -> Variable -> Variable -> [Int]
--test x y z = [length [x,y,z], length (Just x), length (Left x), length (Right x), length (Wrapper x),
--              length (Pair x y), length (Optional x), length Null, length $ fromList [x,y,z]]


--test :: Variable -> Variable -> Variable -> [Bool]
--test x y z = [null [x,y,z], null (Just x), null (Left x), null (Right x), null (Wrapper x),
--              null (Pair x y), null (Optional x), null Null, null $ fromList [x,y,z],
--              elem y [x,y,z], elem y (Just x), elem y (Left x), elem y (Right x :: Either Variable Variable),
--              elem y (Wrapper x), elem y (Pair x y), elem y (Optional x), elem y Null, elem y $ fromList [x,y,z]]

--test :: Variable -> Variable -> Variable -> [Variable]
--test x y z = [maximum [x,y,z], maximum (Just x), maximum (Right x), maximum (Wrapper x),
--              maximum (Pair x y), maximum (Optional x), maximum $ fromList [x,y,z],
--              minimum [x,y,z], minimum (Just x), minimum (Right x :: Either Variable Variable),
--              minimum (Wrapper x), minimum (Pair x y), minimum (Optional x), minimum $ fromList [x,y,z]]
