{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Test where

import Var (Var, Variable)
import GHC.Generics
import Data.List (sort)

data Wrapper a = Wrapper a deriving (Generic, Var, Eq, Ord)

instance Show a => Show (Wrapper a) where
    show (Wrapper x) = "W " ++ show x

data Optional a = Optional a | Null deriving (Show, Generic, Var, Eq, Ord)

data Pair a b = Pair a b deriving (Show, Generic, Var, Eq, Ord)

data List a = Element a (List a) | Empty deriving (Show, Generic, Var, Eq, Ord)

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
