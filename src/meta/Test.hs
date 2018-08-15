{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Test where

import Var (Var, Variable)
import GHC.Generics

data Wrapper a = Wrapper a deriving (Generic, Var, Eq, Ord)

instance Show a => Show (Wrapper a) where
    show (Wrapper x) = "Wrap " ++ show x

data Optional a = Optional a | Null deriving (Show, Generic, Var, Eq, Ord)

data Pair a b = Pair a b deriving (Show, Generic, Var, Eq, Ord)

data List a = Element a (List a) | Empty deriving (Show, Generic, Var, Eq, Ord)

fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = Element x $ fromList xs

----------------------------------------------------------------------------
-- Test Show
----------------------------------------------------------------------------

test :: Variable -> Variable -> Variable -> [String]
test x y z = [show (), show x, show y, show (x,y), show (x,y,z), show ([]::[Int]), show [x], show [x,y], show [x,y,z],
              show $ fromList ([]::[Variable]), show $ fromList [x], show $ fromList [x,y], show $ fromList [x,y,z],
              show (Pair x y), show (Pair x 1), show (Pair x [y]), show (Pair x (Pair y z)), show $ Wrapper y, show $ Wrapper $ Wrapper y,
              show (Optional x), show (Null::Optional Char), show (Optional x, Null::Optional String),
              show (Optional x, Optional y, Null::Optional (), Optional z)]
