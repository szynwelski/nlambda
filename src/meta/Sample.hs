{-# OPTIONS_GHC -fplugin MetaPlugin #-}

{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}


module Sample where

import Meta
import GHC.Generics

--import Prelude

--import Prelude (String, (++))

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

--data Atom = A deriving Show
--data Btom = B deriving Show
--data List a = List a deriving Show

-- OK
--test :: String
--test = show A
--test = show B
--test = show (List A)
--test = show [A]
--test = show (A,A)
--test = show (A,A,A)
--test = show (A,B)
-- ERROR


data Atom a = A a | B deriving (Show, Generic1, MetaLevel)

instance Functor Atom where
    fmap f (A a) = A (f a)
    fmap _ B = B

test :: Atom Int
test = fmap id (A 1)
