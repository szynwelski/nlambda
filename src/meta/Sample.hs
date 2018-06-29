{-# OPTIONS_GHC -fplugin MetaPlugin #-}

module Sample where

--import Prelude

--import Prelude (String, (++))

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

--data Atom a = A a | B deriving Show
--
--instance Functor Atom where
--    fmap f (A a) = A (f a)
--    fmap _ B = B
--
--test :: Atom Int
--test = fmap id (A 1)

data Atom = A deriving Show
data Btom = B deriving Show

--data List a = List a deriving Show


test :: String
-- OK
--test = show (List A)
--test = show [A]
--test = show A
--test = show (A,A)
--test = show (A,A,A)
-- ERROR
test = show (A,B)
