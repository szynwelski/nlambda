{-# OPTIONS_GHC -fplugin MetaPlugin #-}

module Sample where

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom a = A a | B deriving Show

instance Functor Atom where
    fmap f (A a) = A (f a)
    fmap _ B = B

--test :: Atom Int
test = fmap id (A 1)
