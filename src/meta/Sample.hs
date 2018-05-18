{-# OPTIONS_GHC -fplugin MetaPlugin #-}

module Sample where

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A | B deriving (Show, Eq, Ord)

--test :: Bool
test = (compare A A, compare B A, compare A B, compare B B)
