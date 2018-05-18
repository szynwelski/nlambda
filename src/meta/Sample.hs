{-# OPTIONS_GHC -fplugin MetaPlugin #-}

module Sample where

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A deriving Show

test :: String
test = show A
