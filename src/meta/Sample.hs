{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Show)

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A deriving Show

data Maybe a = Nothing | Just a deriving Show

test :: Maybe Atom
test = Just A
