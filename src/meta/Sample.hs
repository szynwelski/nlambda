{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Bool, Show, Eq, (==))


----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A | B deriving (Show, Eq)

test :: Bool
test = [A == B]
