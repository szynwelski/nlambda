{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Bool, Eq, (==))


----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A | B deriving Eq


id x = x

test :: Bool
test = A == B
