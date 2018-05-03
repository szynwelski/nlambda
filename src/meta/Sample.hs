{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Bool, Show, Eq, Ord, min)


----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A | B deriving (Show, Eq, Ord)

test :: Atom
test = min A B
