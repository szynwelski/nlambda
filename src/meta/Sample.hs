{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Show)
import MetaAtom


----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

test :: Atom
test = atom
