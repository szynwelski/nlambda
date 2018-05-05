{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Show, fmap, id)


----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A deriving Show

test :: [Atom]
test = fmap id [A]
