{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Show(..), Int, fmap, id, map, length, ($))


----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A

test :: Int
test = length $ fmap id [A]
