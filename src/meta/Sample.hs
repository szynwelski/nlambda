{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Bool(..), Show, Eq, (==), (/=))


----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A | B deriving Eq

test :: [Bool]
test = [True, False, A == A, A /= B, B /= A, B == A]
