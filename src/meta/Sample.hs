{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import Prelude (Bool(..), Show, Eq, Ord, Num(..), min, max, id, const)


----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A | B deriving (Show, Eq, Ord)

instance Num Atom where
    (+) a b = min a b
    a - b = min a b
    a * b = min a b
    negate = id
    abs = id
    signum = id
    fromInteger = const A

test :: [Atom]
test = [A + B, A - B, A * B, negate A, abs A, signum A, fromInteger 1]
