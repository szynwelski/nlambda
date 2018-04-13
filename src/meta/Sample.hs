{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sample where

import SampleImport

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

id :: a -> a
id x = x

instance Class Atom
instance Class (Maybe a)

test :: Atom
test = method (id justA)
