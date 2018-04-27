{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SampleImport where

import Prelude (Show)

----------------------------------------------------------------------------

data Atom = A deriving Show

data Maybe a = Nothing | Just a deriving Show

--justA :: Maybe Atom
--justA = Just A
--
--class Class a where
--    method :: a -> Atom
--    method _ = A
