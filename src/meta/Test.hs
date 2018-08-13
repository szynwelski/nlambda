{-# OPTIONS_GHC -fplugin MetaPlugin #-}

module Test where

import Var

data Pair a b = Pair a b

test x y = Pair x y
