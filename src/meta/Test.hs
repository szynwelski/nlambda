{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Test where

import Var
import GHC.Generics

data Pair a b = Pair a b deriving (Show, Generic, Var)

test :: Variable -> Variable -> Pair Variable Variable
test x y = Pair x y
