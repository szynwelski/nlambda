{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Test where

import Var
import GHC.Generics

--test :: Variable -> Variable -> Bool
--test x y = x == y

--test :: Variable -> Variable -> (Int, Variable, Variable)
--test x y = id (1, x, y)

--test :: Variable -> Variable -> [Variable]
--test x y = [id] <*> [x]

data Pair a b = Pair a b deriving (Show, Generic, Var)

test :: Variable -> Variable -> Pair Variable Variable
test = Pair

--test :: Variable -> Variable -> Pair Variable [Variable]
--test x y = Pair x [y]
