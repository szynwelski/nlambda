{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
{-# LANGUAGE DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable #-}

module Tests.ImportData where

import GHC.Generics
import Nominal.Meta (MetaLevel)
import Nominal.Variable (Var)

data UpOrDown a b = Up a | Down b deriving (Show, Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable) -- FIXME Traversable

isUp :: UpOrDown a b -> Bool
isUp (Up _) = True
isUp _ = False

isDown :: UpOrDown a b -> Bool
isDown (Down _) = True
isDown _ = False
