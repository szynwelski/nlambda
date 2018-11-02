{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
module Tests.ImportClass where

import Data.Maybe (listToMaybe)

class Select a where
    select :: [a] -> Maybe a
    select = listToMaybe
