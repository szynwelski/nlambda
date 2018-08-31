{-# OPTIONS_GHC -fplugin MetaPlugin #-}

module TestImportClass where

import Data.Maybe (listToMaybe)

class Select a where
    select :: [a] -> Maybe a
    select = listToMaybe
