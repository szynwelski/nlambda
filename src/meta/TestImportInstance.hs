{-# OPTIONS_GHC -fplugin MetaPlugin #-}

module TestImportInstance where

import Data.List (find)
import TestImportClass
import TestImportData

instance Select (UpOrDown a b) where
    select = find isUp
