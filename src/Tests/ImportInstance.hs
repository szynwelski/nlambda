{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}

module Tests.ImportInstance where

import Data.List (find)
import Tests.ImportClass
import Tests.ImportData

instance Select (UpOrDown a b) where
    select = find isUp
