module Nominal.Meta.GHC.Tuple where

import Nominal.Meta
import Nominal.Variable

(####) :: (Var a, Var b) => WithMeta a -> WithMeta b -> WithMeta (a, b)
(####) = renameAndApply2 (,)

(#####) :: (Var a, Var b, Var c) => WithMeta a -> WithMeta b -> WithMeta c -> WithMeta (a, b, c)
(#####) = renameAndApply3 (,,)

(######) :: (Var a, Var b, Var c, Var d) => WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta (a, b, c, d)
(######) = renameAndApply4 (,,,)

(#######) :: (Var a, Var b, Var c, Var d, Var e) => WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta (a, b, c, d, e)
(#######) = renameAndApply5 (,,,,)
