module Nominal.Meta.Data.MultiMap where

import Data.MultiMap
import Nominal.Meta.GHC.Classes
import Nominal.Meta
import Nominal.Variable

nlambda_assocs :: WithMeta (MultiMap k a) -> WithMeta [(k, [a])]
nlambda_assocs = idOp assocs

nlambda_empty :: WithMeta (MultiMap k a)
nlambda_empty = noMeta empty

nlambda_insert :: (Var a, NLambda_Ord k) => WithMeta k -> WithMeta a -> WithMeta (MultiMap k a) -> WithMeta (MultiMap k a)
nlambda_insert = renameAndApply3 insert
