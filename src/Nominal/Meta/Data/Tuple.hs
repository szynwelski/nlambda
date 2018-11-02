module Nominal.Meta.Data.Tuple where

import Data.Tuple
import Nominal.Meta
import Nominal.Variable

nlambda_curry :: (Var a, Var b) => (WithMeta (a, b) -> WithMeta c) -> WithMeta a -> WithMeta b -> WithMeta c
nlambda_curry f x y = f $ renameAndApply2 (,) x y

nlambda_fst :: WithMeta (a, b) -> WithMeta a
nlambda_fst = idOp fst

nlambda_snd :: WithMeta (a, b) -> WithMeta b
nlambda_snd = idOp snd

nlambda_swap :: WithMeta (a, b) -> WithMeta (b, a)
nlambda_swap = idOp swap

nlambda_uncurry :: (WithMeta a -> WithMeta b -> WithMeta c) -> WithMeta (a, b) -> WithMeta c
nlambda_uncurry f p = f (idOp fst p) (idOp snd p)
