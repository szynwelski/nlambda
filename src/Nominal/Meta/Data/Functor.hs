module Nominal.Meta.Data.Functor where

import Nominal.Meta.GHC.Base
import Nominal.Meta
import Nominal.Variable

(###<$>) :: (Var b, Var (f b), NLambda_Functor f) => (WithMeta a -> WithMeta b) -> WithMeta (f a) -> WithMeta (f b)
(###<$>) = nlambda_fmap
