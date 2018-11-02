module Nominal.Meta.Data.Semigroup where

import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup
import Nominal.Meta.GHC.Real
import Nominal.Meta
import Nominal.Variable

class Semigroup a => NLambda_Semigroup a where
  (###<>) :: Var a => WithMeta a -> WithMeta a -> WithMeta a
  (###<>) = renameAndApply2 (<>)
  nlambda_sconcat :: WithMeta (NonEmpty a) -> WithMeta a
  nlambda_sconcat = idOp sconcat
  nlambda_stimes :: (Var a, NLambda_Integral b) => WithMeta b -> WithMeta a -> WithMeta a
  nlambda_stimes = renameAndApply2 stimes

instance NLambda_Semigroup Ordering
instance NLambda_Semigroup ()
instance NLambda_Semigroup [a]
instance NLambda_Semigroup a => NLambda_Semigroup (Maybe a)
instance NLambda_Semigroup b => NLambda_Semigroup (a -> b)
instance NLambda_Semigroup (Either a b)
instance (NLambda_Semigroup a, NLambda_Semigroup b) => NLambda_Semigroup (a, b)
instance (NLambda_Semigroup a, NLambda_Semigroup b, NLambda_Semigroup c) => NLambda_Semigroup (a, b, c)
instance (NLambda_Semigroup a, NLambda_Semigroup b, NLambda_Semigroup c, NLambda_Semigroup d) => NLambda_Semigroup (a, b, c, d)
instance (NLambda_Semigroup a, NLambda_Semigroup b, NLambda_Semigroup c, NLambda_Semigroup d, NLambda_Semigroup e) => NLambda_Semigroup (a, b, c, d, e)
