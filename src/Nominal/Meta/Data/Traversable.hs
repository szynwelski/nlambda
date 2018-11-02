{-# LANGUAGE KindSignatures #-}
module Nominal.Meta.Data.Traversable where

import Nominal.Meta.Data.Either
import Nominal.Meta.Data.Foldable
import Nominal.Meta.GHC.Base
import Nominal.Meta
import Nominal.Variable

-- intentional excessive Var context for MetaPlugin
class (Traversable t, NLambda_Functor t, NLambda_Foldable t) => NLambda_Traversable (t :: * -> *) where
    nlambda_traverse :: (Var b, Var (t b), Var (f (t b)), NLambda_Applicative f) => (WithMeta a -> WithMeta (f b)) -> WithMeta (t a) -> WithMeta (f (t b))
    nlambda_traverse f (WithMeta x m) = lift $ fmap lift $ traverse (dropMeta . metaFun m f) x
    nlambda_sequenceA :: NLambda_Applicative f => WithMeta (t (f a)) -> WithMeta (f (t a))
    nlambda_sequenceA = idOp sequenceA
    nlambda_mapM :: (Var b, Var (t b), Var (m (t b)), NLambda_Monad m) => (WithMeta a -> WithMeta (m b)) -> WithMeta (t a) -> WithMeta (m (t b))
    nlambda_mapM f (WithMeta x m) = lift $ fmap lift $ mapM (dropMeta . metaFun m f) x
    nlambda_sequence :: NLambda_Monad m => WithMeta (t (m a)) -> WithMeta (m (t a))
    nlambda_sequence = idOp sequence

instance NLambda_Traversable []
instance NLambda_Traversable Maybe
instance NLambda_Traversable (Either a)
instance NLambda_Traversable ((,) a)
