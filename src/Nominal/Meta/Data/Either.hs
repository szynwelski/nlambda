{-# LANGUAGE RankNTypes #-}
module Nominal.Meta.Data.Either where

import Data.Either
import Nominal.Meta.GHC.Base
import Nominal.Meta.GHC.Classes
import Nominal.Meta.GHC.Read
import Nominal.Meta.GHC.Show
import Nominal.Meta

instance NLambda_Applicative (Either e)

instance (NLambda_Eq a, NLambda_Eq b) => NLambda_Eq (Either a b)

instance NLambda_Functor (Either a)

instance NLambda_Monad (Either e)

instance (NLambda_Ord a, NLambda_Ord b) => NLambda_Ord (Either a b)

instance (NLambda_Read a, NLambda_Read b) => NLambda_Read (Either a b)

instance (NLambda_Show a, NLambda_Show b) => NLambda_Show (Either a b)

nlambda_Left :: forall a b . WithMeta a -> WithMeta (Either a b)
nlambda_Left = idOp Left

nlambda_Right :: forall a b . WithMeta b -> WithMeta (Either a b)
nlambda_Right = idOp Right

nlambda_isLeft :: WithMeta (Either a b) -> Bool
nlambda_isLeft = isLeft . value

nlambda_isRight :: WithMeta (Either a b) -> Bool
nlambda_isRight = isRight . value

nlambda_either :: (WithMeta a -> WithMeta c) -> (WithMeta b -> WithMeta c) -> WithMeta (Either a b) -> WithMeta c
nlambda_either f1 f2 (WithMeta e m) = either (metaFun m f1) (metaFun m f2) e

