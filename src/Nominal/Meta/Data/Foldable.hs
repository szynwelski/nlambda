{-# LANGUAGE KindSignatures #-}
module Nominal.Meta.Data.Foldable where

import Data.Foldable
import Nominal.Meta.GHC.Base
import Nominal.Meta.GHC.Classes
import Nominal.Meta.GHC.Num
import Nominal.Meta
import Nominal.Variable

-- intentional excessive Var context for MetaPlugin
class (MetaLevel t, Foldable t) => NLambda_Foldable (t :: * -> *) where
    nlambda_fold :: NLambda_Monoid m => WithMeta (t m) -> WithMeta m
    nlambda_fold = idOp fold
    nlambda_foldMap :: NLambda_Monoid m => (WithMeta a -> WithMeta m) -> WithMeta (t a) -> WithMeta m
    nlambda_foldMap = metaFunOp foldMap
    nlambda_foldr :: (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
    nlambda_foldr f x = foldr f x . dropMeta
    nlambda_foldr' :: (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
    nlambda_foldr' f x = foldr' f x . dropMeta
    nlambda_foldl :: (WithMeta b -> WithMeta a -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
    nlambda_foldl f x = foldl f x . dropMeta
    nlambda_foldl' :: (WithMeta b -> WithMeta a -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
    nlambda_foldl' f x = foldl' f x . dropMeta
    nlambda_foldr1 :: (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta (t a) -> WithMeta a
    nlambda_foldr1 f = foldr1 f . dropMeta
    nlambda_foldl1 :: (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta (t a) -> WithMeta a
    nlambda_foldl1 f = foldl1 f . dropMeta
    nlambda_toList :: WithMeta (t a) -> WithMeta [a]
    nlambda_toList = idOp toList
    nlambda_null :: WithMeta (t a) -> Bool
    nlambda_null = noMetaResOp null
    nlambda_length :: WithMeta (t a) -> Int
    nlambda_length = noMetaResOp length
    nlambda_elem :: (Var a, Var (t a), NLambda_Eq a) => WithMeta a -> WithMeta (t a) -> Bool
    nlambda_elem = noMetaRes2ArgOp elem
    nlambda_maximum :: NLambda_Ord a => WithMeta (t a) -> WithMeta a
    nlambda_maximum = idOp maximum
    nlambda_minimum :: NLambda_Ord a => WithMeta (t a) -> WithMeta a
    nlambda_minimum = idOp minimum
    nlambda_sum :: NLambda_Num a => WithMeta (t a) -> WithMeta a
    nlambda_sum = idOp sum
    nlambda_product :: NLambda_Num a => WithMeta (t a) -> WithMeta a
    nlambda_product = idOp product

instance NLambda_Foldable [] -- Defined in ‘Data.Foldable’
instance NLambda_Foldable Maybe -- Defined in ‘Data.Foldable’
instance NLambda_Foldable (Either a) -- Defined in ‘Data.Foldable’
instance NLambda_Foldable ((,) a) -- Defined in ‘Data.Foldable’

nlambda_all :: NLambda_Foldable t => (WithMeta a -> Bool) -> WithMeta (t a) -> Bool
nlambda_all = metaFunOp all

nlambda_any :: NLambda_Foldable t => (WithMeta a -> Bool) -> WithMeta (t a) -> Bool
nlambda_any = metaFunOp any

nlambda_concat :: NLambda_Foldable t => WithMeta (t [a]) -> WithMeta [a]
nlambda_concat = idOp concat

nlambda_concatMap :: (Var b, NLambda_Foldable t) => (WithMeta a -> WithMeta [b]) -> WithMeta (t a) -> WithMeta [b]
nlambda_concatMap f = lift . concatMap (dropMeta . f) . dropMeta

nlambda_find :: NLambda_Foldable t => (WithMeta a -> Bool) -> WithMeta (t a) -> WithMeta (Maybe a)
nlambda_find = noMetaResFunOp find

nlambda_notElem :: (Var (t a), NLambda_Foldable t, NLambda_Eq a) => WithMeta a -> WithMeta (t a) -> Bool
nlambda_notElem = noMetaRes2ArgOp elem
