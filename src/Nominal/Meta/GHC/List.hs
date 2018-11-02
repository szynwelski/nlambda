module Nominal.Meta.GHC.List where

import GHC.List
import Nominal.Meta.GHC.Classes
import Nominal.Meta
import Nominal.Variable

(###!!) :: WithMeta [a] -> Int -> WithMeta a
(###!!) = leftIdOp (!!)

nlambda_cycle :: WithMeta [a] -> WithMeta [a]
nlambda_cycle = idOp cycle

nlambda_drop :: Int -> WithMeta [a] -> WithMeta [a]
nlambda_drop = rightIdOp drop

nlambda_dropWhile :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta [a]
nlambda_dropWhile = noMetaResFunOp dropWhile

nlambda_filter :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta [a]
nlambda_filter = noMetaResFunOp filter

nlambda_head :: WithMeta [a] -> WithMeta a
nlambda_head = idOp head

nlambda_init :: WithMeta [a] -> WithMeta [a]
nlambda_init  = idOp init

nlambda_last :: WithMeta [a] -> WithMeta a
nlambda_last = idOp last

nlambda_lookup :: (Var a, Var b, NLambda_Eq a) => WithMeta a -> WithMeta [(a, b)] -> WithMeta (Maybe b)
nlambda_lookup = renameAndApply2 lookup

nlambda_repeat :: WithMeta a -> WithMeta [a]
nlambda_repeat = idOp repeat

nlambda_replicate :: Int -> WithMeta a -> WithMeta [a]
nlambda_replicate = rightIdOp replicate

nlambda_reverse :: WithMeta [a] -> WithMeta [a]
nlambda_reverse = idOp reverse

nlambda_scanl :: Var b => (WithMeta b -> WithMeta a -> WithMeta b) -> WithMeta b -> WithMeta [a] -> WithMeta [b]
nlambda_scanl f x = lift . scanl f x . dropMeta

nlambda_scanl1 :: Var a => (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta [a] -> WithMeta [a]
nlambda_scanl1 f = lift . scanl1 f . dropMeta

nlambda_scanr :: Var b => (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta [a] -> WithMeta [b]
nlambda_scanr f x = lift . scanr f x . dropMeta

nlambda_scanr1 :: Var a => (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta [a] -> WithMeta [a]
nlambda_scanr1 f = lift . scanr1 f . dropMeta

nlambda_span :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta ([a], [a])
nlambda_span = noMetaResFunOp span

nlambda_splitAt :: Int -> WithMeta [a] -> WithMeta ([a], [a])
nlambda_splitAt = rightIdOp splitAt

nlambda_take :: Int -> WithMeta [a] -> WithMeta [a]
nlambda_take = rightIdOp take

nlambda_takeWhile :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta [a]
nlambda_takeWhile = noMetaResFunOp takeWhile

nlambda_tail :: WithMeta [a] -> WithMeta [a]
nlambda_tail = idOp tail

nlambda_unzip :: WithMeta [(a, b)] -> WithMeta ([a], [b])
nlambda_unzip = idOp unzip

nlambda_unzip3 :: WithMeta [(a, b, c)] -> WithMeta ([a], [b], [c])
nlambda_unzip3 = idOp unzip3

nlambda_zip :: (Var a, Var b) => WithMeta [a] -> WithMeta [b] -> WithMeta [(a, b)]
nlambda_zip = renameAndApply2 zip

nlambda_zip3 :: (Var a, Var b, Var c) => WithMeta [a] -> WithMeta [b] -> WithMeta [c] -> WithMeta [(a, b, c)]
nlambda_zip3 = renameAndApply3 zip3
