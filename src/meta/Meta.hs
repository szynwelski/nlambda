module Meta where

import Data.Functor ((<$>))

data WithMeta a = WithMeta {value :: a, meta :: Meta} deriving Show

type Meta = Maybe Int

empty :: a -> WithMeta a
empty x = WithMeta x Nothing

--lift :: (a -> b) -> WithMeta a -> WithMeta b
--lift f x =

union :: [WithMeta a] -> WithMeta [a]
union xs = WithMeta (value <$> xs) (maximum <$> (sequence $ meta <$> xs))

union1 :: WithMeta a -> WithMeta a
union1 = id

union2 :: WithMeta a -> WithMeta b -> WithMeta (a,b)
union2 x1 x2 = WithMeta (value x1, value x2) (maximum <$> (sequence [meta x1, meta x2]))

union3 :: WithMeta a -> WithMeta b -> WithMeta c -> WithMeta (a,b,c)
union3 x1 x2 x3 = WithMeta (value x1, value x2, value x3) (maximum <$> (sequence [meta x1, meta x2, meta x3]))
