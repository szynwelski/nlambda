module Meta where

import Data.Functor ((<$>))

data WithMeta a = WithMeta {value :: a, meta :: Meta} deriving Show

type Meta = Maybe Int

emptyMeta :: Meta
emptyMeta = Nothing

empty :: a -> WithMeta a
empty x = WithMeta x emptyMeta

create :: a -> Meta -> WithMeta a
create = WithMeta

union :: WithMeta a -> Meta -> WithMeta a
union (WithMeta x m) m' = WithMeta x ((+) <$> m <*> m')
