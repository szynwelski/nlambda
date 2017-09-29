module Meta where

import Data.Functor ((<$>))

data WithMeta a = WithMeta {value :: a, meta :: Meta} deriving Show

type Meta = Maybe Int

create :: a -> Meta -> WithMeta a
create = WithMeta

empty :: a -> WithMeta a
empty x = WithMeta x Nothing

emptyMeta :: Meta
emptyMeta = Nothing

union :: WithMeta a -> Meta -> WithMeta a
union (WithMeta x m) m' = WithMeta x ((+) <$> m <*> m')
