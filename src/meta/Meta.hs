module Meta where

data WithMeta a = WithMeta {value :: a, meta :: Meta} deriving (Show, Eq)

type Meta = Int

emptyMeta :: Meta
emptyMeta = 1

unionMeta :: Meta -> Meta -> Meta
unionMeta = (+)

empty :: a -> WithMeta a
empty x = WithMeta x emptyMeta

create :: a -> Meta -> WithMeta a
create x m = WithMeta x m

union :: WithMeta a -> Meta -> WithMeta a
union (WithMeta x m) m' = WithMeta x (unionMeta m m')

metaColon :: WithMeta (WithMeta a -> WithMeta (WithMeta [a] -> WithMeta [a]))
metaColon = empty $ \(WithMeta x m) -> empty $ \(WithMeta l m') -> create (x:l) (unionMeta m m')

metaPair :: WithMeta (WithMeta a -> WithMeta (WithMeta b -> WithMeta (a,b)))
metaPair = empty $ \(WithMeta x m) -> empty $ \(WithMeta y m') -> create (x,y) (unionMeta m m')

metaEq :: Eq a => WithMeta (WithMeta a -> WithMeta (WithMeta a -> WithMeta Bool))
metaEq = empty $ \x -> empty $ \y -> empty $ x == y
