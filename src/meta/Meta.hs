module Meta where

data WithMeta a = WithMeta {value :: a, meta :: Meta} deriving (Show, Eq)

type Meta = Int
type IdsMap = Int
type Union = (Meta, [IdsMap])

emptyMeta :: Meta
emptyMeta = 1

empty :: a -> WithMeta a
empty x = WithMeta x emptyMeta

create :: a -> Meta -> WithMeta a
create x m = WithMeta x m

union :: [Meta] -> Union
union ms = (0, [])

getMeta :: Union -> Meta
getMeta = fst

rename :: Union -> Int -> a -> a -- Int mówi, którą mapę z [IdsMap] wziąć do zmiany nazwy
rename _ _ = id

emptyList :: [a]
emptyList = []

colon :: a -> [a] -> [a]
colon = (:)
