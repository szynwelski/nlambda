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


----- Prelude -----


metaColon :: WithMeta a -> WithMeta [a] -> WithMeta [a]
metaColon (WithMeta x m) (WithMeta l m') = create (x:l) (unionMeta m m')

metaPair :: WithMeta a -> WithMeta b -> WithMeta (a,b)
metaPair (WithMeta x m) (WithMeta y m') = create (x,y) (unionMeta m m')

metaEq :: Eq a => WithMeta a -> WithMeta a -> WithMeta Bool
metaEq x y = empty $ x == y

metaShow :: Show a => WithMeta a -> WithMeta String
metaShow (WithMeta x _) = empty $ show x

metaPlus :: Num a => WithMeta a -> WithMeta a -> WithMeta a
metaPlus (WithMeta x _) (WithMeta y _) = empty (x+y)

metaMinus :: Num a => WithMeta a -> WithMeta a -> WithMeta a
metaMinus (WithMeta x _) (WithMeta y _) = empty (x-y)

metaConcat :: WithMeta [a] -> WithMeta [a] -> WithMeta [a]
metaConcat (WithMeta l1 m1) (WithMeta l2 m2) = create (l1 ++ l2) (unionMeta m1 m2)
