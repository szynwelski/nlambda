{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Meta where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics


data WithMeta a = WithMeta {value :: a, meta :: Meta} deriving (Show, Eq)

type Identifier = Int
type IdMap = Map Identifier Identifier
type IdPairSet = Set (Identifier, Identifier)
type Meta = (IdMap, IdPairSet)
type Union = (Meta, [IdMap])

-- TODO
replaceVariablesIds :: IdMap -> a -> a
replaceVariablesIds = undefined

splitMapping :: IdPairSet -> IdMap -> (IdMap, IdMap)
splitMapping s m = if Set.null s then (Map.empty, m) else Map.partitionWithKey (\k v -> Set.member (k, v) s) m

renameMaps :: Meta -> Meta -> (Meta, IdMap, IdMap)
renameMaps (m1, s1) (m2, s2) = ((Map.union m1b m2c, Set.unions [s1, s2, Set.fromList $ Map.assocs m2d]), m1a, Map.union m2a m2d)
    where (m1a, m1b) = splitMapping s2 m1
          (m2a, m2b) = splitMapping s1 m2
          (m2c, m2d) = if Map.null m1b then (m2b, Map.empty) else Map.partitionWithKey (\k v -> v == Map.findWithDefault v k m1b) m2b

metaFromMap :: IdMap -> Meta
metaFromMap map = (map, Set.empty)

emptyMeta :: Meta
emptyMeta = (Map.empty, Set.empty)

empty :: a -> WithMeta a
empty x = WithMeta x emptyMeta

create :: a -> Meta -> WithMeta a
create x m = WithMeta x m

union :: [Meta] -> Union
union [] = (emptyMeta, [])
union [m] = (m, [Map.empty])
union (m:ms) = let (m', idMap:idMaps) = union ms
                   (m'', idMap1, idMap2) = renameMaps m m'
               in (m'', idMap1 : (Map.union idMap2 idMap) : idMaps)

getMeta :: Union -> Meta
getMeta = fst

rename :: Union -> Int -> a -> a
rename (_, idMaps) n x = let idMap = idMaps !! n
                         in if null idMap then x else x

emptyList :: [a]
emptyList = []

colon :: a -> [a] -> [a]
colon = (:)

------------------------------------------------------------------------------------------
-- class MetaLevel
------------------------------------------------------------------------------------------

class MetaLevel (f :: * -> *) where
    liftMeta :: f (WithMeta a) -> WithMeta (f a)
    dropMeta :: WithMeta (f a) -> f (WithMeta a)
    default liftMeta :: (Generic1 f, MetaLevel (Rep1 f)) => f (WithMeta a) -> WithMeta (f a)
    liftMeta x = let (WithMeta y m) = liftMeta (from1 x) in WithMeta (to1 y) m
    default dropMeta :: (Generic1 f, MetaLevel (Rep1 f)) => WithMeta (f a) -> f (WithMeta a)
    dropMeta (WithMeta x m) = to1 $ dropMeta (WithMeta (from1 x) m)

instance MetaLevel Par1 where
    liftMeta (Par1 (WithMeta x m)) = WithMeta (Par1 x) m
    dropMeta (WithMeta (Par1 x) m) = Par1 (WithMeta x m)

instance MetaLevel f => MetaLevel (Rec1 f) where
    liftMeta (Rec1 x) = let (WithMeta y m) = liftMeta x in WithMeta (Rec1 y) m
    dropMeta (WithMeta (Rec1 x) m) = Rec1 (dropMeta (WithMeta x m))

instance MetaLevel U1 where
    liftMeta U1 = empty U1
    dropMeta (WithMeta U1 _) = U1

instance MetaLevel (K1 i c) where
    liftMeta (K1 x) = empty $ K1 x
    dropMeta (WithMeta (K1 x) m) = K1 x

instance MetaLevel f => MetaLevel (M1 i c f) where
    liftMeta (M1 x) = let (WithMeta y m) = liftMeta x in WithMeta (M1 y) m
    dropMeta (WithMeta (M1 x) m) = M1 (dropMeta (WithMeta x m))

instance (MetaLevel f, MetaLevel g) => MetaLevel (f :+: g) where
    liftMeta (L1 x) = let (WithMeta y m) = liftMeta x in WithMeta (L1 y) m
    liftMeta (R1 x) = let (WithMeta y m) = liftMeta x in WithMeta (R1 y) m
    dropMeta (WithMeta (L1 x) m) = L1 (dropMeta (WithMeta x m))
    dropMeta (WithMeta (R1 x) m) = R1 (dropMeta (WithMeta x m))

instance (MetaLevel f, MetaLevel g) => MetaLevel (f :*: g) where
    liftMeta (x :*: y) = let (WithMeta x' m1) = liftMeta x
                             (WithMeta y' m2) = liftMeta y
                             u = union [m1, m2]
                             x'' = rename u 0 x'
                             y'' = rename u 1 y'
                         in WithMeta (x'' :*: y'') (getMeta u)
    dropMeta (WithMeta (x :*: y) m) = dropMeta (WithMeta x m) :*: dropMeta (WithMeta y m)

instance MetaLevel Maybe
instance MetaLevel []
instance MetaLevel (Either a)
instance MetaLevel ((,) a)
instance MetaLevel ((->) a) where
    liftMeta f = empty (value . f)
    dropMeta f = empty . (value f)

instance MetaLevel IO where
    liftMeta x = empty $ fmap value x -- empty ???
    dropMeta (WithMeta x m) = fmap (`WithMeta` m) x
