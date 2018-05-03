{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Meta where

import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics

data WithMeta a = WithMeta {value :: a, meta :: Meta}

instance Show a => Show (WithMeta a) where
    show = show . value

instance Eq a => Eq (WithMeta a) where
    (==) = noMetaResUnionOp (==)
    (/=) = noMetaResUnionOp (/=)

instance Ord a => Ord (WithMeta a) where
    compare = noMetaResUnionOp compare
    (<) = noMetaResUnionOp (<)
    (<=) = noMetaResUnionOp (<=)
    (>) = noMetaResUnionOp (>)
    (>=) = noMetaResUnionOp (>=)
    max = unionOp max
    min = unionOp min

instance Num a => Num (WithMeta a) where
    (+) = unionOp (+)
    (-) = unionOp (-)
    (*) = unionOp (*)
    negate = idOp negate
    abs = idOp abs
    signum = idOp signum
    fromInteger = noMeta . fromInteger

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

noMeta :: a -> WithMeta a
noMeta x = WithMeta x emptyMeta

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
    liftMeta U1 = noMeta U1
    dropMeta (WithMeta U1 _) = U1

instance MetaLevel (K1 i c) where
    liftMeta (K1 x) = noMeta $ K1 x
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
    liftMeta f = noMeta (value . f)
    dropMeta f = noMeta . (value f)

instance MetaLevel IO where
    liftMeta x = noMeta $ fmap value x -- noMeta ???
    dropMeta (WithMeta x m) = fmap (`WithMeta` m) x

------------------------------------------------------------------------------------------
-- Conversion functions to meta operations
------------------------------------------------------------------------------------------

instance Monoid a => Monoid (WithMeta a) where
    mempty = noMeta mempty
    mappend = unionOp mappend

idOp :: (a -> b) -> WithMeta a -> WithMeta b
idOp op (WithMeta x m) = WithMeta (op x) m

noMetaResOp :: (a -> b) -> WithMeta a -> b
noMetaResOp op = op . value

leftIdOp :: (a -> b -> c) -> WithMeta a -> b -> WithMeta c
leftIdOp op (WithMeta x m) y = WithMeta (op x y) m

rightIdOp :: (a -> b -> c) -> a -> WithMeta b -> WithMeta c
rightIdOp op x (WithMeta y m) = WithMeta (op x y) m

unionOp :: (a -> b -> c) -> WithMeta a -> WithMeta b -> WithMeta c
unionOp op (WithMeta x m1) (WithMeta y m2) = WithMeta (op x' y') (getMeta u)
    where u = union [m1, m2]
          x' = rename u 0 x
          y' = rename u 1 y

union3Op :: (a -> b -> c -> d) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d
union3Op op (WithMeta x m1) (WithMeta y m2) (WithMeta z m3) = WithMeta (op x' y' z') (getMeta u)
    where u = union [m1, m2]
          x' = rename u 0 x
          y' = rename u 1 y
          z' = rename u 2 z

noMetaResUnionOp :: (a -> b -> c) -> WithMeta a -> WithMeta b -> c
noMetaResUnionOp op x = value . unionOp op x

(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.*) = (.) . (.)

metaFun :: Meta -> (WithMeta a -> b) -> a -> b
metaFun m f x = f (WithMeta x m)

metaFunOp :: ((a -> b) -> c -> d) -> (WithMeta a -> b) -> WithMeta c -> d
metaFunOp op f (WithMeta x m) = op (metaFun m f) x

noMetaResFunOp :: ((a -> b) -> c -> d) -> (WithMeta a -> b) -> WithMeta c -> WithMeta d
noMetaResFunOp op f (WithMeta x m) = WithMeta (op (metaFun m f) x) m


----------------------------------------------------------------------------------------
-- Meta Equivalents
----------------------------------------------------------------------------------------

data ConvertFun = NoMeta | IdOp | NoMetaResOp | LeftIdOp | RightIdOp | UnionOp | Union3Op | NoMetaResUnionOp | MetaFunOp | NoMetaResFunOp deriving Show

convertFunName :: ConvertFun -> String
convertFunName fun = (toLower $ head $ show fun) : (tail $ show fun)

data MetaEquivalentType = FunSuffix | OpSuffix | SameOp | ConvertFun ConvertFun

data MetaEquivalent = NoEquivalent | MetaFun String | MetaConvertFun String | OrigFun

metaEquivalentModules :: [String]
metaEquivalentModules = ["GHC.Base", "GHC.Classes", "GHC.Float", "GHC.List", "GHC.Num", "GHC.Real", "GHC.Show", "GHC.Tuple", "GHC.Types"]

metaEquivalent :: String -> MetaEquivalent
metaEquivalent name = case Map.lookup name preludeEquivalents of
                        Just FunSuffix -> MetaFun (name ++ "_nlambda")
                        Just OpSuffix -> MetaFun (name ++ "###")
                        Just SameOp -> OrigFun
                        Just (ConvertFun fun) -> MetaConvertFun (convertFunName fun)
                        Nothing -> NoEquivalent

----------------------------------------------------------------------------------------
-- Meta Equivalents
----------------------------------------------------------------------------------------

-- TODO change to map (Module -> Fun -> Type) or (Module -> Type -> [Fun])
preludeEquivalents :: Map String MetaEquivalentType
preludeEquivalents = Map.fromList [
-- GHC.Base
    ("$", SameOp),
    ("$!", SameOp),
    ("*>", ConvertFun UnionOp),
    ("++", ConvertFun UnionOp),
    (".", SameOp),
    ("<$", ConvertFun UnionOp),
    ("<*", ConvertFun UnionOp),
    ("<*>", ConvertFun UnionOp),
    (">>" , ConvertFun UnionOp),
    ("Nothing", ConvertFun NoMeta),
    ("Just", ConvertFun IdOp),
    ("id", ConvertFun IdOp),
-- GHC.Classes
    ("D:Eq", SameOp),
    ("/=", SameOp),
    ("==", SameOp),
    ("$dm==", SameOp),
    ("$dm/=", SameOp),
    ("D:Ord", SameOp),
    ("<", SameOp),
    ("<=", SameOp),
    (">", SameOp),
    (">=", SameOp),
    ("min", SameOp),
    ("$dmmin", SameOp),
    ("max", SameOp),
    ("$dmmax", SameOp),
    ("compare", SameOp),
-- GHC.Float -- FIXME SameOp ??
    ("**", SameOp),
-- GHC.List
    ("!!", ConvertFun LeftIdOp),
-- GHC.Num
    ("*", SameOp),
    ("+", SameOp),
    ("-", SameOp),
-- GHC.Real -- FIXME SameOp ??
    ("/", ConvertFun UnionOp),
    ("^", ConvertFun UnionOp),
    ("^^", ConvertFun UnionOp),
-- GHC.Show
    ("D:Show", SameOp),
    ("showList__", SameOp),
    ("showsPrec", SameOp),
    ("$dmshow", SameOp),
    ("$dmshowList", SameOp),
    ("$dmshowsPrec", SameOp),
-- GHC.Tuple
    ("(,)", ConvertFun UnionOp),
-- GHC.Types
    (":", ConvertFun UnionOp),
    ("[]", ConvertFun NoMeta)]
