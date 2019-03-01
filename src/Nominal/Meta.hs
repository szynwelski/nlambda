{-# LANGUAGE DefaultSignatures, DeriveDataTypeable, FlexibleContexts, KindSignatures, TypeOperators #-}
module Nominal.Meta where

import Data.Char (toLower)
import Data.Data (Data)
import Data.Functor.Identity (Identity)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics
import Nominal.Variable
import Text.ParserCombinators.ReadPrec (ReadPrec)

------------------------------------------------------------------------------------------
-- Meta information
------------------------------------------------------------------------------------------

data Meta = Meta {toRename :: IdMap, renamed :: Set (Identifier, Identifier), renameTree :: RenameTree} deriving Show

metaFromMap :: IdMap -> Meta
metaFromMap map = Meta map Set.empty Empty

emptyMeta :: Meta
emptyMeta = metaFromMap Map.empty

metaFromRenamed :: Set (Identifier, Identifier) -> Meta
metaFromRenamed renamed = Meta Map.empty renamed Empty

addMapToMeta :: IdMap -> Meta -> Meta
addMapToMeta map (Meta m s t) = Meta (Map.union map m) s t

removeMapFromMeta :: IdMap -> Meta -> Meta
removeMapFromMeta map (Meta m s t) = Meta (Map.differenceWith (\x y -> if x == y then Nothing else Just x) m map)
                                          (Set.difference s $ Set.fromAscList $ Map.toAscList map)
                                          t

------------------------------------------------------------------------------------------
-- Class WithMeta and instances
------------------------------------------------------------------------------------------

data WithMeta a = WithMeta {value :: a, meta :: Meta}

instance (Var a, Show a) => Show (WithMeta a) where
    showsPrec n = noMetaResOp $ showsPrec n
    show (WithMeta x m) = show x ++ " | meta:" ++ show m
    showList = showList . value . lift

instance (Monoid a, Var a) => Monoid (WithMeta a) where
    mempty = noMeta mempty
    mappend = renameAndApply2 mappend
    mconcat = idOp mconcat . lift

noMeta :: a -> WithMeta a
noMeta x = WithMeta x emptyMeta

create :: a -> Meta -> WithMeta a
create x m = WithMeta x m

-- annotation to not crete meta equivalent for a function
data NoMetaFunction = NoMetaFunction deriving Data

------------------------------------------------------------------------------------------
-- Rename methods
------------------------------------------------------------------------------------------

union :: Bool -> Meta -> Meta -> Meta
union join (Meta map1 set1 tree1) (Meta map2 set2 tree2)
    = Meta (Map.unionWith conflict (Map.difference map1 toRenameNow1) (Map.difference map2 toRenameNow2))
           (Set.unions [set1, set2, Set.fromList $ Map.assocs toRenameNow1, Set.fromList $ Map.assocs toRenameNow2])
           (createNode (not join) children)
    where partitionByRenamed map set = Map.partitionWithKey (\k v -> Set.member (k,v) set) map
          findConflicts mapToRestrict map = Map.filterWithKey (\k v -> Map.findWithDefault v k map /= v) mapToRestrict
          (inRenamed1, notInRenamed1) = partitionByRenamed map1 set2
          (inRenamed2, notInRenamed2) = partitionByRenamed map2 set1
          (conflicts1, conflicts2) = (findConflicts notInRenamed1 notInRenamed2, findConflicts notInRenamed2 notInRenamed1)
          (toRenameNow1, toRenameNow2) = if Map.null inRenamed1
                                         then (Map.empty, Map.unionWith conflict inRenamed2 conflicts2)
                                         else (Map.unionWith conflict inRenamed1 conflicts1, inRenamed2)
          tree1' = addMapToTree toRenameNow1 tree1
          tree2' = addMapToTree toRenameNow2 tree2
          children = if join then getChildrenOrNode tree1' ++ getChildrenOrNode tree2' else [tree1', tree2']
          conflict x y | x == y = x
          conflict x y = error $ "conflict in union function for " ++ show (Meta map1 set1 tree1) ++ show (Meta map2 set2 tree2)

unions :: [Meta] -> Meta
unions [] = emptyMeta
unions ms = foldr1 (union True) ms

rename :: Var a => WithMeta a -> WithMeta a
rename (WithMeta x m)
    | isTreeEmpty tree = create x m'
    | otherwise = create (renameVariables tree x) m'
    where tree = renameTree m
          m' = m {renameTree = Empty}

------------------------------------------------------------------------------------------
-- Conversion functions to meta operations
------------------------------------------------------------------------------------------

idOp :: (a -> b) -> WithMeta a -> WithMeta b
idOp op (WithMeta x m) = WithMeta (op x) m

noMetaArgOp :: (a -> b) -> a -> WithMeta b
noMetaArgOp op = noMeta . op

noMetaResOp :: (a -> b) -> WithMeta a -> b
noMetaResOp op = op . value

leftIdOp :: (a -> b -> c) -> WithMeta a -> b -> WithMeta c
leftIdOp op (WithMeta x m) y = WithMeta (op x y) m

rightIdOp :: (a -> b -> c) -> a -> WithMeta b -> WithMeta c
rightIdOp op x (WithMeta y m) = WithMeta (op x y) m

renameAndApply2 :: (Var a, Var b) => (a -> b -> c) -> WithMeta a -> WithMeta b -> WithMeta c
renameAndApply2 f (WithMeta x1 m1) (WithMeta x2 m2) = create (f x1' x2') m
    where (WithMeta (x1',x2') m) = rename $ create (x1,x2) (unions [m1,m2])

renameAndApply3 :: (Var a, Var b, Var c) => (a -> b -> c -> d) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d
renameAndApply3 f (WithMeta x1 m1) (WithMeta x2 m2) (WithMeta x3 m3) = create (f x1' x2' x3') m
    where (WithMeta (x1',x2',x3') m) = rename $ create (x1,x2,x3) (unions [m1,m2,m3])

renameAndApply4 :: (Var a, Var b, Var c, Var d) => (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e
renameAndApply4 f (WithMeta x1 m1) (WithMeta x2 m2) (WithMeta x3 m3) (WithMeta x4 m4) = create (f x1' x2' x3' x4') m
    where (WithMeta (x1',x2',x3',x4') m) = rename $ create (x1,x2,x3,x4) (unions [m1,m2,m3,m4])

renameAndApply5 :: (Var a, Var b, Var c, Var d, Var e) => (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
renameAndApply5 f (WithMeta x1 m1) (WithMeta x2 m2) (WithMeta x3 m3) (WithMeta x4 m4) (WithMeta x5 m5) = create (f x1' x2' x3' x4' x5') m
    where (WithMeta (x1',x2',x3',x4',x5') m) = rename $ create (x1,x2,x3,x4,x5) (unions [m1,m2,m3,m4,m5])

renameAndApply6 :: (Var a, Var b, Var c, Var d, Var e, Var f) => (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
renameAndApply6 f (WithMeta x1 m1) (WithMeta x2 m2) (WithMeta x3 m3) (WithMeta x4 m4) (WithMeta x5 m5) (WithMeta x6 m6) = create (f x1' x2' x3' x4' x5' x6') m
    where (WithMeta (x1',x2',x3',x4',x5',x6') m) = rename $ create (x1,x2,x3,x4,x5,x6) (unions [m1,m2,m3,m4,m5,m6])

renameAndApply7 :: (Var a, Var b, Var c, Var d, Var e, Var f, Var g) => (a -> b -> c -> d -> e -> f -> g -> h) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g -> WithMeta h
renameAndApply7 f (WithMeta x1 m1) (WithMeta x2 m2) (WithMeta x3 m3) (WithMeta x4 m4) (WithMeta x5 m5) (WithMeta x6 m6) (WithMeta x7 m7) = create (f x1' x2' x3' x4' x5' x6' x7') m
    where (WithMeta (x1',x2',x3',x4',x5',x6',x7') m) = rename $ create (x1,x2,x3,x4,x5,x6,x7) (unions [m1,m2,m3,m4,m5,m6,m7])

noMetaRes2ArgOp :: (Var a, Var b) => (a -> b -> c) -> WithMeta a -> WithMeta b -> c
noMetaRes2ArgOp op x = value . renameAndApply2 op x

noMeta2ArgOp :: Var c => (a -> b -> c) -> a -> b -> WithMeta c
noMeta2ArgOp op x = noMeta . op x

(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.*) = (.) . (.)

metaFun :: Meta -> (WithMeta a -> b) -> a -> b
metaFun m f x = f (WithMeta x m)

metaFunOp :: ((a -> b) -> c -> d) -> (WithMeta a -> b) -> WithMeta c -> d
metaFunOp op f (WithMeta x m) = op (metaFun m f) x

noMetaResFunOp :: ((a -> b) -> c -> d) -> (WithMeta a -> b) -> WithMeta c -> WithMeta d
noMetaResFunOp op f (WithMeta x m) = WithMeta (op (metaFun m f) x) m

------------------------------------------------------------------------------------------
-- Class MetaLevel and deriving methods
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
    liftMeta (Rec1 x) = let (WithMeta y m) = liftMeta x in WithMeta (Rec1 y) (addTreeLevel m)
        where addTreeLevel m = m {renameTree = createNode False [renameTree m]}
    dropMeta (WithMeta (Rec1 x) m) = Rec1 (dropMeta (WithMeta x m))

instance MetaLevel U1 where
    liftMeta U1 = noMeta U1
    dropMeta (WithMeta U1 _) = U1

instance MetaLevel (K1 i a) where
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
                         in WithMeta (x' :*: y') (union True m1 m2)
    dropMeta (WithMeta (x :*: y) m) = dropMeta (WithMeta x m) :*: dropMeta (WithMeta y m)

instance MetaLevel Maybe
instance MetaLevel []
instance MetaLevel (Either a)
instance MetaLevel ((,) a)
instance MetaLevel ((->) a) where
    liftMeta f = noMeta (value . f)
    dropMeta f = noMeta . (value f)

instance MetaLevel IO where
    liftMeta x = noMeta $ fmap value x
    dropMeta (WithMeta x m) = fmap (`create` m) x

instance MetaLevel (Map k) where
    liftMeta map = create (Map.fromDistinctAscList $ zip ks vs') m
        where (ks, vs) = unzip $ Map.toAscList map
              (WithMeta vs' m) = liftMeta vs
    dropMeta (WithMeta map m) = Map.map (`create` m) map

instance MetaLevel ReadPrec where
    liftMeta = noMeta . fmap value
    dropMeta = fmap noMeta . value

instance MetaLevel Identity

lift :: (MetaLevel f, Var (f a)) => f (WithMeta a) -> WithMeta (f a)
lift = rename . liftMeta
