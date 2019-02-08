module Nominal.Meta.Data.Map.Internal where

import Data.Map
import Nominal.Meta hiding (union)
import Nominal.Meta.GHC.Base
import Nominal.Meta.GHC.Classes
import Nominal.Meta.GHC.Read
import Nominal.Meta.GHC.Show
import Nominal.Meta.GHC.Tuple
import Nominal.Variable
import Prelude hiding (filter, lookup, map, null)

instance (Ord k, NLambda_Eq k, NLambda_Eq a) => NLambda_Eq (Map k a)

instance NLambda_Functor (Map k)

instance (NLambda_Ord k, NLambda_Ord a) => NLambda_Ord (Map k a)

instance (NLambda_Ord k, NLambda_Read k, NLambda_Read a) => NLambda_Read (Map k a)

instance (NLambda_Show k, NLambda_Show a) => NLambda_Show (Map k a)

nlambda_assocs :: WithMeta (Map k a) -> WithMeta [(k, a)]
nlambda_assocs = idOp assocs

nlambda_elems :: WithMeta (Map k a) -> WithMeta [a]
nlambda_elems = idOp elems

nlambda_empty :: WithMeta (Map k a)
nlambda_empty = noMeta empty

nlambda_delete :: (Var a, NLambda_Ord k) => WithMeta k -> WithMeta (Map k a) -> WithMeta (Map k a)
nlambda_delete = renameAndApply2 delete

nlambda_difference :: (Var a, Var b, NLambda_Ord k) => WithMeta (Map k a) -> WithMeta (Map k b) -> WithMeta (Map k a)
nlambda_difference = renameAndApply2 difference

nlambda_filter :: (WithMeta a -> Bool) -> WithMeta (Map k a) -> WithMeta (Map k a)
nlambda_filter = noMetaResFunOp filter

nlambda_filterWithKey :: (WithMeta k -> WithMeta a -> Bool) -> WithMeta (Map k a) -> WithMeta (Map k a)
nlambda_filterWithKey f (WithMeta map m) = create map' m
    where map' = filterWithKey (\k v -> f (create k m) (create v m)) map

nlambda_findWithDefault :: (Var a, NLambda_Ord k) => WithMeta a -> WithMeta k -> WithMeta (Map k a) -> WithMeta a
nlambda_findWithDefault = renameAndApply3 findWithDefault

nlambda_fromAscList :: NLambda_Eq k => WithMeta [(k, a)] -> WithMeta (Map k a)
nlambda_fromAscList = idOp fromAscList

nlambda_fromDescList :: NLambda_Eq k => WithMeta [(k, a)] -> WithMeta (Map k a)
nlambda_fromDescList = idOp fromDescList

nlambda_fromDistinctAscList :: WithMeta [(k, a)] -> WithMeta (Map k a)
nlambda_fromDistinctAscList = idOp fromDistinctAscList

nlambda_fromList :: NLambda_Ord k => WithMeta [(k, a)] -> WithMeta (Map k a)
nlambda_fromList = idOp fromList

nlambda_fromListWith :: (Var a, NLambda_Ord k) => (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta [(k, a)] -> WithMeta (Map k a)
nlambda_fromListWith f l = lift $ fromListWith f (dropMeta <$> dropMeta l)

nlambda_intersection :: (Var a, Var b, NLambda_Ord k) => WithMeta (Map k a) -> WithMeta (Map k b) -> WithMeta (Map k a)
nlambda_intersection = renameAndApply2 intersection

nlambda_insert :: (Var a, NLambda_Ord k) => WithMeta k -> WithMeta a -> WithMeta (Map k a) -> WithMeta (Map k a)
nlambda_insert = renameAndApply3 insert

nlambda_insertWith :: (Var a, NLambda_Ord k) => (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta k -> WithMeta a -> WithMeta (Map k a) -> WithMeta (Map k a)
nlambda_insertWith f k v = lift . insertWith f k' (create v' m) . dropMeta
    where WithMeta (k', v') m = k #### v

nlambda_keys :: WithMeta (Map k a) -> WithMeta [k]
nlambda_keys = idOp keys

nlambda_lookup :: (Var a, NLambda_Ord k) => WithMeta k -> WithMeta (Map k a) -> WithMeta (Maybe a)
nlambda_lookup = renameAndApply2 lookup

nlambda_map :: (Var b, Var k, Ord k) => (WithMeta a -> WithMeta b) -> WithMeta (Map k a) -> WithMeta (Map k b)
nlambda_map f = lift . map f . dropMeta

nlambda_mapKeysWith :: (Var a, NLambda_Ord k2) => (WithMeta a -> WithMeta a -> WithMeta a) -> (WithMeta k1 -> WithMeta k2) -> WithMeta (Map k1 a) -> WithMeta (Map k2 a)
nlambda_mapKeysWith fv fk (WithMeta map m) = nlambda_fromListWith fv list
    where list = lift $ fmap (\(k, v) -> let WithMeta k' m' = fk $ create k m in create (k', v) m') (assocs map)

nlambda_member :: (Var a, NLambda_Ord k) => WithMeta k -> WithMeta (Map k a) -> Bool
nlambda_member = noMetaRes2ArgOp member

nlambda_notMember :: (Var a, NLambda_Ord k) => WithMeta k -> WithMeta (Map k a) -> Bool
nlambda_notMember = noMetaRes2ArgOp notMember

nlambda_null :: WithMeta (Map k a) -> Bool
nlambda_null = null . value

nlambda_partition :: (WithMeta a -> Bool) -> WithMeta (Map k a) -> WithMeta (Map k a, Map k a)
nlambda_partition = noMetaResFunOp partition

nlambda_singleton :: (Var k, Var a) => WithMeta k -> WithMeta a -> WithMeta (Map k a)
nlambda_singleton = renameAndApply2 singleton

nlambda_size :: WithMeta (Map k a) -> Int
nlambda_size = size . value

nlambda_toAscList :: WithMeta (Map k a) -> WithMeta [(k, a)]
nlambda_toAscList = idOp toAscList

nlambda_toList :: WithMeta (Map k a) -> WithMeta [(k, a)]
nlambda_toList = idOp toList

nlambda_union :: (Var a, NLambda_Ord k) => WithMeta (Map k a) -> WithMeta (Map k a) -> WithMeta (Map k a)
nlambda_union = renameAndApply2 union

nlambda_unionWith :: (Var a, NLambda_Ord k) => (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta (Map k a) -> WithMeta (Map k a) -> WithMeta (Map k a)
nlambda_unionWith f m1 m2 = lift $ unionWith f (dropMeta m1) (dropMeta m2)
