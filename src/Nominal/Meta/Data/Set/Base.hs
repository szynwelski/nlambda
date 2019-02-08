module Nominal.Meta.Data.Set.Base where

import Data.Set
import Nominal.Meta hiding (union, unions)
import Nominal.Meta.Data.Foldable as Foldable
import Nominal.Meta.GHC.Base as Base
import Nominal.Meta.GHC.Classes
import Nominal.Meta.GHC.Show
import Nominal.Variable
import Prelude hiding (filter, null)

instance NLambda_Show a => NLambda_Show (Set a)

instance (Ord a, NLambda_Eq a) => NLambda_Eq (Set a)

instance NLambda_Ord a => NLambda_Ord (Set a)

nlambda_delete :: NLambda_Ord a => WithMeta a -> WithMeta (Set a) -> WithMeta (Set a)
nlambda_delete = renameAndApply2 delete

nlambda_deleteMax :: WithMeta (Set a) -> WithMeta (Set a)
nlambda_deleteMax = idOp deleteMax

nlambda_deleteMin :: WithMeta (Set a) -> WithMeta (Set a)
nlambda_deleteMin = idOp deleteMin

nlambda_difference :: NLambda_Ord a => WithMeta (Set a) -> WithMeta (Set a) -> WithMeta (Set a)
nlambda_difference = renameAndApply2 difference

nlambda_disjoint :: NLambda_Ord a => WithMeta (Set a) -> WithMeta (Set a) -> Bool
nlambda_disjoint = noMetaRes2ArgOp disjoint

nlambda_elems :: WithMeta (Set a) -> WithMeta [a]
nlambda_elems = idOp elems

nlambda_empty :: WithMeta (Set a)
nlambda_empty = noMeta empty

nlambda_filter :: (WithMeta a -> Bool) -> WithMeta (Set a) -> WithMeta (Set a)
nlambda_filter = noMetaResFunOp filter

nlambda_findMax :: WithMeta (Set a) -> WithMeta a
nlambda_findMax = idOp findMax

nlambda_findMin :: WithMeta (Set a) -> WithMeta a
nlambda_findMin = idOp findMin

nlambda_foldr :: (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta (Set a) -> WithMeta b
nlambda_foldr f x (WithMeta s m) = Foldable.nlambda_foldr f x $ create (toAscList s) m

nlambda_foldl :: (WithMeta a -> WithMeta b -> WithMeta a) -> WithMeta a -> WithMeta (Set b) -> WithMeta a
nlambda_foldl f x (WithMeta s m) = Foldable.nlambda_foldl f x $ create (toAscList s) m

nlambda_fromAscList :: NLambda_Eq a => WithMeta [a] -> WithMeta (Set a)
nlambda_fromAscList = idOp fromAscList

nlambda_fromDescList :: NLambda_Eq a => WithMeta [a] -> WithMeta (Set a)
nlambda_fromDescList = idOp fromDescList

nlambda_fromDistinctAscList :: WithMeta [a] -> WithMeta (Set a)
nlambda_fromDistinctAscList = idOp fromDistinctAscList

nlambda_fromDistinctDescList :: WithMeta [a] -> WithMeta (Set a)
nlambda_fromDistinctDescList = idOp fromDistinctDescList

nlambda_fromList :: NLambda_Ord a => WithMeta [a] -> WithMeta (Set a)
nlambda_fromList = idOp fromList

nlambda_insert :: NLambda_Ord a => WithMeta a -> WithMeta (Set a) -> WithMeta (Set a)
nlambda_insert = renameAndApply2 insert

nlambda_intersection :: NLambda_Ord a => WithMeta (Set a) -> WithMeta (Set a) -> WithMeta (Set a)
nlambda_intersection = renameAndApply2 intersection

nlambda_isProperSubsetOf :: NLambda_Ord a => WithMeta (Set a) -> WithMeta (Set a) -> Bool
nlambda_isProperSubsetOf = noMetaRes2ArgOp isProperSubsetOf

nlambda_isSubsetOf :: NLambda_Ord a => WithMeta (Set a) -> WithMeta (Set a) -> Bool
nlambda_isSubsetOf = noMetaRes2ArgOp isSubsetOf

nlambda_map :: (Var b, NLambda_Ord b) => (WithMeta a -> WithMeta b) -> WithMeta (Set a) -> WithMeta (Set b)
nlambda_map f (WithMeta s m) = create (fromAscList list) m'
    where (WithMeta list m') = Base.nlambda_map f $ create (toAscList s) m

nlambda_member :: NLambda_Ord a => WithMeta a -> WithMeta (Set a) -> Bool
nlambda_member = noMetaRes2ArgOp member

nlambda_notMember :: NLambda_Ord a => WithMeta a -> WithMeta (Set a) -> Bool
nlambda_notMember = noMetaRes2ArgOp notMember

nlambda_null :: WithMeta (Set a) -> Bool
nlambda_null = null . value

nlambda_partition :: (WithMeta a -> Bool) -> WithMeta (Set a) -> WithMeta (Set a, Set a)
nlambda_partition = noMetaResFunOp partition

nlambda_size :: WithMeta (Set a) -> Int
nlambda_size = size . value

nlambda_singleton :: WithMeta a -> WithMeta (Set a)
nlambda_singleton = idOp singleton

nlambda_split :: NLambda_Ord a => WithMeta a -> WithMeta (Set a) -> WithMeta (Set a, Set a)
nlambda_split = renameAndApply2 split

nlambda_toAscList :: WithMeta (Set a) -> WithMeta [a]
nlambda_toAscList = idOp toAscList

nlambda_toDescList :: WithMeta (Set a) -> WithMeta [a]
nlambda_toDescList = idOp toDescList

nlambda_toList :: WithMeta (Set a) -> WithMeta [a]
nlambda_toList = idOp toList

nlambda_union :: NLambda_Ord a => WithMeta (Set a) -> WithMeta (Set a) -> WithMeta (Set a)
nlambda_union = renameAndApply2 union

nlambda_unions :: (NLambda_Foldable f, NLambda_Ord a) => WithMeta (f (Set a)) -> WithMeta (Set a)
nlambda_unions = idOp unions
