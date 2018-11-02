module Nominal.Meta.Data.OldList where

import Data.List
import Nominal.Meta.GHC.Classes
import Nominal.Meta

nlambda_delete :: NLambda_Eq a => WithMeta a -> WithMeta [a] -> WithMeta [a]
nlambda_delete = renameAndApply2 delete

nlambda_elemIndex :: NLambda_Eq a => WithMeta a -> WithMeta [a] -> Maybe Int
nlambda_elemIndex = noMetaRes2ArgOp elemIndex

nlambda_nub :: NLambda_Eq a => WithMeta [a] -> WithMeta [a]
nlambda_nub = idOp nub

nlambda_permutations :: WithMeta [a] -> WithMeta [[a]]
nlambda_permutations = idOp permutations

nlambda_sort :: NLambda_Ord a => WithMeta [a] -> WithMeta [a]
nlambda_sort = idOp sort

nlambda_tails :: WithMeta [a] -> WithMeta [[a]]
nlambda_tails = idOp tails
