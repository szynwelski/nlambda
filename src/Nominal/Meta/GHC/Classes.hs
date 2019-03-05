module Nominal.Meta.GHC.Classes where

import Data.Map (Map)
import Data.Set (Set)
import Nominal.Meta
import Nominal.Variable

class (Var a, Eq a) => NLambda_Eq a where
  (###==) :: WithMeta a -> WithMeta a -> Bool
  (###==) = noMetaRes2ArgOp (==)
  (###/=) :: WithMeta a -> WithMeta a -> Bool
  (###/=) = noMetaRes2ArgOp (/=)

instance NLambda_Eq a => NLambda_Eq [a]
instance NLambda_Eq Word
instance NLambda_Eq Ordering
instance NLambda_Eq Int
instance NLambda_Eq Float
instance NLambda_Eq Double
instance NLambda_Eq Char
instance NLambda_Eq Bool
instance NLambda_Eq ()
instance (NLambda_Eq a, NLambda_Eq b) => NLambda_Eq (a, b)
instance (NLambda_Eq a, NLambda_Eq b, NLambda_Eq c) => NLambda_Eq (a, b, c)

class (NLambda_Eq a, Ord a) => NLambda_Ord a where
  nlambda_compare :: WithMeta a -> WithMeta a -> Ordering
  nlambda_compare = noMetaRes2ArgOp compare
  (###<) :: WithMeta a -> WithMeta a -> Bool
  (###<) = noMetaRes2ArgOp (<)
  (###<=) :: WithMeta a -> WithMeta a -> Bool
  (###<=) = noMetaRes2ArgOp (<=)
  (###>) :: WithMeta a -> WithMeta a -> Bool
  (###>) = noMetaRes2ArgOp (>)
  (###>=) :: WithMeta a -> WithMeta a -> Bool
  (###>=) = noMetaRes2ArgOp (>=)
  nlambda_max :: WithMeta a -> WithMeta a -> WithMeta a
  nlambda_max = renameAndApply2 max
  nlambda_min :: WithMeta a -> WithMeta a -> WithMeta a
  nlambda_min = renameAndApply2 min

instance NLambda_Ord a => NLambda_Ord [a]
instance NLambda_Ord Word
instance NLambda_Ord Ordering
instance NLambda_Ord Int
instance NLambda_Ord Float
instance NLambda_Ord Double
instance NLambda_Ord Char
instance NLambda_Ord Bool
instance NLambda_Ord ()
instance (NLambda_Ord a, NLambda_Ord b) => NLambda_Ord (a, b)
instance (NLambda_Ord a, NLambda_Ord b, NLambda_Ord c) => NLambda_Ord (a, b, c)
