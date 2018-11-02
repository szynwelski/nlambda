module Nominal.Meta.GHC.Enum where

import Nominal.Meta
import Nominal.Variable

class Bounded a => NLambda_Bounded a where
  nlambda_minBound :: WithMeta a
  nlambda_minBound = noMeta minBound
  nlambda_maxBound :: WithMeta a
  nlambda_maxBound = noMeta maxBound

instance NLambda_Bounded Word
instance NLambda_Bounded Ordering
instance NLambda_Bounded Int
instance NLambda_Bounded Char
instance NLambda_Bounded Bool
instance NLambda_Bounded ()
instance (NLambda_Bounded a, NLambda_Bounded b) => NLambda_Bounded (a, b)
instance (NLambda_Bounded a, NLambda_Bounded b, NLambda_Bounded c) => NLambda_Bounded (a, b, c)

class (Var a, Enum a) => NLambda_Enum a where
  nlambda_succ :: WithMeta a -> WithMeta a
  nlambda_succ = idOp succ
  nlambda_pred :: WithMeta a -> WithMeta a
  nlambda_pred = idOp pred
  nlambda_toEnum :: Int -> WithMeta a
  nlambda_toEnum = noMeta . toEnum
  nlambda_fromEnum :: WithMeta a -> Int
  nlambda_fromEnum = noMetaResOp fromEnum
  nlambda_enumFrom :: WithMeta a -> WithMeta [a]
  nlambda_enumFrom = idOp enumFrom
  nlambda_enumFromThen :: WithMeta a -> WithMeta a -> WithMeta [a]
  nlambda_enumFromThen = renameAndApply2 enumFromThen
  nlambda_enumFromTo :: WithMeta a -> WithMeta a -> WithMeta [a]
  nlambda_enumFromTo = renameAndApply2 enumFromTo
  nlambda_enumFromThenTo :: WithMeta a -> WithMeta a -> WithMeta a -> WithMeta [a]
  nlambda_enumFromThenTo = renameAndApply3 enumFromThenTo

instance NLambda_Enum Word
instance NLambda_Enum Ordering
instance NLambda_Enum Integer
instance NLambda_Enum Int
instance NLambda_Enum Char
instance NLambda_Enum Bool
instance NLambda_Enum ()
