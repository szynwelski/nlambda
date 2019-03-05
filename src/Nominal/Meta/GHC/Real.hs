module Nominal.Meta.GHC.Real where

import Data.Ratio (Ratio)
import Nominal.Meta.GHC.Classes
import Nominal.Meta.GHC.Enum
import Nominal.Meta.GHC.Integer.Type
import Nominal.Meta.GHC.Num
import Nominal.Meta

class (NLambda_Num a, Fractional a) => NLambda_Fractional a where
    (###/) :: WithMeta a -> WithMeta a -> WithMeta a
    (###/) = renameAndApply2 (/)
    nlambda_recip :: WithMeta a -> WithMeta a
    nlambda_recip = idOp recip
    nlambda_fromRational :: Rational -> WithMeta a
    nlambda_fromRational = noMeta . fromRational

class (NLambda_Real a, NLambda_Enum a, Integral a) => NLambda_Integral a where
    nlambda_quot :: WithMeta a -> WithMeta a -> WithMeta a
    nlambda_quot = renameAndApply2 quot
    nlambda_rem :: WithMeta a -> WithMeta a -> WithMeta a
    nlambda_rem = renameAndApply2 rem
    nlambda_div :: WithMeta a -> WithMeta a -> WithMeta a
    nlambda_div = renameAndApply2 div
    nlambda_mod :: WithMeta a -> WithMeta a -> WithMeta a
    nlambda_mod = renameAndApply2 mod
    nlambda_quotRem :: WithMeta a -> WithMeta a -> WithMeta (a, a)
    nlambda_quotRem = renameAndApply2 quotRem
    nlambda_divMod :: WithMeta a -> WithMeta a -> WithMeta (a, a)
    nlambda_divMod = renameAndApply2 divMod
    nlambda_toInteger :: WithMeta a -> Integer
    nlambda_toInteger = noMetaResOp toInteger

instance NLambda_Integral Word
instance NLambda_Integral Integer
instance NLambda_Integral Int

instance NLambda_Integral a => NLambda_Num (Ratio a)
instance NLambda_Integral a => NLambda_Fractional (Ratio a)

class (NLambda_Num a, NLambda_Ord a, Real a) => NLambda_Real a where
    nlambda_toRational :: WithMeta a -> Rational
    nlambda_toRational = noMetaResOp toRational

instance NLambda_Real Word
instance NLambda_Real Integer
instance NLambda_Real Int

class (NLambda_Real a, NLambda_Fractional a, RealFrac a) => NLambda_RealFrac a where
    nlambda_properFraction :: NLambda_Integral b => WithMeta a -> WithMeta (b, a)
    nlambda_properFraction = idOp properFraction
    nlambda_truncate :: NLambda_Integral b => WithMeta a -> WithMeta b
    nlambda_truncate = idOp truncate
    nlambda_round :: NLambda_Integral b => WithMeta a -> WithMeta b
    nlambda_round = idOp round
    nlambda_ceiling :: NLambda_Integral b => WithMeta a -> WithMeta b
    nlambda_ceiling = idOp ceiling
    nlambda_floor :: NLambda_Integral b => WithMeta a -> WithMeta b
    nlambda_floor = idOp floor

(###^) :: (NLambda_Integral b, NLambda_Num a) => WithMeta a -> WithMeta b -> WithMeta a
(###^) = renameAndApply2 (^)

(###^^) :: (NLambda_Fractional a, NLambda_Integral b) => WithMeta a -> WithMeta b -> WithMeta a
(###^^) = renameAndApply2 (^^)

nlambda_even :: NLambda_Integral a => WithMeta a -> Bool
nlambda_even = even . value

nlambda_odd :: NLambda_Integral a => WithMeta a -> Bool
nlambda_odd = odd . value
