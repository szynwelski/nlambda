module Nominal.Meta.GHC.Float where

import Nominal.Meta.GHC.Enum
import Nominal.Meta.GHC.Num
import Nominal.Meta.GHC.Real
import Nominal.Meta.GHC.Show
import Nominal.Meta

instance NLambda_Enum Float
instance NLambda_Enum Double

class (Floating a, NLambda_Fractional a) => NLambda_Floating a where
  nlambda_pi :: WithMeta a
  nlambda_pi = noMeta pi
  nlambda_exp :: WithMeta a -> WithMeta a
  nlambda_exp = idOp exp
  nlambda_log :: WithMeta a -> WithMeta a
  nlambda_log = idOp log
  nlambda_sqrt :: WithMeta a -> WithMeta a
  nlambda_sqrt = idOp sqrt
  (###**) :: WithMeta a -> WithMeta a -> WithMeta a
  (###**) = renameAndApply2 (**)
  nlambda_logBase :: WithMeta a -> WithMeta a -> WithMeta a
  nlambda_logBase = renameAndApply2 logBase
  nlambda_sin :: WithMeta a -> WithMeta a
  nlambda_sin = idOp sin
  nlambda_cos :: WithMeta a -> WithMeta a
  nlambda_cos = idOp cos
  nlambda_tan :: WithMeta a -> WithMeta a
  nlambda_tan = idOp tan
  nlambda_asin :: WithMeta a -> WithMeta a
  nlambda_asin = idOp asin
  nlambda_acos :: WithMeta a -> WithMeta a
  nlambda_acos = idOp acos
  nlambda_atan :: WithMeta a -> WithMeta a
  nlambda_atan = idOp atan
  nlambda_sinh :: WithMeta a -> WithMeta a
  nlambda_sinh = idOp sinh
  nlambda_cosh :: WithMeta a -> WithMeta a
  nlambda_cosh = idOp cosh
  nlambda_tanh :: WithMeta a -> WithMeta a
  nlambda_tanh = idOp tanh
  nlambda_asinh :: WithMeta a -> WithMeta a
  nlambda_asinh = idOp asinh
  nlambda_acosh :: WithMeta a -> WithMeta a
  nlambda_acosh = idOp acosh
  nlambda_atanh :: WithMeta a -> WithMeta a
  nlambda_atanh = idOp atanh

instance NLambda_Floating Float
instance NLambda_Floating Double

instance NLambda_Fractional Float
instance NLambda_Fractional Double

instance NLambda_Num Float
instance NLambda_Num Double

instance NLambda_Real Float
instance NLambda_Real Double

class (RealFloat a, NLambda_RealFrac a, NLambda_Floating a) => NLambda_RealFloat a where
  nlambda_floatRadix :: WithMeta a -> Integer
  nlambda_floatRadix = noMetaResOp floatRadix
  nlambda_floatDigits :: WithMeta a -> Int
  nlambda_floatDigits = noMetaResOp floatDigits
  nlambda_floatRange :: WithMeta a -> (Int, Int)
  nlambda_floatRange = noMetaResOp floatRange
  nlambda_decodeFloat :: WithMeta a -> (Integer, Int)
  nlambda_decodeFloat = noMetaResOp decodeFloat
  nlambda_encodeFloat :: Integer -> Int -> WithMeta a
  nlambda_encodeFloat x = noMeta . encodeFloat x
  nlambda_exponent :: WithMeta a -> Int
  nlambda_exponent = noMetaResOp exponent
  nlambda_significand :: WithMeta a -> WithMeta a
  nlambda_significand = idOp significand
  nlambda_scaleFloat :: Int -> WithMeta a -> WithMeta a
  nlambda_scaleFloat = rightIdOp scaleFloat
  nlambda_isNaN :: WithMeta a -> Bool
  nlambda_isNaN = noMetaResOp isNaN
  nlambda_isInfinite :: WithMeta a -> Bool
  nlambda_isInfinite = noMetaResOp isInfinite
  nlambda_isDenormalized :: WithMeta a -> Bool
  nlambda_isDenormalized = noMetaResOp isDenormalized
  nlambda_isNegativeZero :: WithMeta a -> Bool
  nlambda_isNegativeZero = noMetaResOp isNegativeZero
  nlambda_isIEEE :: WithMeta a -> Bool
  nlambda_isIEEE = noMetaResOp isIEEE
  nlambda_atan2 :: WithMeta a -> WithMeta a -> WithMeta a
  nlambda_atan2 = renameAndApply2 atan2

instance NLambda_RealFloat Float
instance NLambda_RealFloat Double

instance NLambda_RealFrac Float
instance NLambda_RealFrac Double

instance NLambda_Show Float
instance NLambda_Show Double
