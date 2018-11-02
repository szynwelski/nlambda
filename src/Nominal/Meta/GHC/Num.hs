module Nominal.Meta.GHC.Num where

import Nominal.Meta
import Nominal.Variable

class (Var a, Num a) => NLambda_Num a where
    (###+) :: WithMeta a -> WithMeta a -> WithMeta a
    (###+) = renameAndApply2 (+)
    (###-) :: WithMeta a -> WithMeta a -> WithMeta a
    (###-) = renameAndApply2 (-)
    (###*) :: WithMeta a -> WithMeta a -> WithMeta a
    (###*) = renameAndApply2 (*)
    nlambda_negate :: WithMeta a -> WithMeta a
    nlambda_negate = idOp negate
    nlambda_abs :: WithMeta a -> WithMeta a
    nlambda_abs = idOp abs
    nlambda_signum :: WithMeta a -> WithMeta a
    nlambda_signum = idOp signum
    nlambda_fromInteger :: Integer -> WithMeta a
    nlambda_fromInteger = noMeta . fromInteger

instance NLambda_Num Word
instance NLambda_Num Integer
instance NLambda_Num Int

