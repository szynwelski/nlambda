module Nominal.Meta.Text.ParserCombinators.ReadPrec where

import Nominal.Meta
import Nominal.Meta.GHC.Base
import Text.ParserCombinators.ReadPrec

instance NLambda_Applicative ReadPrec

instance NLambda_Functor ReadPrec

instance NLambda_Monad ReadPrec

nlambda_pfail :: WithMeta (ReadPrec a)
nlambda_pfail = noMeta pfail

nlambda_reset :: WithMeta (ReadPrec a) -> WithMeta (ReadPrec a)
nlambda_reset = idOp reset

nlambda_step :: WithMeta (ReadPrec a) -> WithMeta (ReadPrec a)
nlambda_step = idOp step

(###+++) :: WithMeta (ReadPrec a) -> WithMeta (ReadPrec a) -> WithMeta (ReadPrec a)
(###+++) = renameAndApply2 (+++)

(###<++) :: WithMeta (ReadPrec a) -> WithMeta (ReadPrec a) -> WithMeta (ReadPrec a)
(###<++) = renameAndApply2 (<++)

nlambda_prec :: Prec -> WithMeta (ReadPrec a) -> WithMeta (ReadPrec a)
nlambda_prec = rightIdOp prec
