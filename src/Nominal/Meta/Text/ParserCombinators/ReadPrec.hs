module Nominal.Meta.Text.ParserCombinators.ReadPrec where

instance NLambda_Applicative ReadPrec

instance NLambda_Functor ReadPrec

instance NLambda_Monad ReadPrec

nlambda_pfail :: ReadPrec a
nlambda_pfail = noMeta pfail

nlambda_reset :: ReadPrec a -> ReadPrec a
nlambda_reset = idOp reset

nlambda_step :: ReadPrec a -> ReadPrec a
nlambda_step = idOp step

(###+++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
(###+++) = renameAndApply2 (+++)

(###<++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
(###<++) = renameAndApply2 (<++)

nlambda_prec :: Prec -> ReadPrec a -> ReadPrec a
nlambda_prec = rightIdOp prec
