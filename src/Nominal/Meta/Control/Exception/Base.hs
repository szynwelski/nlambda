{-# LANGUAGE MagicHash #-}
module Nominal.Meta.Control.Exception.Base where

import Control.Exception.Base
import GHC.Prim
import Nominal.Meta

nlambda_irrefutPatError :: Addr# -> WithMeta a
nlambda_irrefutPatError = irrefutPatError

nlambda_noMethodBindingError :: Addr# -> WithMeta a
nlambda_noMethodBindingError = noMethodBindingError

nlambda_nonExhaustiveGuardsError :: Addr# -> WithMeta a
nlambda_nonExhaustiveGuardsError = nonExhaustiveGuardsError

nlambda_patError :: Addr# -> WithMeta a
nlambda_patError = patError
