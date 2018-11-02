module Nominal.Meta.GHC.Err where

import Nominal.Meta

nlambda_undefined :: WithMeta a
nlambda_undefined = undefined

nlambda_error :: [Char] -> WithMeta a
nlambda_error = noMeta . error
