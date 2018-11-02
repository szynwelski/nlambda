module Nominal.Meta.System.IO where

import Nominal.Meta.GHC.Show
import Nominal.Meta

nlambda_print :: NLambda_Show a => WithMeta a -> IO ()
nlambda_print = print . value
