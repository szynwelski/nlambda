module Nominal.Meta.Debug.Trace where

import Debug.Trace (trace)
import Nominal.Meta

nlambda_trace :: String -> WithMeta a -> WithMeta a
nlambda_trace = trace
