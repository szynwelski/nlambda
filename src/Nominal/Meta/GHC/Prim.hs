module Nominal.Meta.GHC.Prim where

import Nominal.Meta

nlambda_seq :: WithMeta a -> WithMeta b -> WithMeta b
nlambda_seq = rightIdOp seq
