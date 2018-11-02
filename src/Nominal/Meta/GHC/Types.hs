module Nominal.Meta.GHC.Types where

import Nominal.Meta
import Nominal.Variable

(###@@) :: WithMeta [a]
(###@@) = noMeta []

(###:) :: Var a => WithMeta a -> WithMeta [a] -> WithMeta [a]
(###:) = renameAndApply2 (:)
