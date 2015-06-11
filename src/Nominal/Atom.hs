module Nominal.Atom where

import Nominal.Formula
import Nominal.Variable
import Nominal.Variants

----------------------------------------------------------------------------------------------------
-- Atom
----------------------------------------------------------------------------------------------------

type Atom = Variants Variable

atom :: String -> Atom
atom = variant . variable

lt :: Atom -> Atom -> Formula
lt = variantsRelation lessThan

le :: Atom -> Atom -> Formula
le = variantsRelation lessEquals

gt :: Atom -> Atom -> Formula
gt = variantsRelation greaterThan

ge :: Atom -> Atom -> Formula
ge = variantsRelation greaterEquals
