module Nominal.Atom where

import Nominal.Formula
import Nominal.Variable
import Nominal.Variants

----------------------------------------------------------------------------------------------------
-- Atom
----------------------------------------------------------------------------------------------------

-- | Variants of variables.
type Atom = Variants Variable

-- | Creates atom with the given name.
atom :: String -> Atom
atom = variant . variable

-- | Creates a formula that describes the "<" relation between given atoms.
--
-- > lt a a == false
-- > lt a b == gt b a
lt :: Atom -> Atom -> Formula
lt = variantsRelation lessThan

-- | Creates a formula that describes the "≤" relation between given atoms.
--
-- > le a a == true
-- > le a b == ge b a
le :: Atom -> Atom -> Formula
le = variantsRelation lessEquals

-- | Creates a formula that describes the ">" relation between given atoms.
--
-- > gt a a == false
-- > gt a b == lt b a
gt :: Atom -> Atom -> Formula
gt = variantsRelation greaterThan

-- | Creates a formula that describes the "≥" relation between given atoms.
--
-- > ge a a == true
-- > ge a b == le b a
ge :: Atom -> Atom -> Formula
ge = variantsRelation greaterEquals
