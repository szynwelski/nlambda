module Nominal.Contextual where

import Data.Map (Map, assocs, fromList)
import Data.Set (Set, map)
import Nominal.Formula
import Nominal.Variable (Variable)
import Prelude hiding (map, not)

----------------------------------------------------------------------------------------------------
-- Contextual
----------------------------------------------------------------------------------------------------

-- | Class of types of expressions to evaluating with a given context.
class Contextual a where
    -- | Evaluates an expression in the context of a given formula.
    when :: Formula -> a -> a
    when = const id

-- | Evaluates an expression in the context of a 'true' formula. In practice all formulas in expressions are solved.
--
-- > simplify = when true
simplify :: Contextual a => a -> a
simplify = when true

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance Contextual b => Contextual (a -> b) where
    when ctx f = \x -> when ctx (f x)

instance Contextual Variable

instance Contextual Formula where
    when ctx f
        | isTrue (ctx ==> f) = true
        | isTrue (ctx ==> not f) = false
        | otherwise = f

instance Contextual Bool

instance Contextual Char

instance Contextual Double

instance Contextual Float

instance Contextual Int

instance Contextual Integer

instance Contextual Ordering

instance Contextual a => Contextual [a] where
    when ctx = fmap (when ctx)

instance Contextual ()

instance (Contextual a, Contextual b) => Contextual (a, b) where
    when ctx (a, b) = (when ctx a, when ctx b)

instance (Contextual a, Contextual b, Contextual c) => Contextual (a, b, c) where
    when ctx (a, b, c) = (when ctx a, when ctx b, when ctx c)

instance Contextual a => Contextual (Maybe a) where
    when ctx = fmap (when ctx)

instance (Contextual a, Contextual b) => Contextual (Either a b) where
    when ctx (Left v) = Left $ when ctx v
    when ctx (Right v) = Right $ when ctx v

instance (Contextual a, Ord a) => Contextual (Set a) where
    when ctx = map (when ctx)

instance (Contextual k, Contextual v, Ord k, Ord v) => Contextual (Map k v) where
    when ctx = fromList . when ctx . assocs

