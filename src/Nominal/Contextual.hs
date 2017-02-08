{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Nominal.Contextual where

import Data.Map (Map, assocs, fromList)
import Data.Set (Set, map)
import Nominal.Formula
import Nominal.Formula.Operators
import Nominal.Variable (Variable)
import Prelude hiding (map, not)
import GHC.Generics

----------------------------------------------------------------------------------------------------
-- Contextual
----------------------------------------------------------------------------------------------------

-- | Class of types of expressions to evaluating with a given context.
class Contextual a where
    -- | Evaluates an expression in the context of a given formula.
    when :: Formula -> a -> a

    -- Default implementation when the type is generic
    default when :: (Generic a, GContextual (Rep a)) => Formula -> a -> a
    when f = to . gwhen f . from

-- | Evaluates an expression in the context of a 'true' formula. In practice all formulas in expressions are solved.
--
-- > simplify = when true
simplify :: Contextual a => a -> a
simplify = when true

----------------------------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------------------------

instance Contextual b => Contextual (a -> b) where
    when ctx f = when ctx . f

-- We do not do anything on variables. We could rename variables given the
-- context... But currently we don't
instance Contextual Variable where when = const id

instance Contextual Formula where
    when ctx f
        | isTrue ctx = simplifyFormula f
        | isTrue (ctx ==> f) = true
        | isTrue (ctx ==> not f) = false
        | otherwise = simplifyFormula $ mapFormula (when ctx) f

-- Trivial instances, elements cannot be simplified in some context
instance Contextual Bool where when = const id
instance Contextual Char where when = const id
instance Contextual Double where when = const id
instance Contextual Float where when = const id
instance Contextual Int where when = const id
instance Contextual Integer where when = const id
instance Contextual Ordering where when = const id

instance Contextual ()
instance (Contextual a, Contextual b) => Contextual (a,b)
instance (Contextual a, Contextual b, Contextual c) => Contextual (a,b,c)
instance (Contextual a, Contextual b, Contextual c, Contextual d) => Contextual (a,b,c,d)
instance (Contextual a, Contextual b, Contextual c, Contextual d, Contextual e) => Contextual (a,b,c,d,e)
instance (Contextual a, Contextual b, Contextual c, Contextual d, Contextual e, Contextual f) => Contextual (a,b,c,d,e,f)
instance (Contextual a, Contextual b, Contextual c, Contextual d, Contextual e, Contextual f, Contextual g) => Contextual (a,b,c,d,e,f,g)

instance Contextual a => Contextual [a]
instance Contextual a => Contextual (Maybe a)
instance (Contextual a, Contextual b) => Contextual (Either a b)

instance (Contextual a, Ord a) => Contextual (Set a) where
    when ctx = map (when ctx)

instance (Contextual k, Contextual v, Ord k) => Contextual (Map k v) where
    when ctx = fromList . when ctx . assocs

----------------------------------------------------------------------------------------------------
-- Generics
----------------------------------------------------------------------------------------------------

class GContextual f where
    gwhen :: Formula -> f a -> f a

-- As always, the void and unit instances are easy
instance GContextual V1 where gwhen f = id
instance GContextual U1 where gwhen f = id

-- Then the constant and constructor case
instance Contextual c => GContextual (K1 i c) where
    gwhen f = K1 . when f . unK1
instance GContextual f => GContextual (M1 i c f) where
    gwhen f = M1 . gwhen f . unM1

-- Then the "interesting" cases: sum, product and composition
instance (GContextual f, GContextual g) => GContextual (f :+: g) where
    gwhen f (L1 x) = L1 (gwhen f x)
    gwhen f (R1 x) = R1 (gwhen f x)
instance (GContextual f, GContextual g) => GContextual (f :*: g) where
    gwhen f (x :*: y) = gwhen f x :*: gwhen f y
