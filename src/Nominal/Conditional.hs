{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Nominal.Conditional (Conditional(cond), ite) where

import Nominal.Formula
import Prelude hiding (not)

import GHC.Generics

----------------------------------------------------------------------------------------------------
-- Conditional
----------------------------------------------------------------------------------------------------

-- | Class of types implementing conditional statements.
class Conditional a where
    -- | Join two values or returns two variants for undetermined condition in conditional statement.
    cond :: Formula -> a -> a -> a

    -- Default implementation when the type is generic
    default cond :: (Generic a, GConditional (Rep a)) => Formula -> a -> a -> a
    cond f x y = to (gcond f (from x) (from y))

instance Conditional Formula where
    cond f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

instance Conditional b => Conditional (a -> b) where
    cond c f1 f2 x = cond c (f1 x) (f2 x)


instance Conditional ()
instance (Conditional a, Conditional b) => Conditional (a,b)
instance (Conditional a, Conditional b, Conditional c) => Conditional (a,b,c)
instance (Conditional a, Conditional b, Conditional c, Conditional d) => Conditional (a,b,c,d)
instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e) => Conditional (a,b,c,d,e)
instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e, Conditional f) => Conditional (a,b,c,d,e,f)
instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e, Conditional f, Conditional g) => Conditional (a,b,c,d,e,f,g)

----------------------------------------------------------------------------------------------------
-- if then else with formula solving
----------------------------------------------------------------------------------------------------

-- | /if ... then ... else/ ... with condition solving.
ite :: Conditional a => Formula -> a -> a -> a
ite c x1 x2
    | f == true  = x1
    | f == false = x2
    | otherwise  = cond f x1 x2
  where f = simplifyFormula c

----------------------------------------------------------------------------------------------------
-- Generics
----------------------------------------------------------------------------------------------------

class GConditional f where
    gcond :: Formula -> f a -> f a -> f a

-- The generic instances for the void and unit types are trivial
instance GConditional V1 where gcond _ _ = id
instance GConditional U1 where gcond _ U1 U1 = U1

-- The instances for K1 and M1 are clear as well
instance Conditional c => GConditional (K1 i c)
    where gcond f (K1 x) (K1 y) = K1 (cond f x y)
instance GConditional f => GConditional (M1 i c f)
    where gcond f (M1 x) (M1 y) = M1 (gcond f x y)

-- Then the instance for products is as we expect
instance (GConditional f, GConditional g) => GConditional (f :*: g)
    where gcond f (a1 :*: b1) (a2 :*: b2) = gcond f a1 a2 :*: gcond f b1 b2

-- There is *no* instance for sums, because in general we might not be able to
-- decide the formula and then cannot combine a Left and Right value.

-- This also means that any list instance is an "unsafe" instance which may
-- crash. There used to be an instance for lists, it has been removed now.
