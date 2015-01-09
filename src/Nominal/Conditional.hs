module Nominal.Conditional where

import Formula
import Prelude hiding (not)

----------------------------------------------------------------------------------------------------
-- Conditional
----------------------------------------------------------------------------------------------------

class Conditional a where
    iF :: Formula -> a -> a -> a

instance Conditional Formula where
    iF f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

instance Conditional b => Conditional (a -> b) where
    iF c f1 f2 = \x -> iF c (f1 x) (f2 x)

instance (Conditional a) => Conditional [a] where
    iF c l1 l2 = zipWith (iF c) l1 l2

instance Conditional () where
    iF c _ _ = ()

instance (Conditional a, Conditional b) => Conditional (a, b) where
    iF c (a1, b1) (a2, b2) = ((iF c a1 a2), (iF c b1 b2))

instance (Conditional a, Conditional b, Conditional c) => Conditional (a, b, c) where
    iF c (a1, b1, c1) (a2, b2, c2) = ((iF c a1 a2), (iF c b1 b2), (iF c c1 c2))

