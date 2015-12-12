module Nominal.Conditional (Conditional(ite), ite') where

import Nominal.Formula
import Prelude hiding (not)

----------------------------------------------------------------------------------------------------
-- Conditional
----------------------------------------------------------------------------------------------------

class Conditional a where
    ite :: Formula -> a -> a -> a

instance Conditional Formula where
    ite f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

instance Conditional b => Conditional (a -> b) where
    ite c f1 f2 = \x -> ite c (f1 x) (f2 x)

instance (Conditional a) => Conditional [a] where
    ite c l1 l2 = if length l1 == length l2
                  then zipWith (ite c) l1 l2
                  else ifSolve c l1 l2 (error "ite cannot be applied to lists of different sizes with unsolvable condition")

instance Conditional () where
    ite c _ _ = ()

instance (Conditional a, Conditional b) => Conditional (a, b) where
    ite c (a1, b1) (a2, b2) = ((ite c a1 a2), (ite c b1 b2))

instance (Conditional a, Conditional b, Conditional c) => Conditional (a, b, c) where
    ite c (a1, b1, c1) (a2, b2, c2) = ((ite c a1 a2), (ite c b1 b2), (ite c c1 c2))

----------------------------------------------------------------------------------------------------
-- if then else with solving formula
----------------------------------------------------------------------------------------------------

ifSolve :: Formula -> a -> a -> a -> a
ifSolve c x1 x2 x3 = case solve c of
                       Just True -> x1
                       Just False -> x2
                       Nothing -> x3

ite' :: Conditional a => Formula -> a -> a -> a
ite' c x1 x2 = ifSolve c x1 x2 (ite c x1 x2)
