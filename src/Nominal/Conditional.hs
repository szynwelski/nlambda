module Nominal.Conditional (Conditional(cond), ite) where

import Nominal.Formula
import Prelude hiding (not)

----------------------------------------------------------------------------------------------------
-- Conditional
----------------------------------------------------------------------------------------------------

-- | Class of types implementing conditional statements.
class Conditional a where
    -- | Join two values or returns two variants for undetermined condition in conditional statement.
    cond :: Formula -> a -> a -> a

instance Conditional Formula where
    cond f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

instance Conditional b => Conditional (a -> b) where
    cond c f1 f2 = \x -> cond c (f1 x) (f2 x)

instance (Conditional a) => Conditional [a] where
    cond c l1 l2 = if length l1 == length l2
                           then zipWith (cond c) l1 l2
                           else ifSolve c l1 l2 (error "cond cannot be applied to lists of different sizes with unsolvable condition")

instance Conditional () where
    cond c _ _ = ()

instance (Conditional a, Conditional b) => Conditional (a,b) where
    cond c (a1,b1) (a2,b2) = ((cond c a1 a2), (cond c b1 b2))

instance (Conditional a, Conditional b, Conditional c) => Conditional (a,b,c) where
    cond c (a1,b1,c1) (a2,b2,c2) = ((cond c a1 a2), (cond c b1 b2), (cond c c1 c2))

instance (Conditional a, Conditional b, Conditional c, Conditional d) => Conditional (a,b,c,d) where
    cond c (a1,b1,c1,d1) (a2,b2,c2,d2) = ((cond c a1 a2), (cond c b1 b2), (cond c c1 c2), (cond c d1 d2))

instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e) => Conditional (a,b,c,d,e) where
    cond c (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = ((cond c a1 a2), (cond c b1 b2), (cond c c1 c2), (cond c d1 d2), (cond c e1 e2))

instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e, Conditional f) => Conditional (a,b,c,d,e,f) where
    cond c (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) = ((cond c a1 a2), (cond c b1 b2), (cond c c1 c2),
                                                      (cond c d1 d2), (cond c e1 e2), (cond c f1 f2))

instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e, Conditional f, Conditional g) => Conditional (a,b,c,d,e,f,g) where
    cond c (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) = ((cond c a1 a2), (cond c b1 b2), (cond c c1 c2), (cond c d1 d2),
                                                            (cond c e1 e2), (cond c f1 f2), (cond c g1 g2))

instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e, Conditional f, Conditional g, Conditional h) => Conditional (a,b,c,d,e,f,g,h) where
    cond c (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) = ((cond c a1 a2), (cond c b1 b2), (cond c c1 c2), (cond c d1 d2),
                                                                  (cond c e1 e2), (cond c f1 f2), (cond c g1 g2), (cond c h1 h2))

----------------------------------------------------------------------------------------------------
-- if then else with formula solving
----------------------------------------------------------------------------------------------------

ifSolve :: Formula -> a -> a -> a -> a
ifSolve c x1 x2 x3 = case solve c of
                       Just True -> x1
                       Just False -> x2
                       Nothing -> x3

-- | /if ... then ... else/ ... with condition solving.
ite :: Conditional a => Formula -> a -> a -> a
ite c x1 x2 = ifSolve c x1 x2 (cond c x1 x2)
