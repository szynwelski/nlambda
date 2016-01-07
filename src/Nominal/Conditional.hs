module Nominal.Conditional (Conditional(ifUnsolved), ite) where

import Nominal.Formula
import Prelude hiding (not)

----------------------------------------------------------------------------------------------------
-- Conditional
----------------------------------------------------------------------------------------------------

-- | Class of types implementing conditional expression.
class Conditional a where
    -- | /if ... then ... else .../ in the case of unsolved condition
    ifUnsolved :: Formula -> a -> a -> a

instance Conditional Formula where
    ifUnsolved f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

instance Conditional b => Conditional (a -> b) where
    ifUnsolved c f1 f2 = \x -> ifUnsolved c (f1 x) (f2 x)

instance (Conditional a) => Conditional [a] where
    ifUnsolved c l1 l2 = if length l1 == length l2
                           then zipWith (ifUnsolved c) l1 l2
                           else ifSolve c l1 l2 (error "ifUnsolved cannot be applied to lists of different sizes with unsolvable condition")

instance Conditional () where
    ifUnsolved c _ _ = ()

instance (Conditional a, Conditional b) => Conditional (a,b) where
    ifUnsolved c (a1,b1) (a2,b2) = ((ifUnsolved c a1 a2), (ifUnsolved c b1 b2))

instance (Conditional a, Conditional b, Conditional c) => Conditional (a,b,c) where
    ifUnsolved c (a1,b1,c1) (a2,b2,c2) = ((ifUnsolved c a1 a2), (ifUnsolved c b1 b2), (ifUnsolved c c1 c2))

instance (Conditional a, Conditional b, Conditional c, Conditional d) => Conditional (a,b,c,d) where
    ifUnsolved c (a1,b1,c1,d1) (a2,b2,c2,d2) = ((ifUnsolved c a1 a2), (ifUnsolved c b1 b2), (ifUnsolved c c1 c2), (ifUnsolved c d1 d2))

instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e) => Conditional (a,b,c,d,e) where
    ifUnsolved c (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = ((ifUnsolved c a1 a2), (ifUnsolved c b1 b2), (ifUnsolved c c1 c2),
                                                      (ifUnsolved c d1 d2), (ifUnsolved c e1 e2))

instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e, Conditional f) => Conditional (a,b,c,d,e,f) where
    ifUnsolved c (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) = ((ifUnsolved c a1 a2), (ifUnsolved c b1 b2), (ifUnsolved c c1 c2),
                                                            (ifUnsolved c d1 d2), (ifUnsolved c e1 e2), (ifUnsolved c f1 f2))

instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e, Conditional f, Conditional g) => Conditional (a,b,c,d,e,f,g) where
    ifUnsolved c (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) = ((ifUnsolved c a1 a2), (ifUnsolved c b1 b2), (ifUnsolved c c1 c2), (ifUnsolved c d1 d2),
                                                                  (ifUnsolved c e1 e2), (ifUnsolved c f1 f2), (ifUnsolved c g1 g2))

instance (Conditional a, Conditional b, Conditional c, Conditional d, Conditional e, Conditional f, Conditional g, Conditional h) => Conditional (a,b,c,d,e,f,g,h) where
    ifUnsolved c (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) = ((ifUnsolved c a1 a2), (ifUnsolved c b1 b2), (ifUnsolved c c1 c2), (ifUnsolved c d1 d2),
                                                                        (ifUnsolved c e1 e2), (ifUnsolved c f1 f2), (ifUnsolved c g1 g2), (ifUnsolved c h1 h2))

----------------------------------------------------------------------------------------------------
-- if then else with solving formula
----------------------------------------------------------------------------------------------------

ifSolve :: Formula -> a -> a -> a -> a
ifSolve c x1 x2 x3 = case solve c of
                       Just True -> x1
                       Just False -> x2
                       Nothing -> x3

-- | /if ... then ... else/ ... with formula solving.
ite :: Conditional a => Formula -> a -> a -> a
ite c x1 x2 = ifSolve c x1 x2 (ifUnsolved c x1 x2)
