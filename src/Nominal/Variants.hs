module Nominal.Variants where

import Data.List.Utils (join)
import Data.Map (Map)
import qualified Data.Map as Map
import Formula
import Nominal.Conditional
import Nominal.Variable (Variable, variable)
import Prelude hiding (or, not)

----------------------------------------------------------------------------------------------------
-- Variants
----------------------------------------------------------------------------------------------------

data Variants a = Variants (Map a Formula) deriving (Eq, Ord)

variant :: a -> Variants a
variant x = Variants $ Map.singleton x T

instance Show a => Show (Variants a) where
    show (Variants vs) = join " | " (fmap showVariant $ Map.assocs vs)
      where showVariant (v, c) = show v ++ if c == T then "" else " : " ++ show c

instance Ord a => Conditional (Variants a) where
    iF c (Variants vs1) (Variants vs2) = Variants $ unionVariants c vs1 vs2
      where filterWith c vs = Map.map (/\ c) vs
            unionVariants c vs1 vs2 = Map.unionWith (\/) (filterWith c vs1) (filterWith (not c) vs2)

iFv :: Ord a => Formula -> a -> a -> Variants a
iFv c x1 x2 = iF c (variant x1) (variant x2)

toList :: Variants a -> [(a, Formula)]
toList (Variants vs) = Map.assocs vs

fromList :: Ord a => [(a, Formula)] -> Variants a
fromList = Variants . Map.fromList

values :: Variants a -> [a]
values (Variants vs) = Map.keys vs

map :: Ord b => (a -> b) -> Variants a -> Variants b
map f (Variants vs) = Variants (Map.mapKeys f vs)

variantsRelation :: (a -> a -> Formula) -> Variants a -> Variants a -> Formula
variantsRelation r vs1 vs2 = or [(r v1 v2) /\ c1 /\ c2 | (v1, c1) <- toList vs1, (v2, c2) <- toList vs2]

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
