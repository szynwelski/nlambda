module Nominal.Variants where

import Data.List.Utils (join)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Formula
import Nominal.Conditional
import Prelude hiding (or, not)
import Text.Regex.Posix ((=~))

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

map :: Ord b => (a -> b) -> Variants a -> Variants b
map f (Variants vs) = Variants $ Map.mapKeysWith (\/) f vs

----------------------------------------------------------------------------------------------------
-- Atom
----------------------------------------------------------------------------------------------------

type Atom = Variants Variable

atom :: String -> Atom
atom name = if name =~ "(^[a-z]$)"
              then variant $ Variable name
              else error $ "Invalid atom name: " ++ name ++ ".\nAtom name can only contain small letters."
