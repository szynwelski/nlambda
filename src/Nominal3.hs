module Nominal where

import Prelude hiding (or, and, not, sum, map, filter)
import Formula
import Data.List.Utils (join)
import Data.IORef

data Variant a = Variant {value :: a, condition :: Formula} deriving Show

variant :: a -> Variant a
variant = flip Variant T

instance Functor Variant where
    fmap f (Variant v c) = Variant (f v) c

class Variants a where
    iF :: Formula -> a -> a -> a
    getVariants :: a -> [Variant a]

----------------------------------------------------------------------------------------------------

data VAtom = VAtom {atomVariants :: [Variant Atom]} deriving Show

vatom :: Atom -> VAtom
vatom a = VAtom [Variant a T]

instance Variants VAtom Atom where
    iF f a1 a2 = VAtom [Variant a1 f, Variant a2 (not f)]
    getVariants = atomVariants


