{-# LANGUAGE KindSignatures #-}
module Nominal.Meta.GHC.Base where

import Data.Map (Map)
import Nominal.Meta.GHC.Classes
import Nominal.Meta
import Nominal.Variable

-- intentional excessive Var context for MetaPlugin
class (NLambda_Functor f, Applicative f) => NLambda_Applicative (f :: * -> *) where
    nlambda_pure :: Var a => WithMeta a -> WithMeta (f a)
    nlambda_pure = idOp pure
    (###<*>) :: (Var a, Var b, Var (f (WithMeta a -> WithMeta b)), Var (f a), Var (f b)) => WithMeta (f (WithMeta a -> WithMeta b)) -> WithMeta (f a) -> WithMeta (f b)
    (###<*>) f x = let (WithMeta (f', x') m) = renameAndApply2 (,) f x in lift (fmap (metaFun m) f' <*> x')
    (###*>) :: (Var a, Var b, Var (f a), Var (f b)) => WithMeta (f a) -> WithMeta (f b) -> WithMeta (f b)
    (###*>) = renameAndApply2 (*>)
    (###<*) :: (Var a, Var b, Var (f a), Var (f b)) => WithMeta (f a) -> WithMeta (f b) -> WithMeta (f a)
    (###<*) = renameAndApply2 (<*)

instance NLambda_Applicative []
instance NLambda_Applicative Maybe
instance NLambda_Applicative IO
instance NLambda_Applicative ((->) a)
instance NLambda_Monoid a => NLambda_Applicative ((,) a)

-- intentional excessive Var context for MetaPlugin
class (MetaLevel f, Functor f) => NLambda_Functor (f :: * -> *) where
    nlambda_fmap :: (Var b, Var (f b)) => (WithMeta a -> WithMeta b) -> WithMeta (f a) -> WithMeta (f b)
    nlambda_fmap = lift .* metaFunOp fmap
    (###<$) :: (Var a, Var b, Var (f b)) => WithMeta a -> WithMeta (f b) -> WithMeta (f a)
    (###<$) = renameAndApply2 (<$)

instance NLambda_Functor []
instance NLambda_Functor Maybe
instance NLambda_Functor IO
instance NLambda_Functor ((->) r)
instance NLambda_Functor ((,) a)

-- intentional excessive Var context for MetaPlugin
class (NLambda_Applicative m, Monad m) => NLambda_Monad (m :: * -> *) where
    (###>>=) :: (Var b, Var (m b)) => WithMeta (m a) -> (WithMeta a -> WithMeta (m b)) -> WithMeta (m b)
    (###>>=) (WithMeta x m) f = lift $ x >>= (dropMeta . metaFun m f)
    (###>>) :: (Var a, Var b, Var (m a), Var (m b)) => WithMeta (m a) -> WithMeta (m b) -> WithMeta (m b)
    (###>>) = renameAndApply2 (>>)
    nlambda_return :: WithMeta a -> WithMeta (m a)
    nlambda_return = idOp return
    nlambda_fail :: String -> WithMeta (m a)
    nlambda_fail = noMeta . fail

instance NLambda_Monad []
instance NLambda_Monad Maybe
instance NLambda_Monad IO
instance NLambda_Monad ((->) r)

class (Var a, Monoid a) => NLambda_Monoid a where
    nlambda_mempty :: WithMeta a
    nlambda_mempty = noMeta mempty
    nlambda_mappend :: WithMeta a -> WithMeta a -> WithMeta a
    nlambda_mappend = renameAndApply2 mappend
    nlambda_mconcat :: WithMeta [a] -> WithMeta a
    nlambda_mconcat = idOp mconcat

instance Var a => NLambda_Monoid [a]
instance NLambda_Monoid Ordering
instance NLambda_Monoid a => NLambda_Monoid (Maybe a)
instance NLambda_Monoid b => NLambda_Monoid (a -> b)
instance NLambda_Monoid ()
instance (NLambda_Monoid a, NLambda_Monoid b) => NLambda_Monoid (a, b)
instance (NLambda_Monoid a, NLambda_Monoid b, NLambda_Monoid c) => NLambda_Monoid (a, b, c)

instance NLambda_Eq a => NLambda_Eq (Maybe a)
instance NLambda_Ord a => NLambda_Ord (Maybe a)

(###$) :: (WithMeta a -> WithMeta b) -> WithMeta a -> WithMeta b
(###$) = ($)

(###$!) :: (WithMeta a -> WithMeta b) -> WithMeta a -> WithMeta b
(###$!) = ($!)

(###.) :: (WithMeta b -> WithMeta c) -> (WithMeta a -> WithMeta b) -> WithMeta a -> WithMeta c
(###.) = (.)

(###++) :: Var a => WithMeta [a] -> WithMeta [a] -> WithMeta [a]
(###++) = renameAndApply2 (++)

nlambda_const :: WithMeta a -> WithMeta b -> WithMeta a
nlambda_const = const

nlambda_flip :: (WithMeta a -> WithMeta b -> WithMeta c) -> WithMeta b -> WithMeta a -> WithMeta c
nlambda_flip = flip

nlambda_id :: WithMeta a -> WithMeta a
nlambda_id = id

nlambda_map :: Var b => (WithMeta a -> WithMeta b) -> WithMeta [a] -> WithMeta [b]
nlambda_map = nlambda_fmap

nlambda_Nothing :: WithMeta (Maybe a)
nlambda_Nothing = noMeta Nothing

nlambda_Just :: WithMeta a -> WithMeta (Maybe a)
nlambda_Just = idOp Just
