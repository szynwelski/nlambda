{-# LANGUAGE KindSignatures, DefaultSignatures, FlexibleContexts, TypeOperators, RankNTypes #-}

module Meta where

import Data.Char (toLower)
import Data.Foldable (fold, foldl', foldr', toList)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Ratio (Ratio)
import GHC.Generics
import GHC.Read (readPrec, readListPrec)
import GHC.Show (showList__)
import Text.ParserCombinators.ReadPrec (ReadPrec)

import Debug.Trace

------------------------------------------------------------------------------------------
-- Class WithMeta and instances
------------------------------------------------------------------------------------------

type Identifier = Int
type IdMap = Map Identifier Identifier
type IdPairSet = Set (Identifier, Identifier)
type Meta = (IdMap, IdPairSet)

data WithMeta a = WithMeta {value :: a, meta :: Meta}

instance Show a => Show (WithMeta a) where
    showsPrec n = noMetaResOp $ showsPrec n
    show (WithMeta x m) = show x ++ " | meta:" ++ show m
    showList = showList . value . liftMeta

--instance Eq a => Eq (WithMeta a) where
--    (==) = noMetaResUnionOp (==)
--    (/=) = noMetaResUnionOp (/=)
--
--instance Ord a => Ord (WithMeta a) where
--    compare = noMetaResUnionOp compare
--    (<) = noMetaResUnionOp (<)
--    (<=) = noMetaResUnionOp (<=)
--    (>) = noMetaResUnionOp (>)
--    (>=) = noMetaResUnionOp (>=)
--    max = unionOp max
--    min = unionOp min
--
--instance Bounded a => Bounded (WithMeta a) where
--    minBound = noMeta minBound
--    maxBound = noMeta maxBound
--
--instance Enum a => Enum (WithMeta a) where
--    succ = idOp succ
--    pred = idOp pred
--    toEnum = noMeta . toEnum
--    fromEnum = noMetaResOp fromEnum
--    enumFrom = fmap noMeta . enumFrom . value
--    enumFromThen x = fmap noMeta . enumFromThen (value x). value
--    enumFromTo x = fmap noMeta . enumFromTo (value x). value
--    enumFromThenTo x y = fmap noMeta . enumFromThenTo (value x) (value y) . value
--
--instance Num a => Num (WithMeta a) where
--    (+) = unionOp (+)
--    (-) = unionOp (-)
--    (*) = unionOp (*)
--    negate = idOp negate
--    abs = idOp abs
--    signum = idOp signum
--    fromInteger = noMeta . fromInteger
--
instance Monoid a => Monoid (WithMeta a) where
    mempty = noMeta mempty
    mappend = unionOp mappend
    mconcat = idOp mconcat . liftMeta

------------------------------------------------------------------------------------------
-- Methods to handle meta information in expressions
------------------------------------------------------------------------------------------

type Union = (Meta, [IdMap])

-- TODO
replaceVariablesIds :: IdMap -> a -> a
replaceVariablesIds = undefined

splitMapping :: IdPairSet -> IdMap -> (IdMap, IdMap)
splitMapping s m = if Set.null s then (Map.empty, m) else Map.partitionWithKey (\k v -> Set.member (k, v) s) m

renameMaps :: Meta -> Meta -> (Meta, IdMap, IdMap)
renameMaps (m1, s1) (m2, s2) = ((Map.union m1b m2c, Set.unions [s1, s2, Set.fromList $ Map.assocs m2d]), m1a, Map.union m2a m2d)
    where (m1a, m1b) = splitMapping s2 m1
          (m2a, m2b) = splitMapping s1 m2
          (m2c, m2d) = if Map.null m1b then (m2b, Map.empty) else Map.partitionWithKey (\k v -> v == Map.findWithDefault v k m1b) m2b

metaFromMap :: IdMap -> Meta
metaFromMap map = (map, Set.empty)

emptyMeta :: Meta
emptyMeta = (Map.empty, Set.empty)

noMeta :: a -> WithMeta a
noMeta x = WithMeta x emptyMeta

create :: a -> Meta -> WithMeta a
create x m = WithMeta x m

union :: [Meta] -> Union
union [] = (emptyMeta, [])
union [m] = (m, [Map.empty])
union (m:ms) = let (m', idMap:idMaps) = union ms
                   (m'', idMap1, idMap2) = renameMaps m m'
               in (m'', idMap1 : (Map.union idMap2 idMap) : idMaps)

getMeta :: Union -> Meta
getMeta = fst

rename :: Union -> Int -> a -> a
rename (_, idMaps) n x = let idMap = idMaps !! n
                         in if null idMap then x else x -- TODO call replaceVariablesIds

emptyList :: [a]
emptyList = []

colon :: a -> [a] -> [a]
colon = (:)

------------------------------------------------------------------------------------------
-- Conversion functions to meta operations
------------------------------------------------------------------------------------------

idOp :: (a -> b) -> WithMeta a -> WithMeta b
idOp op (WithMeta x m) = WithMeta (op x) m

noMetaArgOp :: (a -> b) -> a -> WithMeta b
noMetaArgOp op = noMeta . op

noMetaResOp :: (a -> b) -> WithMeta a -> b
noMetaResOp op = op . value

leftIdOp :: (a -> b -> c) -> WithMeta a -> b -> WithMeta c
leftIdOp op (WithMeta x m) y = WithMeta (op x y) m

rightIdOp :: (a -> b -> c) -> a -> WithMeta b -> WithMeta c
rightIdOp op x (WithMeta y m) = WithMeta (op x y) m

unionOp :: (a -> b -> c) -> WithMeta a -> WithMeta b -> WithMeta c
unionOp op (WithMeta x m1) (WithMeta y m2) = WithMeta (op x' y') (getMeta u)
    where u = union [m1, m2]
          x' = rename u 0 x
          y' = rename u 1 y

union3Op :: (a -> b -> c -> d) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d
union3Op op (WithMeta x m1) (WithMeta y m2) (WithMeta z m3) = WithMeta (op x' y' z') (getMeta u)
    where u = union [m1, m2, m3]
          x' = rename u 0 x
          y' = rename u 1 y
          z' = rename u 2 z

noMetaResUnionOp :: (a -> b -> c) -> WithMeta a -> WithMeta b -> c
noMetaResUnionOp op x = value . unionOp op x

(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.*) = (.) . (.)

metaFun :: Meta -> (WithMeta a -> b) -> a -> b
metaFun m f x = f (WithMeta x m)

metaFunOp :: ((a -> b) -> c -> d) -> (WithMeta a -> b) -> WithMeta c -> d
metaFunOp op f (WithMeta x m) = op (metaFun m f) x

noMetaResFunOp :: ((a -> b) -> c -> d) -> (WithMeta a -> b) -> WithMeta c -> WithMeta d
noMetaResFunOp op f (WithMeta x m) = WithMeta (op (metaFun m f) x) m

------------------------------------------------------------------------------------------
-- Class MetaLevel and deriving methods
------------------------------------------------------------------------------------------

class MetaLevel (f :: * -> *) where
    liftMeta :: f (WithMeta a) -> WithMeta (f a)
    dropMeta :: WithMeta (f a) -> f (WithMeta a)
    default liftMeta :: (Generic1 f, MetaLevel (Rep1 f)) => f (WithMeta a) -> WithMeta (f a)
    liftMeta x = let (WithMeta y m) = liftMeta (from1 x) in WithMeta (to1 y) m
    default dropMeta :: (Generic1 f, MetaLevel (Rep1 f)) => WithMeta (f a) -> f (WithMeta a)
    dropMeta (WithMeta x m) = to1 $ dropMeta (WithMeta (from1 x) m)

instance MetaLevel Par1 where
    liftMeta (Par1 (WithMeta x m)) = WithMeta (Par1 x) m
    dropMeta (WithMeta (Par1 x) m) = Par1 (WithMeta x m)

instance MetaLevel f => MetaLevel (Rec1 f) where
    liftMeta (Rec1 x) = let (WithMeta y m) = liftMeta x in WithMeta (Rec1 y) m
    dropMeta (WithMeta (Rec1 x) m) = Rec1 (dropMeta (WithMeta x m))

instance MetaLevel U1 where
    liftMeta U1 = noMeta U1
    dropMeta (WithMeta U1 _) = U1

instance MetaLevel (K1 i c) where
    liftMeta (K1 x) = noMeta $ K1 x
    dropMeta (WithMeta (K1 x) m) = K1 x

instance MetaLevel f => MetaLevel (M1 i c f) where
    liftMeta (M1 x) = let (WithMeta y m) = liftMeta x in WithMeta (M1 y) m
    dropMeta (WithMeta (M1 x) m) = M1 (dropMeta (WithMeta x m))

instance (MetaLevel f, MetaLevel g) => MetaLevel (f :+: g) where
    liftMeta (L1 x) = let (WithMeta y m) = liftMeta x in WithMeta (L1 y) m
    liftMeta (R1 x) = let (WithMeta y m) = liftMeta x in WithMeta (R1 y) m
    dropMeta (WithMeta (L1 x) m) = L1 (dropMeta (WithMeta x m))
    dropMeta (WithMeta (R1 x) m) = R1 (dropMeta (WithMeta x m))

instance (MetaLevel f, MetaLevel g) => MetaLevel (f :*: g) where
    liftMeta (x :*: y) = let (WithMeta x' m1) = liftMeta x
                             (WithMeta y' m2) = liftMeta y
                             u = union [m1, m2]
                             x'' = rename u 0 x'
                             y'' = rename u 1 y'
                         in WithMeta (x'' :*: y'') (getMeta u)
    dropMeta (WithMeta (x :*: y) m) = dropMeta (WithMeta x m) :*: dropMeta (WithMeta y m)

instance MetaLevel Maybe
instance MetaLevel []
instance MetaLevel (Either a)
instance MetaLevel ((,) a)
instance MetaLevel ((->) a) where
    liftMeta f = noMeta (value . f)
    dropMeta f = noMeta . (value f)

instance MetaLevel IO where
    liftMeta x = noMeta $ fmap value x -- noMeta ???
    dropMeta (WithMeta x m) = fmap (`WithMeta` m) x

------------------------------------------------------------------------------------------
-- Meta classes from Prelude
------------------------------------------------------------------------------------------

class (Applicative f, Functor_nlambda f) => Applicative_nlambda (f :: * -> *) where
  pure_nlambda :: WithMeta a -> WithMeta (f a)
  pure_nlambda = idOp pure
  (<*>###) :: WithMeta (f (WithMeta a -> WithMeta b)) -> WithMeta (f a) -> WithMeta (f b)
  (<*>###) f x = let (WithMeta (f', x') m) = unionOp (,) f x in liftMeta (fmap (metaFun m) f' <*> x')
  (*>###) :: WithMeta (f a) -> WithMeta (f b) -> WithMeta (f b)
  (*>###) = unionOp (*>)
  (<*###) :: WithMeta (f a) -> WithMeta (f b) -> WithMeta (f a)
  (<*###) = unionOp (<*)

instance Applicative_nlambda (Either e) -- Defined in ‘Data.Either’
instance Applicative_nlambda [] -- Defined in ‘GHC.Base’
instance Applicative_nlambda Maybe -- Defined in ‘GHC.Base’
instance Applicative_nlambda IO -- Defined in ‘GHC.Base’
instance Applicative_nlambda ((->) a) -- Defined in ‘GHC.Base’
instance Monoid_nlambda a => Applicative_nlambda ((,) a) -- Defined in ‘GHC.Base’

class Bounded a => Bounded_nlambda a where
  minBound_nlambda :: WithMeta a
  minBound_nlambda = noMeta minBound
  maxBound_nlambda :: WithMeta a
  maxBound_nlambda = noMeta maxBound

instance Bounded_nlambda Word -- Defined in ‘GHC.Enum’
instance Bounded_nlambda Ordering -- Defined in ‘GHC.Enum’
instance Bounded_nlambda Int -- Defined in ‘GHC.Enum’
instance Bounded_nlambda Char -- Defined in ‘GHC.Enum’
instance Bounded_nlambda Bool -- Defined in ‘GHC.Enum’
instance Bounded_nlambda () -- Defined in ‘GHC.Enum’
instance (Bounded_nlambda a, Bounded_nlambda b) => Bounded_nlambda (a, b)  -- Defined in ‘GHC.Enum’
instance (Bounded_nlambda a, Bounded_nlambda b, Bounded_nlambda c) => Bounded_nlambda (a, b, c)  -- Defined in ‘GHC.Enum’

class Enum a => Enum_nlambda a where
  succ_nlambda :: WithMeta a -> WithMeta a
  succ_nlambda = idOp succ
  pred_nlambda :: WithMeta a -> WithMeta a
  pred_nlambda = idOp pred
  toEnum_nlambda :: Int -> WithMeta a
  toEnum_nlambda = noMeta . toEnum
  fromEnum_nlambda :: WithMeta a -> Int
  fromEnum_nlambda = noMetaResOp fromEnum
  enumFrom_nlambda :: WithMeta a -> WithMeta [a]
  enumFrom_nlambda = idOp enumFrom
  enumFromThen_nlambda :: WithMeta a -> WithMeta a -> WithMeta [a]
  enumFromThen_nlambda = unionOp enumFromThen
  enumFromTo_nlambda :: WithMeta a -> WithMeta a -> WithMeta [a]
  enumFromTo_nlambda = unionOp enumFromTo
  enumFromThenTo_nlambda :: WithMeta a -> WithMeta a -> WithMeta a -> WithMeta [a]
  enumFromThenTo_nlambda = union3Op enumFromThenTo

instance Enum_nlambda Word -- Defined in ‘GHC.Enum’
instance Enum_nlambda Ordering -- Defined in ‘GHC.Enum’
instance Enum_nlambda Integer -- Defined in ‘GHC.Enum’
instance Enum_nlambda Int -- Defined in ‘GHC.Enum’
instance Enum_nlambda Char -- Defined in ‘GHC.Enum’
instance Enum_nlambda Bool -- Defined in ‘GHC.Enum’
instance Enum_nlambda () -- Defined in ‘GHC.Enum’
instance Enum_nlambda Float -- Defined in ‘GHC.Float’
instance Enum_nlambda Double -- Defined in ‘GHC.Float’

class Eq a => Eq_nlambda a where
  (==###) :: WithMeta a -> WithMeta a -> Bool
  (==###) = noMetaResUnionOp (==) -- FIXME replace vars and then compare?
  (/=###) :: WithMeta a -> WithMeta a -> Bool
  (/=###) = noMetaResUnionOp (/=)

instance (Eq_nlambda a, Eq_nlambda b) => Eq_nlambda (Either a b)  -- Defined in ‘Data.Either’
instance Eq_nlambda Integer  -- Defined in ‘integer-gmp-1.0.0.0:GHC.Integer.Type’
instance Eq_nlambda a => Eq_nlambda [a] -- Defined in ‘GHC.Classes’
instance Eq_nlambda Word -- Defined in ‘GHC.Classes’
instance Eq_nlambda Ordering -- Defined in ‘GHC.Classes’
instance Eq_nlambda Int -- Defined in ‘GHC.Classes’
instance Eq_nlambda Float -- Defined in ‘GHC.Classes’
instance Eq_nlambda Double -- Defined in ‘GHC.Classes’
instance Eq_nlambda Char -- Defined in ‘GHC.Classes’
instance Eq_nlambda Bool -- Defined in ‘GHC.Classes’
instance Eq_nlambda () -- Defined in ‘GHC.Classes’
instance (Eq_nlambda a, Eq_nlambda b) => Eq_nlambda (a, b) -- Defined in ‘GHC.Classes’
instance (Eq_nlambda a, Eq_nlambda b, Eq_nlambda c) => Eq_nlambda (a, b, c)  -- Defined in ‘GHC.Classes’
instance Eq_nlambda a => Eq_nlambda (Maybe a) -- Defined in ‘GHC.Base’

class (Floating a, Fractional_nlambda a) => Floating_nlambda a where
  pi_nlambda :: WithMeta a
  pi_nlambda = noMeta pi
  exp_nlambda :: WithMeta a -> WithMeta a
  exp_nlambda = idOp exp
  log_nlambda :: WithMeta a -> WithMeta a
  log_nlambda = idOp log
  sqrt_nlambda :: WithMeta a -> WithMeta a
  sqrt_nlambda = idOp sqrt
  (**###) :: WithMeta a -> WithMeta a -> WithMeta a
  (**###) = unionOp (**)
  logBase_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  logBase_nlambda = unionOp logBase
  sin_nlambda :: WithMeta a -> WithMeta a
  sin_nlambda = idOp sin
  cos_nlambda :: WithMeta a -> WithMeta a
  cos_nlambda = idOp cos
  tan_nlambda :: WithMeta a -> WithMeta a
  tan_nlambda = idOp tan
  asin_nlambda :: WithMeta a -> WithMeta a
  asin_nlambda = idOp asin
  acos_nlambda :: WithMeta a -> WithMeta a
  acos_nlambda = idOp acos
  atan_nlambda :: WithMeta a -> WithMeta a
  atan_nlambda = idOp atan
  sinh_nlambda :: WithMeta a -> WithMeta a
  sinh_nlambda = idOp sinh
  cosh_nlambda :: WithMeta a -> WithMeta a
  cosh_nlambda = idOp cosh
  tanh_nlambda :: WithMeta a -> WithMeta a
  tanh_nlambda = idOp tanh
  asinh_nlambda :: WithMeta a -> WithMeta a
  asinh_nlambda = idOp asinh
  acosh_nlambda :: WithMeta a -> WithMeta a
  acosh_nlambda = idOp acosh
  atanh_nlambda :: WithMeta a -> WithMeta a
  atanh_nlambda = idOp atanh

instance Floating_nlambda Float -- Defined in ‘GHC.Float’
instance Floating_nlambda Double -- Defined in ‘GHC.Float’

class (MetaLevel t, Foldable t) => Foldable_nlambda (t :: * -> *) where
  fold_nlambda :: Monoid_nlambda m => WithMeta (t m) -> WithMeta m
  fold_nlambda = idOp fold
  foldMap_nlambda :: Monoid_nlambda m => (WithMeta a -> WithMeta m) -> WithMeta (t a) -> WithMeta m
  foldMap_nlambda = metaFunOp foldMap
  foldr_nlambda :: (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
  foldr_nlambda f x = foldr f x . dropMeta
  foldr'_nlambda :: (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
  foldr'_nlambda f x = foldr' f x . dropMeta
  foldl_nlambda :: (WithMeta b -> WithMeta a -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
  foldl_nlambda f x = foldl f x . dropMeta
  foldl'_nlambda :: (WithMeta b -> WithMeta a -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
  foldl'_nlambda f x = foldl' f x . dropMeta
  foldr1_nlambda :: (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta (t a) -> WithMeta a
  foldr1_nlambda f = foldr1 f . dropMeta
  foldl1_nlambda :: (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta (t a) -> WithMeta a
  foldl1_nlambda f = foldl1 f . dropMeta
  toList_nlambda :: WithMeta (t a) -> WithMeta [a]
  toList_nlambda = idOp toList
  null_nlambda :: WithMeta (t a) -> Bool
  null_nlambda = noMetaResOp null
  length_nlambda :: WithMeta (t a) -> Int
  length_nlambda = noMetaResOp length
  elem_nlambda :: Eq_nlambda a => WithMeta a -> WithMeta (t a) -> Bool
  elem_nlambda = noMetaResUnionOp elem
  maximum_nlambda :: Ord_nlambda a => WithMeta (t a) -> WithMeta a
  maximum_nlambda = idOp maximum
  minimum_nlambda :: Ord_nlambda a => WithMeta (t a) -> WithMeta a
  minimum_nlambda = idOp minimum
  sum_nlambda :: Num_nlambda a => WithMeta (t a) -> WithMeta a
  sum_nlambda = idOp sum
  product_nlambda :: Num_nlambda a => WithMeta (t a) -> WithMeta a
  product_nlambda = idOp product

instance Foldable_nlambda [] -- Defined in ‘Data.Foldable’
instance Foldable_nlambda Maybe -- Defined in ‘Data.Foldable’
instance Foldable_nlambda (Either a) -- Defined in ‘Data.Foldable’
instance Foldable_nlambda ((,) a) -- Defined in ‘Data.Foldable’

class (Fractional a, Num_nlambda a) => Fractional_nlambda a where
  (/###) :: WithMeta a -> WithMeta a -> WithMeta a
  (/###) = unionOp (/)
  recip_nlambda :: WithMeta a -> WithMeta a
  recip_nlambda = idOp recip
  fromRational_nlambda :: Rational -> WithMeta a
  fromRational_nlambda = noMeta . fromRational

instance Fractional_nlambda Float -- Defined in ‘GHC.Float’
instance Fractional_nlambda Double -- Defined in ‘GHC.Float’

class (MetaLevel f, Functor f) => Functor_nlambda (f :: * -> *) where
  fmap_nlambda :: (WithMeta a -> WithMeta b) -> WithMeta (f a) -> WithMeta (f b)
  fmap_nlambda = liftMeta .* metaFunOp fmap
  (<$###) :: WithMeta a -> WithMeta (f b) -> WithMeta (f a)
  (<$###) = unionOp (<$)

instance Functor_nlambda (Either a) -- Defined in ‘Data.Either’
instance Functor_nlambda [] -- Defined in ‘GHC.Base’
instance Functor_nlambda Maybe -- Defined in ‘GHC.Base’
instance Functor_nlambda IO -- Defined in ‘GHC.Base’
instance Functor_nlambda ((->) r) -- Defined in ‘GHC.Base’
instance Functor_nlambda ((,) a) -- Defined in ‘GHC.Base’

class (Integral a, Real_nlambda a, Enum_nlambda a) => Integral_nlambda a where
  quot_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  quot_nlambda = unionOp quot
  rem_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  rem_nlambda = unionOp rem
  div_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  div_nlambda = unionOp div
  mod_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  mod_nlambda = unionOp mod
  quotRem_nlambda :: WithMeta a -> WithMeta a -> WithMeta (a, a)
  quotRem_nlambda = unionOp quotRem
  divMod_nlambda :: WithMeta a -> WithMeta a -> WithMeta (a, a)
  divMod_nlambda = unionOp divMod
  toInteger_nlambda :: WithMeta a -> Integer
  toInteger_nlambda = noMetaResOp toInteger

instance Integral_nlambda Word -- Defined in ‘GHC.Real’
instance Integral_nlambda Integer -- Defined in ‘GHC.Real’
instance Integral_nlambda Int -- Defined in ‘GHC.Real’

class (Monad m, Applicative_nlambda m) => Monad_nlambda (m :: * -> *) where
  (>>=###) :: WithMeta (m a) -> (WithMeta a -> WithMeta (m b)) -> WithMeta (m b)
  (>>=###) (WithMeta x m) f = liftMeta $ x >>= (dropMeta . metaFun m f)
  (>>###) :: WithMeta (m a) -> WithMeta (m b) -> WithMeta (m b)
  (>>###) = unionOp (>>)
  return_nlambda :: WithMeta a -> WithMeta (m a)
  return_nlambda = idOp return
  fail_nlambda :: String -> WithMeta (m a)
  fail_nlambda = noMeta . fail

instance Monad_nlambda (Either e) -- Defined in ‘Data.Either’
instance Monad_nlambda [] -- Defined in ‘GHC.Base’
instance Monad_nlambda Maybe -- Defined in ‘GHC.Base’
instance Monad_nlambda IO -- Defined in ‘GHC.Base’
instance Monad_nlambda ((->) r) -- Defined in ‘GHC.Base’

class Monoid a => Monoid_nlambda a where
  mempty_nlambda :: WithMeta a
  mempty_nlambda = noMeta mempty
  mappend_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  mappend_nlambda = unionOp mappend
  mconcat_nlambda :: WithMeta [a] -> WithMeta a
  mconcat_nlambda = idOp mconcat

instance Monoid_nlambda [a] -- Defined in ‘GHC.Base’
instance Monoid_nlambda Ordering -- Defined in ‘GHC.Base’
instance Monoid_nlambda a => Monoid_nlambda (Maybe a) -- Defined in ‘GHC.Base’
instance Monoid_nlambda b => Monoid_nlambda (a -> b) -- Defined in ‘GHC.Base’
instance Monoid_nlambda () -- Defined in ‘GHC.Base’
instance (Monoid_nlambda a, Monoid_nlambda b) => Monoid_nlambda (a, b)  -- Defined in ‘GHC.Base’
instance (Monoid_nlambda a, Monoid_nlambda b, Monoid_nlambda c) => Monoid_nlambda (a, b, c)  -- Defined in ‘GHC.Base’

class Num a => Num_nlambda a where
  (+###) :: WithMeta a -> WithMeta a -> WithMeta a
  (+###) = unionOp (+)
  (-###) :: WithMeta a -> WithMeta a -> WithMeta a
  (-###) = unionOp (-)
  (*###) :: WithMeta a -> WithMeta a -> WithMeta a
  (*###) = unionOp (*)
  negate_nlambda :: WithMeta a -> WithMeta a
  negate_nlambda = idOp negate
  abs_nlambda :: WithMeta a -> WithMeta a
  abs_nlambda = idOp abs
  signum_nlambda :: WithMeta a -> WithMeta a
  signum_nlambda = idOp signum
  fromInteger_nlambda :: Integer -> WithMeta a
  fromInteger_nlambda = noMeta . fromInteger

instance Num_nlambda Word -- Defined in ‘GHC.Num’
instance Num_nlambda Integer -- Defined in ‘GHC.Num’
instance Num_nlambda Int -- Defined in ‘GHC.Num’
instance Num_nlambda Float -- Defined in ‘GHC.Float’
instance Num_nlambda Double -- Defined in ‘GHC.Float’
instance Integral_nlambda a => Num_nlambda (Ratio a)

class (Ord a, Eq_nlambda a) => Ord_nlambda a where
  compare_nlambda :: WithMeta a -> WithMeta a -> Ordering
  compare_nlambda = noMetaResUnionOp compare
  (<###) :: WithMeta a -> WithMeta a -> Bool
  (<###) = noMetaResUnionOp (<)
  (<=###) :: WithMeta a -> WithMeta a -> Bool
  (<=###) = noMetaResUnionOp (<=)
  (>###) :: WithMeta a -> WithMeta a -> Bool
  (>###) = noMetaResUnionOp (>)
  (>=###) :: WithMeta a -> WithMeta a -> Bool
  (>=###) = noMetaResUnionOp (>=)
  max_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  max_nlambda = unionOp max
  min_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  min_nlambda = unionOp min

instance Ord_nlambda Integer  -- Defined in ‘integer-gmp-1.0.0.0:GHC.Integer.Type’
instance Ord_nlambda a => Ord_nlambda [a] -- Defined in ‘GHC.Classes’
instance Ord_nlambda Word -- Defined in ‘GHC.Classes’
instance Ord_nlambda Ordering -- Defined in ‘GHC.Classes’
instance Ord_nlambda Int -- Defined in ‘GHC.Classes’
instance Ord_nlambda Float -- Defined in ‘GHC.Classes’
instance Ord_nlambda Double -- Defined in ‘GHC.Classes’
instance Ord_nlambda Char -- Defined in ‘GHC.Classes’
instance Ord_nlambda Bool -- Defined in ‘GHC.Classes’
instance Ord_nlambda () -- Defined in ‘GHC.Classes’
instance (Ord_nlambda a, Ord_nlambda b) => Ord_nlambda (a, b) -- Defined in ‘GHC.Classes’
instance (Ord_nlambda a, Ord_nlambda b, Ord_nlambda c) => Ord_nlambda (a, b, c)  -- Defined in ‘GHC.Classes’
instance (Ord_nlambda a, Ord_nlambda b) => Ord_nlambda (Either a b)  -- Defined in ‘Data.Either’
instance Ord_nlambda a => Ord_nlambda (Maybe a) -- Defined in ‘GHC.Base’

class Read a => Read_nlambda a where
  readsPrec_nlambda :: Int -> String -> WithMeta [(a, String)]
  readsPrec_nlambda n = noMeta . readsPrec n
  readList_nlambda :: String -> WithMeta [([a], String)]
  readList_nlambda = noMeta . readList
  readPrec_nlambda :: WithMeta (ReadPrec a)
  readPrec_nlambda = noMeta readPrec
  readListPrec_nlambda :: WithMeta (ReadPrec [a])
  readListPrec_nlambda = noMeta readListPrec

instance Read_nlambda a => Read_nlambda [a] -- Defined in ‘GHC.Read’
instance Read_nlambda Word -- Defined in ‘GHC.Read’
instance Read_nlambda Ordering -- Defined in ‘GHC.Read’
instance Read_nlambda a => Read_nlambda (Maybe a) -- Defined in ‘GHC.Read’
instance Read_nlambda Integer -- Defined in ‘GHC.Read’
instance Read_nlambda Int -- Defined in ‘GHC.Read’
instance Read_nlambda Float -- Defined in ‘GHC.Read’
instance Read_nlambda Double -- Defined in ‘GHC.Read’
instance Read_nlambda Char -- Defined in ‘GHC.Read’
instance Read_nlambda Bool -- Defined in ‘GHC.Read’
instance Read_nlambda () -- Defined in ‘GHC.Read’
instance (Read_nlambda a, Read_nlambda b) => Read_nlambda (a, b) -- Defined in ‘GHC.Read’
instance (Read_nlambda a, Read_nlambda b, Read_nlambda c) => Read_nlambda (a, b, c)  -- Defined in ‘GHC.Read’
instance (Read_nlambda a, Read_nlambda b) => Read_nlambda (Either a b)  -- Defined in ‘Data.Either’

class (Real a, Num_nlambda a, Ord_nlambda a) => Real_nlambda a where
  toRational_nlambda :: WithMeta a -> Rational
  toRational_nlambda = noMetaResOp toRational

instance Real_nlambda Word -- Defined in ‘GHC.Real’
instance Real_nlambda Integer -- Defined in ‘GHC.Real’
instance Real_nlambda Int -- Defined in ‘GHC.Real’
instance Real_nlambda Float -- Defined in ‘GHC.Float’
instance Real_nlambda Double -- Defined in ‘GHC.Float’

class (RealFloat a, RealFrac_nlambda a, Floating_nlambda a) => RealFloat_nlambda a where
  floatRadix_nlambda :: WithMeta a -> Integer
  floatRadix_nlambda = noMetaResOp floatRadix
  floatDigits_nlambda :: WithMeta a -> Int
  floatDigits_nlambda = noMetaResOp floatDigits
  floatRange_nlambda :: WithMeta a -> (Int, Int)
  floatRange_nlambda = noMetaResOp floatRange
  decodeFloat_nlambda :: WithMeta a -> (Integer, Int)
  decodeFloat_nlambda = noMetaResOp decodeFloat
  encodeFloat_nlambda :: Integer -> Int -> WithMeta a
  encodeFloat_nlambda x = noMeta . encodeFloat x
  exponent_nlambda :: WithMeta a -> Int
  exponent_nlambda = noMetaResOp exponent
  significand_nlambda :: WithMeta a -> WithMeta a
  significand_nlambda = idOp significand
  scaleFloat_nlambda :: Int -> WithMeta a -> WithMeta a
  scaleFloat_nlambda = rightIdOp scaleFloat
  isNaN_nlambda :: WithMeta a -> Bool
  isNaN_nlambda = noMetaResOp isNaN
  isInfinite_nlambda :: WithMeta a -> Bool
  isInfinite_nlambda = noMetaResOp isInfinite
  isDenormalized_nlambda :: WithMeta a -> Bool
  isDenormalized_nlambda = noMetaResOp isDenormalized
  isNegativeZero_nlambda :: WithMeta a -> Bool
  isNegativeZero_nlambda = noMetaResOp isNegativeZero
  isIEEE_nlambda :: WithMeta a -> Bool
  isIEEE_nlambda = noMetaResOp isIEEE
  atan2_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  atan2_nlambda = unionOp atan2

instance RealFloat_nlambda Float -- Defined in ‘GHC.Float’
instance RealFloat_nlambda Double -- Defined in ‘GHC.Float’

class (RealFrac a, Real_nlambda a, Fractional_nlambda a) => RealFrac_nlambda a where
  properFraction_nlambda :: Integral_nlambda b => WithMeta a -> WithMeta (b, a)
  properFraction_nlambda = idOp properFraction
  truncate_nlambda :: Integral_nlambda b => WithMeta a -> WithMeta b
  truncate_nlambda = idOp truncate
  round_nlambda :: Integral_nlambda b => WithMeta a -> WithMeta b
  round_nlambda = idOp round
  ceiling_nlambda :: Integral_nlambda b => WithMeta a -> WithMeta b
  ceiling_nlambda = idOp ceiling
  floor_nlambda :: Integral_nlambda b => WithMeta a -> WithMeta b
  floor_nlambda = idOp floor

instance RealFrac_nlambda Float -- Defined in ‘GHC.Float’
instance RealFrac_nlambda Double -- Defined in ‘GHC.Float’

class Show a => Show_nlambda a where
  showsPrec_nlambda :: Int -> WithMeta a -> ShowS
  showsPrec_nlambda n = noMetaResOp (showsPrec n)
  show_nlambda :: WithMeta a -> String
  show_nlambda = noMetaResOp show
  showList_nlambda :: WithMeta [a] -> ShowS
  showList_nlambda = noMetaResOp showList

instance Show_nlambda a => Show_nlambda [a] -- Defined in ‘GHC.Show’
instance Show_nlambda Word -- Defined in ‘GHC.Show’
instance Show_nlambda Ordering -- Defined in ‘GHC.Show’
instance Show_nlambda a => Show_nlambda (Maybe a) -- Defined in ‘GHC.Show’
instance Show_nlambda Integer -- Defined in ‘GHC.Show’
instance Show_nlambda Int -- Defined in ‘GHC.Show’
instance Show_nlambda Char -- Defined in ‘GHC.Show’
instance Show_nlambda Bool -- Defined in ‘GHC.Show’
instance Show_nlambda () -- Defined in ‘GHC.Show’
instance (Show_nlambda a, Show_nlambda b) => Show_nlambda (a, b) -- Defined in ‘GHC.Show’
instance (Show_nlambda a, Show_nlambda b, Show_nlambda c) => Show_nlambda (a, b, c)  -- Defined in ‘GHC.Show’
instance (Show_nlambda a, Show_nlambda b) => Show_nlambda (Either a b)  -- Defined in ‘Data.Either’
instance Show_nlambda Float -- Defined in ‘GHC.Float’
instance Show_nlambda Double -- Defined in ‘GHC.Float’

class (Traversable t, Functor_nlambda t, Foldable_nlambda t) => Traversable_nlambda (t :: * -> *) where
  traverse_nlambda :: (MetaLevel f, Applicative_nlambda f) => (WithMeta a -> WithMeta (f b)) -> WithMeta (t a) -> WithMeta (f (t b))
  traverse_nlambda f (WithMeta x m) = liftMeta $ fmap liftMeta $ traverse (dropMeta . metaFun m f) x
  sequenceA_nlambda :: Applicative_nlambda f => WithMeta (t (f a)) -> WithMeta (f (t a))
  sequenceA_nlambda = idOp sequenceA
  mapM_nlambda :: Monad_nlambda m => (WithMeta a -> WithMeta (m b)) -> WithMeta (t a) -> WithMeta (m (t b))
  mapM_nlambda f (WithMeta x m) = liftMeta $ fmap liftMeta $ mapM (dropMeta . metaFun m f) x
  sequence_nlambda :: Monad_nlambda m => WithMeta (t (m a)) -> WithMeta (m (t a))
  sequence_nlambda = idOp sequence

instance Traversable_nlambda [] -- Defined in ‘Data.Traversable’
instance Traversable_nlambda Maybe -- Defined in ‘Data.Traversable’
instance Traversable_nlambda (Either a) -- Defined in ‘Data.Traversable’
instance Traversable_nlambda ((,) a) -- Defined in ‘Data.Traversable’

showList___nlambda :: (WithMeta a -> ShowS) ->  WithMeta [a] -> ShowS
showList___nlambda f (WithMeta xs m) = showList__ (metaFun m f) xs

----------------------------------------------------------------------------------------
-- Meta equivalents
----------------------------------------------------------------------------------------

name_suffix :: String
name_suffix = "_nlambda"

op_suffix :: String
op_suffix = "###"

data ConvertFun = NoMeta | IdOp | NoMetaArgOp | NoMetaResOp | LeftIdOp | RightIdOp | UnionOp | Union3Op
    | NoMetaResUnionOp | MetaFunOp | NoMetaResFunOp | LiftMeta | DropMeta deriving (Show, Eq, Ord)

convertFunName :: ConvertFun -> String
convertFunName fun = (toLower $ head $ show fun) : (tail $ show fun)

data MetaEquivalentType = SameOp | ConvertFun ConvertFun deriving (Eq, Ord)

data MetaEquivalent = NoEquivalent | MetaFun String | MetaConvertFun String | OrigFun deriving Show

type ModuleName = String
type MethodName = String
type MetaEquivalentMap = Map ModuleName (Map MetaEquivalentType [MethodName])

createEquivalentsMap :: ModuleName -> [(MetaEquivalentType, [MethodName])] -> MetaEquivalentMap
createEquivalentsMap mod = Map.singleton mod . Map.fromList

preludeEquivalents :: Map ModuleName (Map MetaEquivalentType [MethodName])
preludeEquivalents = Map.unions [ghcBase, ghcClasses, ghcEnum, ghcErr, ghcFloat, ghcList, ghcNum, ghcReal, ghcShow, ghcTuple, ghcTypes,
                                 dataEither, dataTuple, controlExceptionBase]

preludeModules :: [ModuleName]
preludeModules = Map.keys preludeEquivalents

metaEquivalent :: ModuleName -> MethodName -> MetaEquivalent
metaEquivalent mod name | isSuffixOf "#" name = OrigFun
metaEquivalent mod name = case maybe Nothing findMethod $ Map.lookup mod preludeEquivalents of
                            Just SameOp -> OrigFun
                            Just (ConvertFun fun) -> MetaConvertFun (convertFunName fun)
                            Nothing -> NoEquivalent
    where findMethod = maybe Nothing (Just . fst . fst) . Map.minViewWithKey . Map.filter (elem name)

----------------------------------------------------------------------------------------
-- Meta equivalents methods
----------------------------------------------------------------------------------------

ghcBase :: MetaEquivalentMap
ghcBase = createEquivalentsMap "GHC.Base"
    [(SameOp, ["$", "$!", ".", "id", "const"]),
     (ConvertFun NoMeta, ["Nothing"]),
     (ConvertFun IdOp, ["Just"]),
     (ConvertFun UnionOp, ["++", "$dm<$", "$dm<*", "$dm*>", "$dm>>"])]

map_nlambda :: (WithMeta a -> WithMeta b) -> WithMeta [a] -> WithMeta [b]
map_nlambda = fmap_nlambda

ghcClasses :: MetaEquivalentMap
ghcClasses = createEquivalentsMap "GHC.Classes" []

ghcEnum :: MetaEquivalentMap
ghcEnum = createEquivalentsMap "GHC.Enum" []

ghcErr :: MetaEquivalentMap
ghcErr = createEquivalentsMap "GHC.Err"
    [(SameOp, ["undefined"]),
     (ConvertFun NoMetaArgOp, ["error"])]

ghcFloat :: MetaEquivalentMap
ghcFloat = createEquivalentsMap "GHC.Float" []

ghcList :: MetaEquivalentMap
ghcList = createEquivalentsMap "GHC.List"
    [(ConvertFun LeftIdOp, ["!!"])]

ghcNum :: MetaEquivalentMap
ghcNum = createEquivalentsMap "GHC.Num"
    [(ConvertFun UnionOp, ["$dm-"])]

ghcReal :: MetaEquivalentMap
ghcReal = createEquivalentsMap "GHC.Real"
    [(ConvertFun UnionOp, ["^", "^^"])]

ghcShow :: MetaEquivalentMap
ghcShow = createEquivalentsMap "GHC.Show" []

ghcTuple :: MetaEquivalentMap
ghcTuple = createEquivalentsMap "GHC.Tuple"
    [(ConvertFun UnionOp, ["(,)"]),
     (ConvertFun Union3Op, ["(,,)"])]

ghcTypes :: MetaEquivalentMap
ghcTypes = createEquivalentsMap "GHC.Types"
    [(ConvertFun NoMeta, ["[]"]),
     (ConvertFun UnionOp, [":"])]

dataEither :: MetaEquivalentMap
dataEither = createEquivalentsMap "Data.Either"
    [(ConvertFun IdOp, ["Left", "Right"])]

dataTuple :: MetaEquivalentMap
dataTuple = createEquivalentsMap "Data.Tuple"
    [(ConvertFun IdOp, ["fst", "snd"])]

curry_nlambda :: (WithMeta (a, b) -> WithMeta c) -> WithMeta a -> WithMeta b -> WithMeta c
curry_nlambda f x y = f $ unionOp (,) x y

uncurry_nlambda :: (WithMeta a -> WithMeta b -> WithMeta c) -> WithMeta (a, b) -> WithMeta c
uncurry_nlambda f p = f (idOp fst p) (idOp snd p)

controlExceptionBase = createEquivalentsMap "Control.Exception.Base"
    [(SameOp, ["patError", "noMethodBindingError"])]
