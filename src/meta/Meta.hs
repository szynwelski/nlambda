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
    show = noMetaResOp show
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
  (<*>###) f x = let (WithMeta (f',x') m) = unionOp (,) f x in liftMeta (fmap (metaFun m) f' <*> x')
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

------------------------------------------------------------------------------------------
-- Conversion functions for meta types
------------------------------------------------------------------------------------------

convert0 :: WithMeta a -> a
convert0 = value

convert1 :: a -> WithMeta a
convert1 = noMeta
convertx0 :: (a -> WithMeta b) -> a -> b
convertx0 f x = value $ f x

convertx1 :: (a -> b) -> a -> WithMeta b
convertx1 f x = noMeta $ f x

convert0x :: (WithMeta a -> b) -> a -> b
convert0x f x = f (noMeta x)

convert00 :: (WithMeta a -> WithMeta b) -> a -> b
convert00 f x = value $ f (noMeta x)

convert01 :: (WithMeta a -> b) -> a -> WithMeta b
convert01 f x = noMeta $ f (noMeta x)

convert1x :: (a -> b) -> WithMeta a -> b
convert1x f x = f (value x)

convert10 :: (a -> WithMeta b) -> WithMeta a -> b
convert10 f x = value $ f (value x)

convert11 :: (a -> b) -> WithMeta a -> WithMeta b
convert11 f x = noMeta $ f (value x)

convertxx0 :: (a -> b -> WithMeta c) -> a -> b -> c
convertxx0 f x1 x2 = value $ f x1 x2

convertxx1 :: (a -> b -> c) -> a -> b -> WithMeta c
convertxx1 f x1 x2 = noMeta $ f x1 x2

convertx0x :: (a -> WithMeta b -> c) -> a -> b -> c
convertx0x f x1 x2 = f x1 (noMeta x2)

convertx00 :: (a -> WithMeta b -> WithMeta c) -> a -> b -> c
convertx00 f x1 x2 = value $ f x1 (noMeta x2)

convertx01 :: (a -> WithMeta b -> c) -> a -> b -> WithMeta c
convertx01 f x1 x2 = noMeta $ f x1 (noMeta x2)

convertx1x :: (a -> b -> c) -> a -> WithMeta b -> c
convertx1x f x1 x2 = f x1 (value x2)

convertx10 :: (a -> b -> WithMeta c) -> a -> WithMeta b -> c
convertx10 f x1 x2 = value $ f x1 (value x2)

convertx11 :: (a -> b -> c) -> a -> WithMeta b -> WithMeta c
convertx11 f x1 x2 = noMeta $ f x1 (value x2)

convert0xx :: (WithMeta a -> b -> c) -> a -> b -> c
convert0xx f x1 x2 = f (noMeta x1) x2

convert0x0 :: (WithMeta a -> b -> WithMeta c) -> a -> b -> c
convert0x0 f x1 x2 = value $ f (noMeta x1) x2

convert0x1 :: (WithMeta a -> b -> c) -> a -> b -> WithMeta c
convert0x1 f x1 x2 = noMeta $ f (noMeta x1) x2

convert00x :: (WithMeta a -> WithMeta b -> c) -> a -> b -> c
convert00x f x1 x2 = f (noMeta x1) (noMeta x2)

convert000 :: (WithMeta a -> WithMeta b -> WithMeta c) -> a -> b -> c
convert000 f x1 x2 = value $ f (noMeta x1) (noMeta x2)

convert001 :: (WithMeta a -> WithMeta b -> c) -> a -> b -> WithMeta c
convert001 f x1 x2 = noMeta $ f (noMeta x1) (noMeta x2)

convert01x :: (WithMeta a -> b -> c) -> a -> WithMeta b -> c
convert01x f x1 x2 = f (noMeta x1) (value x2)

convert010 :: (WithMeta a -> b -> WithMeta c) -> a -> WithMeta b -> c
convert010 f x1 x2 = value $ f (noMeta x1) (value x2)

convert011 :: (WithMeta a -> b -> c) -> a -> WithMeta b -> WithMeta c
convert011 f x1 x2 = noMeta $ f (noMeta x1) (value x2)

convert1xx :: (a -> b -> c) -> WithMeta a -> b -> c
convert1xx f x1 x2 = f (value x1) x2

convert1x0 :: (a -> b -> WithMeta c) -> WithMeta a -> b -> c
convert1x0 f x1 x2 = value $ f (value x1) x2

convert1x1 :: (a -> b -> c) -> WithMeta a -> b -> WithMeta c
convert1x1 f x1 x2 = noMeta $ f (value x1) x2

convert10x :: (a -> WithMeta b -> c) -> WithMeta a -> b -> c
convert10x f x1 x2 = f (value x1) (noMeta x2)

convert100 :: (a -> WithMeta b -> WithMeta c) -> WithMeta a -> b -> c
convert100 f x1 x2 = value $ f (value x1) (noMeta x2)

convert101 :: (a -> WithMeta b -> c) -> WithMeta a -> b -> WithMeta c
convert101 f x1 x2 = noMeta $ f (value x1) (noMeta x2)

convert11x :: (a -> b -> c) -> WithMeta a -> WithMeta b -> c
convert11x f x1 x2 = f (value x1) (value x2)

convert110 :: (a -> b -> WithMeta c) -> WithMeta a -> WithMeta b -> c
convert110 f x1 x2 = value $ f (value x1) (value x2)

convert111 :: (a -> b -> c) -> WithMeta a -> WithMeta b -> WithMeta c
convert111 f x1 x2 = noMeta $ f (value x1) (value x2)

convertxxx0 :: (a -> b -> c -> WithMeta d) -> a -> b -> c -> d
convertxxx0 f x1 x2 x3 = value $ f x1 x2 x3

convertxxx1 :: (a -> b -> c -> d) -> a -> b -> c -> WithMeta d
convertxxx1 f x1 x2 x3 = noMeta $ f x1 x2 x3

convertxx0x :: (a -> b -> WithMeta c -> d) -> a -> b -> c -> d
convertxx0x f x1 x2 x3 = f x1 x2 (noMeta x3)

convertxx00 :: (a -> b -> WithMeta c -> WithMeta d) -> a -> b -> c -> d
convertxx00 f x1 x2 x3 = value $ f x1 x2 (noMeta x3)

convertxx01 :: (a -> b -> WithMeta c -> d) -> a -> b -> c -> WithMeta d
convertxx01 f x1 x2 x3 = noMeta $ f x1 x2 (noMeta x3)

convertxx1x :: (a -> b -> c -> d) -> a -> b -> WithMeta c -> d
convertxx1x f x1 x2 x3 = f x1 x2 (value x3)

convertxx10 :: (a -> b -> c -> WithMeta d) -> a -> b -> WithMeta c -> d
convertxx10 f x1 x2 x3 = value $ f x1 x2 (value x3)

convertxx11 :: (a -> b -> c -> d) -> a -> b -> WithMeta c -> WithMeta d
convertxx11 f x1 x2 x3 = noMeta $ f x1 x2 (value x3)

convertx0xx :: (a -> WithMeta b -> c -> d) -> a -> b -> c -> d
convertx0xx f x1 x2 x3 = f x1 (noMeta x2) x3

convertx0x0 :: (a -> WithMeta b -> c -> WithMeta d) -> a -> b -> c -> d
convertx0x0 f x1 x2 x3 = value $ f x1 (noMeta x2) x3

convertx0x1 :: (a -> WithMeta b -> c -> d) -> a -> b -> c -> WithMeta d
convertx0x1 f x1 x2 x3 = noMeta $ f x1 (noMeta x2) x3

convertx00x :: (a -> WithMeta b -> WithMeta c -> d) -> a -> b -> c -> d
convertx00x f x1 x2 x3 = f x1 (noMeta x2) (noMeta x3)

convertx000 :: (a -> WithMeta b -> WithMeta c -> WithMeta d) -> a -> b -> c -> d
convertx000 f x1 x2 x3 = value $ f x1 (noMeta x2) (noMeta x3)

convertx001 :: (a -> WithMeta b -> WithMeta c -> d) -> a -> b -> c -> WithMeta d
convertx001 f x1 x2 x3 = noMeta $ f x1 (noMeta x2) (noMeta x3)

convertx01x :: (a -> WithMeta b -> c -> d) -> a -> b -> WithMeta c -> d
convertx01x f x1 x2 x3 = f x1 (noMeta x2) (value x3)

convertx010 :: (a -> WithMeta b -> c -> WithMeta d) -> a -> b -> WithMeta c -> d
convertx010 f x1 x2 x3 = value $ f x1 (noMeta x2) (value x3)

convertx011 :: (a -> WithMeta b -> c -> d) -> a -> b -> WithMeta c -> WithMeta d
convertx011 f x1 x2 x3 = noMeta $ f x1 (noMeta x2) (value x3)

convertx1xx :: (a -> b -> c -> d) -> a -> WithMeta b -> c -> d
convertx1xx f x1 x2 x3 = f x1 (value x2) x3

convertx1x0 :: (a -> b -> c -> WithMeta d) -> a -> WithMeta b -> c -> d
convertx1x0 f x1 x2 x3 = value $ f x1 (value x2) x3

convertx1x1 :: (a -> b -> c -> d) -> a -> WithMeta b -> c -> WithMeta d
convertx1x1 f x1 x2 x3 = noMeta $ f x1 (value x2) x3

convertx10x :: (a -> b -> WithMeta c -> d) -> a -> WithMeta b -> c -> d
convertx10x f x1 x2 x3 = f x1 (value x2) (noMeta x3)

convertx100 :: (a -> b -> WithMeta c -> WithMeta d) -> a -> WithMeta b -> c -> d
convertx100 f x1 x2 x3 = value $ f x1 (value x2) (noMeta x3)

convertx101 :: (a -> b -> WithMeta c -> d) -> a -> WithMeta b -> c -> WithMeta d
convertx101 f x1 x2 x3 = noMeta $ f x1 (value x2) (noMeta x3)

convertx11x :: (a -> b -> c -> d) -> a -> WithMeta b -> WithMeta c -> d
convertx11x f x1 x2 x3 = f x1 (value x2) (value x3)

convertx110 :: (a -> b -> c -> WithMeta d) -> a -> WithMeta b -> WithMeta c -> d
convertx110 f x1 x2 x3 = value $ f x1 (value x2) (value x3)

convertx111 :: (a -> b -> c -> d) -> a -> WithMeta b -> WithMeta c -> WithMeta d
convertx111 f x1 x2 x3 = noMeta $ f x1 (value x2) (value x3)

convert0xxx :: (WithMeta a -> b -> c -> d) -> a -> b -> c -> d
convert0xxx f x1 x2 x3 = f (noMeta x1) x2 x3

convert0xx0 :: (WithMeta a -> b -> c -> WithMeta d) -> a -> b -> c -> d
convert0xx0 f x1 x2 x3 = value $ f (noMeta x1) x2 x3

convert0xx1 :: (WithMeta a -> b -> c -> d) -> a -> b -> c -> WithMeta d
convert0xx1 f x1 x2 x3 = noMeta $ f (noMeta x1) x2 x3

convert0x0x :: (WithMeta a -> b -> WithMeta c -> d) -> a -> b -> c -> d
convert0x0x f x1 x2 x3 = f (noMeta x1) x2 (noMeta x3)

convert0x00 :: (WithMeta a -> b -> WithMeta c -> WithMeta d) -> a -> b -> c -> d
convert0x00 f x1 x2 x3 = value $ f (noMeta x1) x2 (noMeta x3)

convert0x01 :: (WithMeta a -> b -> WithMeta c -> d) -> a -> b -> c -> WithMeta d
convert0x01 f x1 x2 x3 = noMeta $ f (noMeta x1) x2 (noMeta x3)

convert0x1x :: (WithMeta a -> b -> c -> d) -> a -> b -> WithMeta c -> d
convert0x1x f x1 x2 x3 = f (noMeta x1) x2 (value x3)

convert0x10 :: (WithMeta a -> b -> c -> WithMeta d) -> a -> b -> WithMeta c -> d
convert0x10 f x1 x2 x3 = value $ f (noMeta x1) x2 (value x3)

convert0x11 :: (WithMeta a -> b -> c -> d) -> a -> b -> WithMeta c -> WithMeta d
convert0x11 f x1 x2 x3 = noMeta $ f (noMeta x1) x2 (value x3)

convert00xx :: (WithMeta a -> WithMeta b -> c -> d) -> a -> b -> c -> d
convert00xx f x1 x2 x3 = f (noMeta x1) (noMeta x2) x3

convert00x0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d) -> a -> b -> c -> d
convert00x0 f x1 x2 x3 = value $ f (noMeta x1) (noMeta x2) x3

convert00x1 :: (WithMeta a -> WithMeta b -> c -> d) -> a -> b -> c -> WithMeta d
convert00x1 f x1 x2 x3 = noMeta $ f (noMeta x1) (noMeta x2) x3

convert000x :: (WithMeta a -> WithMeta b -> WithMeta c -> d) -> a -> b -> c -> d
convert000x f x1 x2 x3 = f (noMeta x1) (noMeta x2) (noMeta x3)

convert0000 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d) -> a -> b -> c -> d
convert0000 f x1 x2 x3 = value $ f (noMeta x1) (noMeta x2) (noMeta x3)

convert0001 :: (WithMeta a -> WithMeta b -> WithMeta c -> d) -> a -> b -> c -> WithMeta d
convert0001 f x1 x2 x3 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3)

convert001x :: (WithMeta a -> WithMeta b -> c -> d) -> a -> b -> WithMeta c -> d
convert001x f x1 x2 x3 = f (noMeta x1) (noMeta x2) (value x3)

convert0010 :: (WithMeta a -> WithMeta b -> c -> WithMeta d) -> a -> b -> WithMeta c -> d
convert0010 f x1 x2 x3 = value $ f (noMeta x1) (noMeta x2) (value x3)

convert0011 :: (WithMeta a -> WithMeta b -> c -> d) -> a -> b -> WithMeta c -> WithMeta d
convert0011 f x1 x2 x3 = noMeta $ f (noMeta x1) (noMeta x2) (value x3)

convert01xx :: (WithMeta a -> b -> c -> d) -> a -> WithMeta b -> c -> d
convert01xx f x1 x2 x3 = f (noMeta x1) (value x2) x3

convert01x0 :: (WithMeta a -> b -> c -> WithMeta d) -> a -> WithMeta b -> c -> d
convert01x0 f x1 x2 x3 = value $ f (noMeta x1) (value x2) x3

convert01x1 :: (WithMeta a -> b -> c -> d) -> a -> WithMeta b -> c -> WithMeta d
convert01x1 f x1 x2 x3 = noMeta $ f (noMeta x1) (value x2) x3

convert010x :: (WithMeta a -> b -> WithMeta c -> d) -> a -> WithMeta b -> c -> d
convert010x f x1 x2 x3 = f (noMeta x1) (value x2) (noMeta x3)

convert0100 :: (WithMeta a -> b -> WithMeta c -> WithMeta d) -> a -> WithMeta b -> c -> d
convert0100 f x1 x2 x3 = value $ f (noMeta x1) (value x2) (noMeta x3)

convert0101 :: (WithMeta a -> b -> WithMeta c -> d) -> a -> WithMeta b -> c -> WithMeta d
convert0101 f x1 x2 x3 = noMeta $ f (noMeta x1) (value x2) (noMeta x3)

convert011x :: (WithMeta a -> b -> c -> d) -> a -> WithMeta b -> WithMeta c -> d
convert011x f x1 x2 x3 = f (noMeta x1) (value x2) (value x3)

convert0110 :: (WithMeta a -> b -> c -> WithMeta d) -> a -> WithMeta b -> WithMeta c -> d
convert0110 f x1 x2 x3 = value $ f (noMeta x1) (value x2) (value x3)

convert0111 :: (WithMeta a -> b -> c -> d) -> a -> WithMeta b -> WithMeta c -> WithMeta d
convert0111 f x1 x2 x3 = noMeta $ f (noMeta x1) (value x2) (value x3)

convert1xxx :: (a -> b -> c -> d) -> WithMeta a -> b -> c -> d
convert1xxx f x1 x2 x3 = f (value x1) x2 x3

convert1xx0 :: (a -> b -> c -> WithMeta d) -> WithMeta a -> b -> c -> d
convert1xx0 f x1 x2 x3 = value $ f (value x1) x2 x3

convert1xx1 :: (a -> b -> c -> d) -> WithMeta a -> b -> c -> WithMeta d
convert1xx1 f x1 x2 x3 = noMeta $ f (value x1) x2 x3

convert1x0x :: (a -> b -> WithMeta c -> d) -> WithMeta a -> b -> c -> d
convert1x0x f x1 x2 x3 = f (value x1) x2 (noMeta x3)

convert1x00 :: (a -> b -> WithMeta c -> WithMeta d) -> WithMeta a -> b -> c -> d
convert1x00 f x1 x2 x3 = value $ f (value x1) x2 (noMeta x3)

convert1x01 :: (a -> b -> WithMeta c -> d) -> WithMeta a -> b -> c -> WithMeta d
convert1x01 f x1 x2 x3 = noMeta $ f (value x1) x2 (noMeta x3)

convert1x1x :: (a -> b -> c -> d) -> WithMeta a -> b -> WithMeta c -> d
convert1x1x f x1 x2 x3 = f (value x1) x2 (value x3)

convert1x10 :: (a -> b -> c -> WithMeta d) -> WithMeta a -> b -> WithMeta c -> d
convert1x10 f x1 x2 x3 = value $ f (value x1) x2 (value x3)

convert1x11 :: (a -> b -> c -> d) -> WithMeta a -> b -> WithMeta c -> WithMeta d
convert1x11 f x1 x2 x3 = noMeta $ f (value x1) x2 (value x3)

convert10xx :: (a -> WithMeta b -> c -> d) -> WithMeta a -> b -> c -> d
convert10xx f x1 x2 x3 = f (value x1) (noMeta x2) x3

convert10x0 :: (a -> WithMeta b -> c -> WithMeta d) -> WithMeta a -> b -> c -> d
convert10x0 f x1 x2 x3 = value $ f (value x1) (noMeta x2) x3

convert10x1 :: (a -> WithMeta b -> c -> d) -> WithMeta a -> b -> c -> WithMeta d
convert10x1 f x1 x2 x3 = noMeta $ f (value x1) (noMeta x2) x3

convert100x :: (a -> WithMeta b -> WithMeta c -> d) -> WithMeta a -> b -> c -> d
convert100x f x1 x2 x3 = f (value x1) (noMeta x2) (noMeta x3)

convert1000 :: (a -> WithMeta b -> WithMeta c -> WithMeta d) -> WithMeta a -> b -> c -> d
convert1000 f x1 x2 x3 = value $ f (value x1) (noMeta x2) (noMeta x3)

convert1001 :: (a -> WithMeta b -> WithMeta c -> d) -> WithMeta a -> b -> c -> WithMeta d
convert1001 f x1 x2 x3 = noMeta $ f (value x1) (noMeta x2) (noMeta x3)

convert101x :: (a -> WithMeta b -> c -> d) -> WithMeta a -> b -> WithMeta c -> d
convert101x f x1 x2 x3 = f (value x1) (noMeta x2) (value x3)

convert1010 :: (a -> WithMeta b -> c -> WithMeta d) -> WithMeta a -> b -> WithMeta c -> d
convert1010 f x1 x2 x3 = value $ f (value x1) (noMeta x2) (value x3)

convert1011 :: (a -> WithMeta b -> c -> d) -> WithMeta a -> b -> WithMeta c -> WithMeta d
convert1011 f x1 x2 x3 = noMeta $ f (value x1) (noMeta x2) (value x3)

convert11xx :: (a -> b -> c -> d) -> WithMeta a -> WithMeta b -> c -> d
convert11xx f x1 x2 x3 = f (value x1) (value x2) x3

convert11x0 :: (a -> b -> c -> WithMeta d) -> WithMeta a -> WithMeta b -> c -> d
convert11x0 f x1 x2 x3 = value $ f (value x1) (value x2) x3

convert11x1 :: (a -> b -> c -> d) -> WithMeta a -> WithMeta b -> c -> WithMeta d
convert11x1 f x1 x2 x3 = noMeta $ f (value x1) (value x2) x3

convert110x :: (a -> b -> WithMeta c -> d) -> WithMeta a -> WithMeta b -> c -> d
convert110x f x1 x2 x3 = f (value x1) (value x2) (noMeta x3)

convert1100 :: (a -> b -> WithMeta c -> WithMeta d) -> WithMeta a -> WithMeta b -> c -> d
convert1100 f x1 x2 x3 = value $ f (value x1) (value x2) (noMeta x3)

convert1101 :: (a -> b -> WithMeta c -> d) -> WithMeta a -> WithMeta b -> c -> WithMeta d
convert1101 f x1 x2 x3 = noMeta $ f (value x1) (value x2) (noMeta x3)

convert111x :: (a -> b -> c -> d) -> WithMeta a -> WithMeta b -> WithMeta c -> d
convert111x f x1 x2 x3 = f (value x1) (value x2) (value x3)

convert1110 :: (a -> b -> c -> WithMeta d) -> WithMeta a -> WithMeta b -> WithMeta c -> d
convert1110 f x1 x2 x3 = value $ f (value x1) (value x2) (value x3)

convert1111 :: (a -> b -> c -> d) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d
convert1111 f x1 x2 x3 = noMeta $ f (value x1) (value x2) (value x3)

convertxxxx0 :: (a -> b -> c -> d -> WithMeta e) -> a -> b -> c -> d -> e
convertxxxx0 f x1 x2 x3 x4 = value $ f x1 x2 x3 x4

convertxxxx1 :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> WithMeta e
convertxxxx1 f x1 x2 x3 x4 = noMeta $ f x1 x2 x3 x4

convertxxx0x :: (a -> b -> c -> WithMeta d -> e) -> a -> b -> c -> d -> e
convertxxx0x f x1 x2 x3 x4 = f x1 x2 x3 (noMeta x4)

convertxxx00 :: (a -> b -> c -> WithMeta d -> WithMeta e) -> a -> b -> c -> d -> e
convertxxx00 f x1 x2 x3 x4 = value $ f x1 x2 x3 (noMeta x4)

convertxxx01 :: (a -> b -> c -> WithMeta d -> e) -> a -> b -> c -> d -> WithMeta e
convertxxx01 f x1 x2 x3 x4 = noMeta $ f x1 x2 x3 (noMeta x4)

convertxxx1x :: (a -> b -> c -> d -> e) -> a -> b -> c -> WithMeta d -> e
convertxxx1x f x1 x2 x3 x4 = f x1 x2 x3 (value x4)

convertxxx10 :: (a -> b -> c -> d -> WithMeta e) -> a -> b -> c -> WithMeta d -> e
convertxxx10 f x1 x2 x3 x4 = value $ f x1 x2 x3 (value x4)

convertxxx11 :: (a -> b -> c -> d -> e) -> a -> b -> c -> WithMeta d -> WithMeta e
convertxxx11 f x1 x2 x3 x4 = noMeta $ f x1 x2 x3 (value x4)

convertxx0xx :: (a -> b -> WithMeta c -> d -> e) -> a -> b -> c -> d -> e
convertxx0xx f x1 x2 x3 x4 = f x1 x2 (noMeta x3) x4

convertxx0x0 :: (a -> b -> WithMeta c -> d -> WithMeta e) -> a -> b -> c -> d -> e
convertxx0x0 f x1 x2 x3 x4 = value $ f x1 x2 (noMeta x3) x4

convertxx0x1 :: (a -> b -> WithMeta c -> d -> e) -> a -> b -> c -> d -> WithMeta e
convertxx0x1 f x1 x2 x3 x4 = noMeta $ f x1 x2 (noMeta x3) x4

convertxx00x :: (a -> b -> WithMeta c -> WithMeta d -> e) -> a -> b -> c -> d -> e
convertxx00x f x1 x2 x3 x4 = f x1 x2 (noMeta x3) (noMeta x4)

convertxx000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e) -> a -> b -> c -> d -> e
convertxx000 f x1 x2 x3 x4 = value $ f x1 x2 (noMeta x3) (noMeta x4)

convertxx001 :: (a -> b -> WithMeta c -> WithMeta d -> e) -> a -> b -> c -> d -> WithMeta e
convertxx001 f x1 x2 x3 x4 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4)

convertxx01x :: (a -> b -> WithMeta c -> d -> e) -> a -> b -> c -> WithMeta d -> e
convertxx01x f x1 x2 x3 x4 = f x1 x2 (noMeta x3) (value x4)

convertxx010 :: (a -> b -> WithMeta c -> d -> WithMeta e) -> a -> b -> c -> WithMeta d -> e
convertxx010 f x1 x2 x3 x4 = value $ f x1 x2 (noMeta x3) (value x4)

convertxx011 :: (a -> b -> WithMeta c -> d -> e) -> a -> b -> c -> WithMeta d -> WithMeta e
convertxx011 f x1 x2 x3 x4 = noMeta $ f x1 x2 (noMeta x3) (value x4)

convertxx1xx :: (a -> b -> c -> d -> e) -> a -> b -> WithMeta c -> d -> e
convertxx1xx f x1 x2 x3 x4 = f x1 x2 (value x3) x4

convertxx1x0 :: (a -> b -> c -> d -> WithMeta e) -> a -> b -> WithMeta c -> d -> e
convertxx1x0 f x1 x2 x3 x4 = value $ f x1 x2 (value x3) x4

convertxx1x1 :: (a -> b -> c -> d -> e) -> a -> b -> WithMeta c -> d -> WithMeta e
convertxx1x1 f x1 x2 x3 x4 = noMeta $ f x1 x2 (value x3) x4

convertxx10x :: (a -> b -> c -> WithMeta d -> e) -> a -> b -> WithMeta c -> d -> e
convertxx10x f x1 x2 x3 x4 = f x1 x2 (value x3) (noMeta x4)

convertxx100 :: (a -> b -> c -> WithMeta d -> WithMeta e) -> a -> b -> WithMeta c -> d -> e
convertxx100 f x1 x2 x3 x4 = value $ f x1 x2 (value x3) (noMeta x4)

convertxx101 :: (a -> b -> c -> WithMeta d -> e) -> a -> b -> WithMeta c -> d -> WithMeta e
convertxx101 f x1 x2 x3 x4 = noMeta $ f x1 x2 (value x3) (noMeta x4)

convertxx11x :: (a -> b -> c -> d -> e) -> a -> b -> WithMeta c -> WithMeta d -> e
convertxx11x f x1 x2 x3 x4 = f x1 x2 (value x3) (value x4)

convertxx110 :: (a -> b -> c -> d -> WithMeta e) -> a -> b -> WithMeta c -> WithMeta d -> e
convertxx110 f x1 x2 x3 x4 = value $ f x1 x2 (value x3) (value x4)

convertxx111 :: (a -> b -> c -> d -> e) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e
convertxx111 f x1 x2 x3 x4 = noMeta $ f x1 x2 (value x3) (value x4)

convertx0xxx :: (a -> WithMeta b -> c -> d -> e) -> a -> b -> c -> d -> e
convertx0xxx f x1 x2 x3 x4 = f x1 (noMeta x2) x3 x4

convertx0xx0 :: (a -> WithMeta b -> c -> d -> WithMeta e) -> a -> b -> c -> d -> e
convertx0xx0 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) x3 x4

convertx0xx1 :: (a -> WithMeta b -> c -> d -> e) -> a -> b -> c -> d -> WithMeta e
convertx0xx1 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) x3 x4

convertx0x0x :: (a -> WithMeta b -> c -> WithMeta d -> e) -> a -> b -> c -> d -> e
convertx0x0x f x1 x2 x3 x4 = f x1 (noMeta x2) x3 (noMeta x4)

convertx0x00 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e) -> a -> b -> c -> d -> e
convertx0x00 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) x3 (noMeta x4)

convertx0x01 :: (a -> WithMeta b -> c -> WithMeta d -> e) -> a -> b -> c -> d -> WithMeta e
convertx0x01 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4)

convertx0x1x :: (a -> WithMeta b -> c -> d -> e) -> a -> b -> c -> WithMeta d -> e
convertx0x1x f x1 x2 x3 x4 = f x1 (noMeta x2) x3 (value x4)

convertx0x10 :: (a -> WithMeta b -> c -> d -> WithMeta e) -> a -> b -> c -> WithMeta d -> e
convertx0x10 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) x3 (value x4)

convertx0x11 :: (a -> WithMeta b -> c -> d -> e) -> a -> b -> c -> WithMeta d -> WithMeta e
convertx0x11 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) x3 (value x4)

convertx00xx :: (a -> WithMeta b -> WithMeta c -> d -> e) -> a -> b -> c -> d -> e
convertx00xx f x1 x2 x3 x4 = f x1 (noMeta x2) (noMeta x3) x4

convertx00x0 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e) -> a -> b -> c -> d -> e
convertx00x0 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) (noMeta x3) x4

convertx00x1 :: (a -> WithMeta b -> WithMeta c -> d -> e) -> a -> b -> c -> d -> WithMeta e
convertx00x1 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4

convertx000x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e) -> a -> b -> c -> d -> e
convertx000x f x1 x2 x3 x4 = f x1 (noMeta x2) (noMeta x3) (noMeta x4)

convertx0000 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e) -> a -> b -> c -> d -> e
convertx0000 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4)

convertx0001 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e) -> a -> b -> c -> d -> WithMeta e
convertx0001 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4)

convertx001x :: (a -> WithMeta b -> WithMeta c -> d -> e) -> a -> b -> c -> WithMeta d -> e
convertx001x f x1 x2 x3 x4 = f x1 (noMeta x2) (noMeta x3) (value x4)

convertx0010 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e) -> a -> b -> c -> WithMeta d -> e
convertx0010 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) (noMeta x3) (value x4)

convertx0011 :: (a -> WithMeta b -> WithMeta c -> d -> e) -> a -> b -> c -> WithMeta d -> WithMeta e
convertx0011 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4)

convertx01xx :: (a -> WithMeta b -> c -> d -> e) -> a -> b -> WithMeta c -> d -> e
convertx01xx f x1 x2 x3 x4 = f x1 (noMeta x2) (value x3) x4

convertx01x0 :: (a -> WithMeta b -> c -> d -> WithMeta e) -> a -> b -> WithMeta c -> d -> e
convertx01x0 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) (value x3) x4

convertx01x1 :: (a -> WithMeta b -> c -> d -> e) -> a -> b -> WithMeta c -> d -> WithMeta e
convertx01x1 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) (value x3) x4

convertx010x :: (a -> WithMeta b -> c -> WithMeta d -> e) -> a -> b -> WithMeta c -> d -> e
convertx010x f x1 x2 x3 x4 = f x1 (noMeta x2) (value x3) (noMeta x4)

convertx0100 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e) -> a -> b -> WithMeta c -> d -> e
convertx0100 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) (value x3) (noMeta x4)

convertx0101 :: (a -> WithMeta b -> c -> WithMeta d -> e) -> a -> b -> WithMeta c -> d -> WithMeta e
convertx0101 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4)

convertx011x :: (a -> WithMeta b -> c -> d -> e) -> a -> b -> WithMeta c -> WithMeta d -> e
convertx011x f x1 x2 x3 x4 = f x1 (noMeta x2) (value x3) (value x4)

convertx0110 :: (a -> WithMeta b -> c -> d -> WithMeta e) -> a -> b -> WithMeta c -> WithMeta d -> e
convertx0110 f x1 x2 x3 x4 = value $ f x1 (noMeta x2) (value x3) (value x4)

convertx0111 :: (a -> WithMeta b -> c -> d -> e) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e
convertx0111 f x1 x2 x3 x4 = noMeta $ f x1 (noMeta x2) (value x3) (value x4)

convertx1xxx :: (a -> b -> c -> d -> e) -> a -> WithMeta b -> c -> d -> e
convertx1xxx f x1 x2 x3 x4 = f x1 (value x2) x3 x4

convertx1xx0 :: (a -> b -> c -> d -> WithMeta e) -> a -> WithMeta b -> c -> d -> e
convertx1xx0 f x1 x2 x3 x4 = value $ f x1 (value x2) x3 x4

convertx1xx1 :: (a -> b -> c -> d -> e) -> a -> WithMeta b -> c -> d -> WithMeta e
convertx1xx1 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) x3 x4

convertx1x0x :: (a -> b -> c -> WithMeta d -> e) -> a -> WithMeta b -> c -> d -> e
convertx1x0x f x1 x2 x3 x4 = f x1 (value x2) x3 (noMeta x4)

convertx1x00 :: (a -> b -> c -> WithMeta d -> WithMeta e) -> a -> WithMeta b -> c -> d -> e
convertx1x00 f x1 x2 x3 x4 = value $ f x1 (value x2) x3 (noMeta x4)

convertx1x01 :: (a -> b -> c -> WithMeta d -> e) -> a -> WithMeta b -> c -> d -> WithMeta e
convertx1x01 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) x3 (noMeta x4)

convertx1x1x :: (a -> b -> c -> d -> e) -> a -> WithMeta b -> c -> WithMeta d -> e
convertx1x1x f x1 x2 x3 x4 = f x1 (value x2) x3 (value x4)

convertx1x10 :: (a -> b -> c -> d -> WithMeta e) -> a -> WithMeta b -> c -> WithMeta d -> e
convertx1x10 f x1 x2 x3 x4 = value $ f x1 (value x2) x3 (value x4)

convertx1x11 :: (a -> b -> c -> d -> e) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e
convertx1x11 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) x3 (value x4)

convertx10xx :: (a -> b -> WithMeta c -> d -> e) -> a -> WithMeta b -> c -> d -> e
convertx10xx f x1 x2 x3 x4 = f x1 (value x2) (noMeta x3) x4

convertx10x0 :: (a -> b -> WithMeta c -> d -> WithMeta e) -> a -> WithMeta b -> c -> d -> e
convertx10x0 f x1 x2 x3 x4 = value $ f x1 (value x2) (noMeta x3) x4

convertx10x1 :: (a -> b -> WithMeta c -> d -> e) -> a -> WithMeta b -> c -> d -> WithMeta e
convertx10x1 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) (noMeta x3) x4

convertx100x :: (a -> b -> WithMeta c -> WithMeta d -> e) -> a -> WithMeta b -> c -> d -> e
convertx100x f x1 x2 x3 x4 = f x1 (value x2) (noMeta x3) (noMeta x4)

convertx1000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e) -> a -> WithMeta b -> c -> d -> e
convertx1000 f x1 x2 x3 x4 = value $ f x1 (value x2) (noMeta x3) (noMeta x4)

convertx1001 :: (a -> b -> WithMeta c -> WithMeta d -> e) -> a -> WithMeta b -> c -> d -> WithMeta e
convertx1001 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4)

convertx101x :: (a -> b -> WithMeta c -> d -> e) -> a -> WithMeta b -> c -> WithMeta d -> e
convertx101x f x1 x2 x3 x4 = f x1 (value x2) (noMeta x3) (value x4)

convertx1010 :: (a -> b -> WithMeta c -> d -> WithMeta e) -> a -> WithMeta b -> c -> WithMeta d -> e
convertx1010 f x1 x2 x3 x4 = value $ f x1 (value x2) (noMeta x3) (value x4)

convertx1011 :: (a -> b -> WithMeta c -> d -> e) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e
convertx1011 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) (noMeta x3) (value x4)

convertx11xx :: (a -> b -> c -> d -> e) -> a -> WithMeta b -> WithMeta c -> d -> e
convertx11xx f x1 x2 x3 x4 = f x1 (value x2) (value x3) x4

convertx11x0 :: (a -> b -> c -> d -> WithMeta e) -> a -> WithMeta b -> WithMeta c -> d -> e
convertx11x0 f x1 x2 x3 x4 = value $ f x1 (value x2) (value x3) x4

convertx11x1 :: (a -> b -> c -> d -> e) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e
convertx11x1 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) (value x3) x4

convertx110x :: (a -> b -> c -> WithMeta d -> e) -> a -> WithMeta b -> WithMeta c -> d -> e
convertx110x f x1 x2 x3 x4 = f x1 (value x2) (value x3) (noMeta x4)

convertx1100 :: (a -> b -> c -> WithMeta d -> WithMeta e) -> a -> WithMeta b -> WithMeta c -> d -> e
convertx1100 f x1 x2 x3 x4 = value $ f x1 (value x2) (value x3) (noMeta x4)

convertx1101 :: (a -> b -> c -> WithMeta d -> e) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e
convertx1101 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) (value x3) (noMeta x4)

convertx111x :: (a -> b -> c -> d -> e) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e
convertx111x f x1 x2 x3 x4 = f x1 (value x2) (value x3) (value x4)

convertx1110 :: (a -> b -> c -> d -> WithMeta e) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e
convertx1110 f x1 x2 x3 x4 = value $ f x1 (value x2) (value x3) (value x4)

convertx1111 :: (a -> b -> c -> d -> e) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e
convertx1111 f x1 x2 x3 x4 = noMeta $ f x1 (value x2) (value x3) (value x4)

convert0xxxx :: (WithMeta a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
convert0xxxx f x1 x2 x3 x4 = f (noMeta x1) x2 x3 x4

convert0xxx0 :: (WithMeta a -> b -> c -> d -> WithMeta e) -> a -> b -> c -> d -> e
convert0xxx0 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 x3 x4

convert0xxx1 :: (WithMeta a -> b -> c -> d -> e) -> a -> b -> c -> d -> WithMeta e
convert0xxx1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 x3 x4

convert0xx0x :: (WithMeta a -> b -> c -> WithMeta d -> e) -> a -> b -> c -> d -> e
convert0xx0x f x1 x2 x3 x4 = f (noMeta x1) x2 x3 (noMeta x4)

convert0xx00 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e) -> a -> b -> c -> d -> e
convert0xx00 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 x3 (noMeta x4)

convert0xx01 :: (WithMeta a -> b -> c -> WithMeta d -> e) -> a -> b -> c -> d -> WithMeta e
convert0xx01 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4)

convert0xx1x :: (WithMeta a -> b -> c -> d -> e) -> a -> b -> c -> WithMeta d -> e
convert0xx1x f x1 x2 x3 x4 = f (noMeta x1) x2 x3 (value x4)

convert0xx10 :: (WithMeta a -> b -> c -> d -> WithMeta e) -> a -> b -> c -> WithMeta d -> e
convert0xx10 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 x3 (value x4)

convert0xx11 :: (WithMeta a -> b -> c -> d -> e) -> a -> b -> c -> WithMeta d -> WithMeta e
convert0xx11 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 x3 (value x4)

convert0x0xx :: (WithMeta a -> b -> WithMeta c -> d -> e) -> a -> b -> c -> d -> e
convert0x0xx f x1 x2 x3 x4 = f (noMeta x1) x2 (noMeta x3) x4

convert0x0x0 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e) -> a -> b -> c -> d -> e
convert0x0x0 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 (noMeta x3) x4

convert0x0x1 :: (WithMeta a -> b -> WithMeta c -> d -> e) -> a -> b -> c -> d -> WithMeta e
convert0x0x1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4

convert0x00x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e) -> a -> b -> c -> d -> e
convert0x00x f x1 x2 x3 x4 = f (noMeta x1) x2 (noMeta x3) (noMeta x4)

convert0x000 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e) -> a -> b -> c -> d -> e
convert0x000 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4)

convert0x001 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e) -> a -> b -> c -> d -> WithMeta e
convert0x001 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4)

convert0x01x :: (WithMeta a -> b -> WithMeta c -> d -> e) -> a -> b -> c -> WithMeta d -> e
convert0x01x f x1 x2 x3 x4 = f (noMeta x1) x2 (noMeta x3) (value x4)

convert0x010 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e) -> a -> b -> c -> WithMeta d -> e
convert0x010 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 (noMeta x3) (value x4)

convert0x011 :: (WithMeta a -> b -> WithMeta c -> d -> e) -> a -> b -> c -> WithMeta d -> WithMeta e
convert0x011 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4)

convert0x1xx :: (WithMeta a -> b -> c -> d -> e) -> a -> b -> WithMeta c -> d -> e
convert0x1xx f x1 x2 x3 x4 = f (noMeta x1) x2 (value x3) x4

convert0x1x0 :: (WithMeta a -> b -> c -> d -> WithMeta e) -> a -> b -> WithMeta c -> d -> e
convert0x1x0 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 (value x3) x4

convert0x1x1 :: (WithMeta a -> b -> c -> d -> e) -> a -> b -> WithMeta c -> d -> WithMeta e
convert0x1x1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 (value x3) x4

convert0x10x :: (WithMeta a -> b -> c -> WithMeta d -> e) -> a -> b -> WithMeta c -> d -> e
convert0x10x f x1 x2 x3 x4 = f (noMeta x1) x2 (value x3) (noMeta x4)

convert0x100 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e) -> a -> b -> WithMeta c -> d -> e
convert0x100 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 (value x3) (noMeta x4)

convert0x101 :: (WithMeta a -> b -> c -> WithMeta d -> e) -> a -> b -> WithMeta c -> d -> WithMeta e
convert0x101 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4)

convert0x11x :: (WithMeta a -> b -> c -> d -> e) -> a -> b -> WithMeta c -> WithMeta d -> e
convert0x11x f x1 x2 x3 x4 = f (noMeta x1) x2 (value x3) (value x4)

convert0x110 :: (WithMeta a -> b -> c -> d -> WithMeta e) -> a -> b -> WithMeta c -> WithMeta d -> e
convert0x110 f x1 x2 x3 x4 = value $ f (noMeta x1) x2 (value x3) (value x4)

convert0x111 :: (WithMeta a -> b -> c -> d -> e) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e
convert0x111 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) x2 (value x3) (value x4)

convert00xxx :: (WithMeta a -> WithMeta b -> c -> d -> e) -> a -> b -> c -> d -> e
convert00xxx f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) x3 x4

convert00xx0 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e) -> a -> b -> c -> d -> e
convert00xx0 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) x3 x4

convert00xx1 :: (WithMeta a -> WithMeta b -> c -> d -> e) -> a -> b -> c -> d -> WithMeta e
convert00xx1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4

convert00x0x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e) -> a -> b -> c -> d -> e
convert00x0x f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) x3 (noMeta x4)

convert00x00 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e) -> a -> b -> c -> d -> e
convert00x00 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4)

convert00x01 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e) -> a -> b -> c -> d -> WithMeta e
convert00x01 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4)

convert00x1x :: (WithMeta a -> WithMeta b -> c -> d -> e) -> a -> b -> c -> WithMeta d -> e
convert00x1x f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) x3 (value x4)

convert00x10 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e) -> a -> b -> c -> WithMeta d -> e
convert00x10 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) x3 (value x4)

convert00x11 :: (WithMeta a -> WithMeta b -> c -> d -> e) -> a -> b -> c -> WithMeta d -> WithMeta e
convert00x11 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4)

convert000xx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e) -> a -> b -> c -> d -> e
convert000xx f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) (noMeta x3) x4

convert000x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e) -> a -> b -> c -> d -> e
convert000x0 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4

convert000x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e) -> a -> b -> c -> d -> WithMeta e
convert000x1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4

convert0000x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e) -> a -> b -> c -> d -> e
convert0000x f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4)

convert00000 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e) -> a -> b -> c -> d -> e
convert00000 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4)

convert00001 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e) -> a -> b -> c -> d -> WithMeta e
convert00001 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4)

convert0001x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e) -> a -> b -> c -> WithMeta d -> e
convert0001x f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4)

convert00010 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e) -> a -> b -> c -> WithMeta d -> e
convert00010 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4)

convert00011 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e) -> a -> b -> c -> WithMeta d -> WithMeta e
convert00011 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4)

convert001xx :: (WithMeta a -> WithMeta b -> c -> d -> e) -> a -> b -> WithMeta c -> d -> e
convert001xx f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) (value x3) x4

convert001x0 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e) -> a -> b -> WithMeta c -> d -> e
convert001x0 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) (value x3) x4

convert001x1 :: (WithMeta a -> WithMeta b -> c -> d -> e) -> a -> b -> WithMeta c -> d -> WithMeta e
convert001x1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4

convert0010x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e) -> a -> b -> WithMeta c -> d -> e
convert0010x f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4)

convert00100 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e) -> a -> b -> WithMeta c -> d -> e
convert00100 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4)

convert00101 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e) -> a -> b -> WithMeta c -> d -> WithMeta e
convert00101 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4)

convert0011x :: (WithMeta a -> WithMeta b -> c -> d -> e) -> a -> b -> WithMeta c -> WithMeta d -> e
convert0011x f x1 x2 x3 x4 = f (noMeta x1) (noMeta x2) (value x3) (value x4)

convert00110 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e) -> a -> b -> WithMeta c -> WithMeta d -> e
convert00110 f x1 x2 x3 x4 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4)

convert00111 :: (WithMeta a -> WithMeta b -> c -> d -> e) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e
convert00111 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4)

convert01xxx :: (WithMeta a -> b -> c -> d -> e) -> a -> WithMeta b -> c -> d -> e
convert01xxx f x1 x2 x3 x4 = f (noMeta x1) (value x2) x3 x4

convert01xx0 :: (WithMeta a -> b -> c -> d -> WithMeta e) -> a -> WithMeta b -> c -> d -> e
convert01xx0 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) x3 x4

convert01xx1 :: (WithMeta a -> b -> c -> d -> e) -> a -> WithMeta b -> c -> d -> WithMeta e
convert01xx1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) x3 x4

convert01x0x :: (WithMeta a -> b -> c -> WithMeta d -> e) -> a -> WithMeta b -> c -> d -> e
convert01x0x f x1 x2 x3 x4 = f (noMeta x1) (value x2) x3 (noMeta x4)

convert01x00 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e) -> a -> WithMeta b -> c -> d -> e
convert01x00 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) x3 (noMeta x4)

convert01x01 :: (WithMeta a -> b -> c -> WithMeta d -> e) -> a -> WithMeta b -> c -> d -> WithMeta e
convert01x01 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4)

convert01x1x :: (WithMeta a -> b -> c -> d -> e) -> a -> WithMeta b -> c -> WithMeta d -> e
convert01x1x f x1 x2 x3 x4 = f (noMeta x1) (value x2) x3 (value x4)

convert01x10 :: (WithMeta a -> b -> c -> d -> WithMeta e) -> a -> WithMeta b -> c -> WithMeta d -> e
convert01x10 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) x3 (value x4)

convert01x11 :: (WithMeta a -> b -> c -> d -> e) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e
convert01x11 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) x3 (value x4)

convert010xx :: (WithMeta a -> b -> WithMeta c -> d -> e) -> a -> WithMeta b -> c -> d -> e
convert010xx f x1 x2 x3 x4 = f (noMeta x1) (value x2) (noMeta x3) x4

convert010x0 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e) -> a -> WithMeta b -> c -> d -> e
convert010x0 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) (noMeta x3) x4

convert010x1 :: (WithMeta a -> b -> WithMeta c -> d -> e) -> a -> WithMeta b -> c -> d -> WithMeta e
convert010x1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4

convert0100x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e) -> a -> WithMeta b -> c -> d -> e
convert0100x f x1 x2 x3 x4 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4)

convert01000 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e) -> a -> WithMeta b -> c -> d -> e
convert01000 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4)

convert01001 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e) -> a -> WithMeta b -> c -> d -> WithMeta e
convert01001 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4)

convert0101x :: (WithMeta a -> b -> WithMeta c -> d -> e) -> a -> WithMeta b -> c -> WithMeta d -> e
convert0101x f x1 x2 x3 x4 = f (noMeta x1) (value x2) (noMeta x3) (value x4)

convert01010 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e) -> a -> WithMeta b -> c -> WithMeta d -> e
convert01010 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4)

convert01011 :: (WithMeta a -> b -> WithMeta c -> d -> e) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e
convert01011 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4)

convert011xx :: (WithMeta a -> b -> c -> d -> e) -> a -> WithMeta b -> WithMeta c -> d -> e
convert011xx f x1 x2 x3 x4 = f (noMeta x1) (value x2) (value x3) x4

convert011x0 :: (WithMeta a -> b -> c -> d -> WithMeta e) -> a -> WithMeta b -> WithMeta c -> d -> e
convert011x0 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) (value x3) x4

convert011x1 :: (WithMeta a -> b -> c -> d -> e) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e
convert011x1 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) (value x3) x4

convert0110x :: (WithMeta a -> b -> c -> WithMeta d -> e) -> a -> WithMeta b -> WithMeta c -> d -> e
convert0110x f x1 x2 x3 x4 = f (noMeta x1) (value x2) (value x3) (noMeta x4)

convert01100 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e) -> a -> WithMeta b -> WithMeta c -> d -> e
convert01100 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4)

convert01101 :: (WithMeta a -> b -> c -> WithMeta d -> e) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e
convert01101 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4)

convert0111x :: (WithMeta a -> b -> c -> d -> e) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e
convert0111x f x1 x2 x3 x4 = f (noMeta x1) (value x2) (value x3) (value x4)

convert01110 :: (WithMeta a -> b -> c -> d -> WithMeta e) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e
convert01110 f x1 x2 x3 x4 = value $ f (noMeta x1) (value x2) (value x3) (value x4)

convert01111 :: (WithMeta a -> b -> c -> d -> e) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e
convert01111 f x1 x2 x3 x4 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4)

convert1xxxx :: (a -> b -> c -> d -> e) -> WithMeta a -> b -> c -> d -> e
convert1xxxx f x1 x2 x3 x4 = f (value x1) x2 x3 x4

convert1xxx0 :: (a -> b -> c -> d -> WithMeta e) -> WithMeta a -> b -> c -> d -> e
convert1xxx0 f x1 x2 x3 x4 = value $ f (value x1) x2 x3 x4

convert1xxx1 :: (a -> b -> c -> d -> e) -> WithMeta a -> b -> c -> d -> WithMeta e
convert1xxx1 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 x3 x4

convert1xx0x :: (a -> b -> c -> WithMeta d -> e) -> WithMeta a -> b -> c -> d -> e
convert1xx0x f x1 x2 x3 x4 = f (value x1) x2 x3 (noMeta x4)

convert1xx00 :: (a -> b -> c -> WithMeta d -> WithMeta e) -> WithMeta a -> b -> c -> d -> e
convert1xx00 f x1 x2 x3 x4 = value $ f (value x1) x2 x3 (noMeta x4)

convert1xx01 :: (a -> b -> c -> WithMeta d -> e) -> WithMeta a -> b -> c -> d -> WithMeta e
convert1xx01 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 x3 (noMeta x4)

convert1xx1x :: (a -> b -> c -> d -> e) -> WithMeta a -> b -> c -> WithMeta d -> e
convert1xx1x f x1 x2 x3 x4 = f (value x1) x2 x3 (value x4)

convert1xx10 :: (a -> b -> c -> d -> WithMeta e) -> WithMeta a -> b -> c -> WithMeta d -> e
convert1xx10 f x1 x2 x3 x4 = value $ f (value x1) x2 x3 (value x4)

convert1xx11 :: (a -> b -> c -> d -> e) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e
convert1xx11 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 x3 (value x4)

convert1x0xx :: (a -> b -> WithMeta c -> d -> e) -> WithMeta a -> b -> c -> d -> e
convert1x0xx f x1 x2 x3 x4 = f (value x1) x2 (noMeta x3) x4

convert1x0x0 :: (a -> b -> WithMeta c -> d -> WithMeta e) -> WithMeta a -> b -> c -> d -> e
convert1x0x0 f x1 x2 x3 x4 = value $ f (value x1) x2 (noMeta x3) x4

convert1x0x1 :: (a -> b -> WithMeta c -> d -> e) -> WithMeta a -> b -> c -> d -> WithMeta e
convert1x0x1 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 (noMeta x3) x4

convert1x00x :: (a -> b -> WithMeta c -> WithMeta d -> e) -> WithMeta a -> b -> c -> d -> e
convert1x00x f x1 x2 x3 x4 = f (value x1) x2 (noMeta x3) (noMeta x4)

convert1x000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e) -> WithMeta a -> b -> c -> d -> e
convert1x000 f x1 x2 x3 x4 = value $ f (value x1) x2 (noMeta x3) (noMeta x4)

convert1x001 :: (a -> b -> WithMeta c -> WithMeta d -> e) -> WithMeta a -> b -> c -> d -> WithMeta e
convert1x001 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4)

convert1x01x :: (a -> b -> WithMeta c -> d -> e) -> WithMeta a -> b -> c -> WithMeta d -> e
convert1x01x f x1 x2 x3 x4 = f (value x1) x2 (noMeta x3) (value x4)

convert1x010 :: (a -> b -> WithMeta c -> d -> WithMeta e) -> WithMeta a -> b -> c -> WithMeta d -> e
convert1x010 f x1 x2 x3 x4 = value $ f (value x1) x2 (noMeta x3) (value x4)

convert1x011 :: (a -> b -> WithMeta c -> d -> e) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e
convert1x011 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 (noMeta x3) (value x4)

convert1x1xx :: (a -> b -> c -> d -> e) -> WithMeta a -> b -> WithMeta c -> d -> e
convert1x1xx f x1 x2 x3 x4 = f (value x1) x2 (value x3) x4

convert1x1x0 :: (a -> b -> c -> d -> WithMeta e) -> WithMeta a -> b -> WithMeta c -> d -> e
convert1x1x0 f x1 x2 x3 x4 = value $ f (value x1) x2 (value x3) x4

convert1x1x1 :: (a -> b -> c -> d -> e) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e
convert1x1x1 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 (value x3) x4

convert1x10x :: (a -> b -> c -> WithMeta d -> e) -> WithMeta a -> b -> WithMeta c -> d -> e
convert1x10x f x1 x2 x3 x4 = f (value x1) x2 (value x3) (noMeta x4)

convert1x100 :: (a -> b -> c -> WithMeta d -> WithMeta e) -> WithMeta a -> b -> WithMeta c -> d -> e
convert1x100 f x1 x2 x3 x4 = value $ f (value x1) x2 (value x3) (noMeta x4)

convert1x101 :: (a -> b -> c -> WithMeta d -> e) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e
convert1x101 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 (value x3) (noMeta x4)

convert1x11x :: (a -> b -> c -> d -> e) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e
convert1x11x f x1 x2 x3 x4 = f (value x1) x2 (value x3) (value x4)

convert1x110 :: (a -> b -> c -> d -> WithMeta e) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e
convert1x110 f x1 x2 x3 x4 = value $ f (value x1) x2 (value x3) (value x4)

convert1x111 :: (a -> b -> c -> d -> e) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e
convert1x111 f x1 x2 x3 x4 = noMeta $ f (value x1) x2 (value x3) (value x4)

convert10xxx :: (a -> WithMeta b -> c -> d -> e) -> WithMeta a -> b -> c -> d -> e
convert10xxx f x1 x2 x3 x4 = f (value x1) (noMeta x2) x3 x4

convert10xx0 :: (a -> WithMeta b -> c -> d -> WithMeta e) -> WithMeta a -> b -> c -> d -> e
convert10xx0 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) x3 x4

convert10xx1 :: (a -> WithMeta b -> c -> d -> e) -> WithMeta a -> b -> c -> d -> WithMeta e
convert10xx1 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) x3 x4

convert10x0x :: (a -> WithMeta b -> c -> WithMeta d -> e) -> WithMeta a -> b -> c -> d -> e
convert10x0x f x1 x2 x3 x4 = f (value x1) (noMeta x2) x3 (noMeta x4)

convert10x00 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e) -> WithMeta a -> b -> c -> d -> e
convert10x00 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) x3 (noMeta x4)

convert10x01 :: (a -> WithMeta b -> c -> WithMeta d -> e) -> WithMeta a -> b -> c -> d -> WithMeta e
convert10x01 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4)

convert10x1x :: (a -> WithMeta b -> c -> d -> e) -> WithMeta a -> b -> c -> WithMeta d -> e
convert10x1x f x1 x2 x3 x4 = f (value x1) (noMeta x2) x3 (value x4)

convert10x10 :: (a -> WithMeta b -> c -> d -> WithMeta e) -> WithMeta a -> b -> c -> WithMeta d -> e
convert10x10 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) x3 (value x4)

convert10x11 :: (a -> WithMeta b -> c -> d -> e) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e
convert10x11 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) x3 (value x4)

convert100xx :: (a -> WithMeta b -> WithMeta c -> d -> e) -> WithMeta a -> b -> c -> d -> e
convert100xx f x1 x2 x3 x4 = f (value x1) (noMeta x2) (noMeta x3) x4

convert100x0 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e) -> WithMeta a -> b -> c -> d -> e
convert100x0 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) (noMeta x3) x4

convert100x1 :: (a -> WithMeta b -> WithMeta c -> d -> e) -> WithMeta a -> b -> c -> d -> WithMeta e
convert100x1 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4

convert1000x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e) -> WithMeta a -> b -> c -> d -> e
convert1000x f x1 x2 x3 x4 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4)

convert10000 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e) -> WithMeta a -> b -> c -> d -> e
convert10000 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4)

convert10001 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e) -> WithMeta a -> b -> c -> d -> WithMeta e
convert10001 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4)

convert1001x :: (a -> WithMeta b -> WithMeta c -> d -> e) -> WithMeta a -> b -> c -> WithMeta d -> e
convert1001x f x1 x2 x3 x4 = f (value x1) (noMeta x2) (noMeta x3) (value x4)

convert10010 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e) -> WithMeta a -> b -> c -> WithMeta d -> e
convert10010 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4)

convert10011 :: (a -> WithMeta b -> WithMeta c -> d -> e) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e
convert10011 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4)

convert101xx :: (a -> WithMeta b -> c -> d -> e) -> WithMeta a -> b -> WithMeta c -> d -> e
convert101xx f x1 x2 x3 x4 = f (value x1) (noMeta x2) (value x3) x4

convert101x0 :: (a -> WithMeta b -> c -> d -> WithMeta e) -> WithMeta a -> b -> WithMeta c -> d -> e
convert101x0 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) (value x3) x4

convert101x1 :: (a -> WithMeta b -> c -> d -> e) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e
convert101x1 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) (value x3) x4

convert1010x :: (a -> WithMeta b -> c -> WithMeta d -> e) -> WithMeta a -> b -> WithMeta c -> d -> e
convert1010x f x1 x2 x3 x4 = f (value x1) (noMeta x2) (value x3) (noMeta x4)

convert10100 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e) -> WithMeta a -> b -> WithMeta c -> d -> e
convert10100 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4)

convert10101 :: (a -> WithMeta b -> c -> WithMeta d -> e) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e
convert10101 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4)

convert1011x :: (a -> WithMeta b -> c -> d -> e) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e
convert1011x f x1 x2 x3 x4 = f (value x1) (noMeta x2) (value x3) (value x4)

convert10110 :: (a -> WithMeta b -> c -> d -> WithMeta e) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e
convert10110 f x1 x2 x3 x4 = value $ f (value x1) (noMeta x2) (value x3) (value x4)

convert10111 :: (a -> WithMeta b -> c -> d -> e) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e
convert10111 f x1 x2 x3 x4 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4)

convert11xxx :: (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> c -> d -> e
convert11xxx f x1 x2 x3 x4 = f (value x1) (value x2) x3 x4

convert11xx0 :: (a -> b -> c -> d -> WithMeta e) -> WithMeta a -> WithMeta b -> c -> d -> e
convert11xx0 f x1 x2 x3 x4 = value $ f (value x1) (value x2) x3 x4

convert11xx1 :: (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e
convert11xx1 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) x3 x4

convert11x0x :: (a -> b -> c -> WithMeta d -> e) -> WithMeta a -> WithMeta b -> c -> d -> e
convert11x0x f x1 x2 x3 x4 = f (value x1) (value x2) x3 (noMeta x4)

convert11x00 :: (a -> b -> c -> WithMeta d -> WithMeta e) -> WithMeta a -> WithMeta b -> c -> d -> e
convert11x00 f x1 x2 x3 x4 = value $ f (value x1) (value x2) x3 (noMeta x4)

convert11x01 :: (a -> b -> c -> WithMeta d -> e) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e
convert11x01 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) x3 (noMeta x4)

convert11x1x :: (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e
convert11x1x f x1 x2 x3 x4 = f (value x1) (value x2) x3 (value x4)

convert11x10 :: (a -> b -> c -> d -> WithMeta e) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e
convert11x10 f x1 x2 x3 x4 = value $ f (value x1) (value x2) x3 (value x4)

convert11x11 :: (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e
convert11x11 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) x3 (value x4)

convert110xx :: (a -> b -> WithMeta c -> d -> e) -> WithMeta a -> WithMeta b -> c -> d -> e
convert110xx f x1 x2 x3 x4 = f (value x1) (value x2) (noMeta x3) x4

convert110x0 :: (a -> b -> WithMeta c -> d -> WithMeta e) -> WithMeta a -> WithMeta b -> c -> d -> e
convert110x0 f x1 x2 x3 x4 = value $ f (value x1) (value x2) (noMeta x3) x4

convert110x1 :: (a -> b -> WithMeta c -> d -> e) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e
convert110x1 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) (noMeta x3) x4

convert1100x :: (a -> b -> WithMeta c -> WithMeta d -> e) -> WithMeta a -> WithMeta b -> c -> d -> e
convert1100x f x1 x2 x3 x4 = f (value x1) (value x2) (noMeta x3) (noMeta x4)

convert11000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e) -> WithMeta a -> WithMeta b -> c -> d -> e
convert11000 f x1 x2 x3 x4 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4)

convert11001 :: (a -> b -> WithMeta c -> WithMeta d -> e) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e
convert11001 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4)

convert1101x :: (a -> b -> WithMeta c -> d -> e) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e
convert1101x f x1 x2 x3 x4 = f (value x1) (value x2) (noMeta x3) (value x4)

convert11010 :: (a -> b -> WithMeta c -> d -> WithMeta e) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e
convert11010 f x1 x2 x3 x4 = value $ f (value x1) (value x2) (noMeta x3) (value x4)

convert11011 :: (a -> b -> WithMeta c -> d -> e) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e
convert11011 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4)

convert111xx :: (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e
convert111xx f x1 x2 x3 x4 = f (value x1) (value x2) (value x3) x4

convert111x0 :: (a -> b -> c -> d -> WithMeta e) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e
convert111x0 f x1 x2 x3 x4 = value $ f (value x1) (value x2) (value x3) x4

convert111x1 :: (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e
convert111x1 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) (value x3) x4

convert1110x :: (a -> b -> c -> WithMeta d -> e) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e
convert1110x f x1 x2 x3 x4 = f (value x1) (value x2) (value x3) (noMeta x4)

convert11100 :: (a -> b -> c -> WithMeta d -> WithMeta e) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e
convert11100 f x1 x2 x3 x4 = value $ f (value x1) (value x2) (value x3) (noMeta x4)

convert11101 :: (a -> b -> c -> WithMeta d -> e) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e
convert11101 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4)

convert1111x :: (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e
convert1111x f x1 x2 x3 x4 = f (value x1) (value x2) (value x3) (value x4)

convert11110 :: (a -> b -> c -> d -> WithMeta e) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e
convert11110 f x1 x2 x3 x4 = value $ f (value x1) (value x2) (value x3) (value x4)

convert11111 :: (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e
convert11111 f x1 x2 x3 x4 = noMeta $ f (value x1) (value x2) (value x3) (value x4)

convertxxxxx0 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertxxxxx0 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 x4 x5

convertxxxxx1 :: (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertxxxxx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 x4 x5

convertxxxx0x :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convertxxxx0x f x1 x2 x3 x4 x5 = f x1 x2 x3 x4 (noMeta x5)

convertxxxx00 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertxxxx00 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 x4 (noMeta x5)

convertxxxx01 :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertxxxx01 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 x4 (noMeta x5)

convertxxxx1x :: (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convertxxxx1x f x1 x2 x3 x4 x5 = f x1 x2 x3 x4 (value x5)

convertxxxx10 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convertxxxx10 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 x4 (value x5)

convertxxxx11 :: (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convertxxxx11 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 x4 (value x5)

convertxxx0xx :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> f
convertxxx0xx f x1 x2 x3 x4 x5 = f x1 x2 x3 (noMeta x4) x5

convertxxx0x0 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertxxx0x0 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 (noMeta x4) x5

convertxxx0x1 :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertxxx0x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 (noMeta x4) x5

convertxxx00x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convertxxx00x f x1 x2 x3 x4 x5 = f x1 x2 x3 (noMeta x4) (noMeta x5)

convertxxx000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertxxx000 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 (noMeta x4) (noMeta x5)

convertxxx001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertxxx001 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 (noMeta x4) (noMeta x5)

convertxxx01x :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convertxxx01x f x1 x2 x3 x4 x5 = f x1 x2 x3 (noMeta x4) (value x5)

convertxxx010 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convertxxx010 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 (noMeta x4) (value x5)

convertxxx011 :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convertxxx011 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 (noMeta x4) (value x5)

convertxxx1xx :: (a -> b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convertxxx1xx f x1 x2 x3 x4 x5 = f x1 x2 x3 (value x4) x5

convertxxx1x0 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convertxxx1x0 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 (value x4) x5

convertxxx1x1 :: (a -> b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convertxxx1x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 (value x4) x5

convertxxx10x :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convertxxx10x f x1 x2 x3 x4 x5 = f x1 x2 x3 (value x4) (noMeta x5)

convertxxx100 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convertxxx100 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 (value x4) (noMeta x5)

convertxxx101 :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convertxxx101 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 (value x4) (noMeta x5)

convertxxx11x :: (a -> b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convertxxx11x f x1 x2 x3 x4 x5 = f x1 x2 x3 (value x4) (value x5)

convertxxx110 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convertxxx110 f x1 x2 x3 x4 x5 = value $ f x1 x2 x3 (value x4) (value x5)

convertxxx111 :: (a -> b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convertxxx111 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 x3 (value x4) (value x5)

convertxx0xxx :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> e -> f
convertxx0xxx f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) x4 x5

convertxx0xx0 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertxx0xx0 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) x4 x5

convertxx0xx1 :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertxx0xx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) x4 x5

convertxx0x0x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convertxx0x0x f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) x4 (noMeta x5)

convertxx0x00 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertxx0x00 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) x4 (noMeta x5)

convertxx0x01 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertxx0x01 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) x4 (noMeta x5)

convertxx0x1x :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convertxx0x1x f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) x4 (value x5)

convertxx0x10 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convertxx0x10 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) x4 (value x5)

convertxx0x11 :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convertxx0x11 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) x4 (value x5)

convertxx00xx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> f
convertxx00xx f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) (noMeta x4) x5

convertxx00x0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertxx00x0 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) (noMeta x4) x5

convertxx00x1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertxx00x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) x5

convertxx000x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convertxx000x f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5)

convertxx0000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertxx0000 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5)

convertxx0001 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertxx0001 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5)

convertxx001x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convertxx001x f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) (noMeta x4) (value x5)

convertxx0010 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convertxx0010 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) (noMeta x4) (value x5)

convertxx0011 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convertxx0011 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) (value x5)

convertxx01xx :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convertxx01xx f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) (value x4) x5

convertxx01x0 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convertxx01x0 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) (value x4) x5

convertxx01x1 :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convertxx01x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) (value x4) x5

convertxx010x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convertxx010x f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) (value x4) (noMeta x5)

convertxx0100 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convertxx0100 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) (value x4) (noMeta x5)

convertxx0101 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convertxx0101 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) (value x4) (noMeta x5)

convertxx011x :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convertxx011x f x1 x2 x3 x4 x5 = f x1 x2 (noMeta x3) (value x4) (value x5)

convertxx0110 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convertxx0110 f x1 x2 x3 x4 x5 = value $ f x1 x2 (noMeta x3) (value x4) (value x5)

convertxx0111 :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convertxx0111 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (noMeta x3) (value x4) (value x5)

convertxx1xxx :: (a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convertxx1xxx f x1 x2 x3 x4 x5 = f x1 x2 (value x3) x4 x5

convertxx1xx0 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convertxx1xx0 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) x4 x5

convertxx1xx1 :: (a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convertxx1xx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) x4 x5

convertxx1x0x :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convertxx1x0x f x1 x2 x3 x4 x5 = f x1 x2 (value x3) x4 (noMeta x5)

convertxx1x00 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convertxx1x00 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) x4 (noMeta x5)

convertxx1x01 :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convertxx1x01 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) x4 (noMeta x5)

convertxx1x1x :: (a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convertxx1x1x f x1 x2 x3 x4 x5 = f x1 x2 (value x3) x4 (value x5)

convertxx1x10 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convertxx1x10 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) x4 (value x5)

convertxx1x11 :: (a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convertxx1x11 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) x4 (value x5)

convertxx10xx :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convertxx10xx f x1 x2 x3 x4 x5 = f x1 x2 (value x3) (noMeta x4) x5

convertxx10x0 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convertxx10x0 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) (noMeta x4) x5

convertxx10x1 :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convertxx10x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) (noMeta x4) x5

convertxx100x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convertxx100x f x1 x2 x3 x4 x5 = f x1 x2 (value x3) (noMeta x4) (noMeta x5)

convertxx1000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convertxx1000 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) (noMeta x4) (noMeta x5)

convertxx1001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convertxx1001 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) (noMeta x4) (noMeta x5)

convertxx101x :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convertxx101x f x1 x2 x3 x4 x5 = f x1 x2 (value x3) (noMeta x4) (value x5)

convertxx1010 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convertxx1010 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) (noMeta x4) (value x5)

convertxx1011 :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convertxx1011 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) (noMeta x4) (value x5)

convertxx11xx :: (a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convertxx11xx f x1 x2 x3 x4 x5 = f x1 x2 (value x3) (value x4) x5

convertxx11x0 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convertxx11x0 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) (value x4) x5

convertxx11x1 :: (a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convertxx11x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) (value x4) x5

convertxx110x :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convertxx110x f x1 x2 x3 x4 x5 = f x1 x2 (value x3) (value x4) (noMeta x5)

convertxx1100 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convertxx1100 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) (value x4) (noMeta x5)

convertxx1101 :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convertxx1101 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) (value x4) (noMeta x5)

convertxx111x :: (a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convertxx111x f x1 x2 x3 x4 x5 = f x1 x2 (value x3) (value x4) (value x5)

convertxx1110 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convertxx1110 f x1 x2 x3 x4 x5 = value $ f x1 x2 (value x3) (value x4) (value x5)

convertxx1111 :: (a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convertxx1111 f x1 x2 x3 x4 x5 = noMeta $ f x1 x2 (value x3) (value x4) (value x5)

convertx0xxxx :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> f
convertx0xxxx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 x4 x5

convertx0xxx0 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertx0xxx0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 x4 x5

convertx0xxx1 :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertx0xxx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 x4 x5

convertx0xx0x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convertx0xx0x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 x4 (noMeta x5)

convertx0xx00 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertx0xx00 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 x4 (noMeta x5)

convertx0xx01 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertx0xx01 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 x4 (noMeta x5)

convertx0xx1x :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convertx0xx1x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 x4 (value x5)

convertx0xx10 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convertx0xx10 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 x4 (value x5)

convertx0xx11 :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convertx0xx11 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 x4 (value x5)

convertx0x0xx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> f
convertx0x0xx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 (noMeta x4) x5

convertx0x0x0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertx0x0x0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 (noMeta x4) x5

convertx0x0x1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertx0x0x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) x5

convertx0x00x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convertx0x00x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5)

convertx0x000 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertx0x000 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5)

convertx0x001 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertx0x001 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5)

convertx0x01x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convertx0x01x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 (noMeta x4) (value x5)

convertx0x010 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convertx0x010 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 (noMeta x4) (value x5)

convertx0x011 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convertx0x011 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) (value x5)

convertx0x1xx :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convertx0x1xx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 (value x4) x5

convertx0x1x0 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convertx0x1x0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 (value x4) x5

convertx0x1x1 :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convertx0x1x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 (value x4) x5

convertx0x10x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convertx0x10x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 (value x4) (noMeta x5)

convertx0x100 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convertx0x100 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 (value x4) (noMeta x5)

convertx0x101 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convertx0x101 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 (value x4) (noMeta x5)

convertx0x11x :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convertx0x11x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) x3 (value x4) (value x5)

convertx0x110 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convertx0x110 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) x3 (value x4) (value x5)

convertx0x111 :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convertx0x111 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) x3 (value x4) (value x5)

convertx00xxx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> e -> f
convertx00xxx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) x4 x5

convertx00xx0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertx00xx0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) x4 x5

convertx00xx1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertx00xx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 x5

convertx00x0x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convertx00x0x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5)

convertx00x00 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertx00x00 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5)

convertx00x01 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertx00x01 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5)

convertx00x1x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convertx00x1x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) x4 (value x5)

convertx00x10 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convertx00x10 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) x4 (value x5)

convertx00x11 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convertx00x11 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 (value x5)

convertx000xx :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> f
convertx000xx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5

convertx000x0 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertx000x0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5

convertx000x1 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertx000x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5

convertx0000x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convertx0000x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convertx00000 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convertx00000 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convertx00001 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convertx00001 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convertx0001x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convertx0001x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convertx00010 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convertx00010 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convertx00011 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convertx00011 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convertx001xx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convertx001xx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) (value x4) x5

convertx001x0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convertx001x0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) x5

convertx001x1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convertx001x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) x5

convertx0010x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convertx0010x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convertx00100 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convertx00100 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convertx00101 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convertx00101 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convertx0011x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convertx0011x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (noMeta x3) (value x4) (value x5)

convertx00110 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convertx00110 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) (value x5)

convertx00111 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convertx00111 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) (value x5)

convertx01xxx :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convertx01xxx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) x4 x5

convertx01xx0 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convertx01xx0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) x4 x5

convertx01xx1 :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convertx01xx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) x4 x5

convertx01x0x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convertx01x0x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) x4 (noMeta x5)

convertx01x00 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convertx01x00 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) x4 (noMeta x5)

convertx01x01 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convertx01x01 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) x4 (noMeta x5)

convertx01x1x :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convertx01x1x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) x4 (value x5)

convertx01x10 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convertx01x10 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) x4 (value x5)

convertx01x11 :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convertx01x11 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) x4 (value x5)

convertx010xx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convertx010xx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) (noMeta x4) x5

convertx010x0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convertx010x0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) x5

convertx010x1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convertx010x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) x5

convertx0100x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convertx0100x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convertx01000 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convertx01000 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convertx01001 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convertx01001 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convertx0101x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convertx0101x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) (noMeta x4) (value x5)

convertx01010 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convertx01010 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) (value x5)

convertx01011 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convertx01011 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) (value x5)

convertx011xx :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convertx011xx f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) (value x4) x5

convertx011x0 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convertx011x0 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) (value x4) x5

convertx011x1 :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convertx011x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) x5

convertx0110x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convertx0110x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) (value x4) (noMeta x5)

convertx01100 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convertx01100 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) (value x4) (noMeta x5)

convertx01101 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convertx01101 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) (noMeta x5)

convertx0111x :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convertx0111x f x1 x2 x3 x4 x5 = f x1 (noMeta x2) (value x3) (value x4) (value x5)

convertx01110 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convertx01110 f x1 x2 x3 x4 x5 = value $ f x1 (noMeta x2) (value x3) (value x4) (value x5)

convertx01111 :: (a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convertx01111 f x1 x2 x3 x4 x5 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) (value x5)

convertx1xxxx :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1xxxx f x1 x2 x3 x4 x5 = f x1 (value x2) x3 x4 x5

convertx1xxx0 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1xxx0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 x4 x5

convertx1xxx1 :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convertx1xxx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 x4 x5

convertx1xx0x :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1xx0x f x1 x2 x3 x4 x5 = f x1 (value x2) x3 x4 (noMeta x5)

convertx1xx00 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1xx00 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 x4 (noMeta x5)

convertx1xx01 :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convertx1xx01 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 x4 (noMeta x5)

convertx1xx1x :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convertx1xx1x f x1 x2 x3 x4 x5 = f x1 (value x2) x3 x4 (value x5)

convertx1xx10 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convertx1xx10 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 x4 (value x5)

convertx1xx11 :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convertx1xx11 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 x4 (value x5)

convertx1x0xx :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1x0xx f x1 x2 x3 x4 x5 = f x1 (value x2) x3 (noMeta x4) x5

convertx1x0x0 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1x0x0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 (noMeta x4) x5

convertx1x0x1 :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convertx1x0x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 (noMeta x4) x5

convertx1x00x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1x00x f x1 x2 x3 x4 x5 = f x1 (value x2) x3 (noMeta x4) (noMeta x5)

convertx1x000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1x000 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 (noMeta x4) (noMeta x5)

convertx1x001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convertx1x001 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 (noMeta x4) (noMeta x5)

convertx1x01x :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convertx1x01x f x1 x2 x3 x4 x5 = f x1 (value x2) x3 (noMeta x4) (value x5)

convertx1x010 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convertx1x010 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 (noMeta x4) (value x5)

convertx1x011 :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convertx1x011 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 (noMeta x4) (value x5)

convertx1x1xx :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convertx1x1xx f x1 x2 x3 x4 x5 = f x1 (value x2) x3 (value x4) x5

convertx1x1x0 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convertx1x1x0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 (value x4) x5

convertx1x1x1 :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convertx1x1x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 (value x4) x5

convertx1x10x :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convertx1x10x f x1 x2 x3 x4 x5 = f x1 (value x2) x3 (value x4) (noMeta x5)

convertx1x100 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convertx1x100 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 (value x4) (noMeta x5)

convertx1x101 :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convertx1x101 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 (value x4) (noMeta x5)

convertx1x11x :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convertx1x11x f x1 x2 x3 x4 x5 = f x1 (value x2) x3 (value x4) (value x5)

convertx1x110 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convertx1x110 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) x3 (value x4) (value x5)

convertx1x111 :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convertx1x111 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) x3 (value x4) (value x5)

convertx10xxx :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convertx10xxx f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) x4 x5

convertx10xx0 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convertx10xx0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) x4 x5

convertx10xx1 :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convertx10xx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) x4 x5

convertx10x0x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convertx10x0x f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) x4 (noMeta x5)

convertx10x00 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convertx10x00 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) x4 (noMeta x5)

convertx10x01 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convertx10x01 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) x4 (noMeta x5)

convertx10x1x :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convertx10x1x f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) x4 (value x5)

convertx10x10 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convertx10x10 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) x4 (value x5)

convertx10x11 :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convertx10x11 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) x4 (value x5)

convertx100xx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convertx100xx f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) (noMeta x4) x5

convertx100x0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convertx100x0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) x5

convertx100x1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convertx100x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) x5

convertx1000x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convertx1000x f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convertx10000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convertx10000 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convertx10001 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convertx10001 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convertx1001x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convertx1001x f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) (noMeta x4) (value x5)

convertx10010 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convertx10010 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) (value x5)

convertx10011 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convertx10011 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) (value x5)

convertx101xx :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convertx101xx f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) (value x4) x5

convertx101x0 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convertx101x0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) (value x4) x5

convertx101x1 :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convertx101x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) x5

convertx1010x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convertx1010x f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) (value x4) (noMeta x5)

convertx10100 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convertx10100 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) (value x4) (noMeta x5)

convertx10101 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convertx10101 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) (noMeta x5)

convertx1011x :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convertx1011x f x1 x2 x3 x4 x5 = f x1 (value x2) (noMeta x3) (value x4) (value x5)

convertx10110 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convertx10110 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (noMeta x3) (value x4) (value x5)

convertx10111 :: (a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convertx10111 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) (value x5)

convertx11xxx :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convertx11xxx f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) x4 x5

convertx11xx0 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convertx11xx0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) x4 x5

convertx11xx1 :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convertx11xx1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) x4 x5

convertx11x0x :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convertx11x0x f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) x4 (noMeta x5)

convertx11x00 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convertx11x00 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) x4 (noMeta x5)

convertx11x01 :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convertx11x01 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) x4 (noMeta x5)

convertx11x1x :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convertx11x1x f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) x4 (value x5)

convertx11x10 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convertx11x10 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) x4 (value x5)

convertx11x11 :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convertx11x11 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) x4 (value x5)

convertx110xx :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convertx110xx f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) (noMeta x4) x5

convertx110x0 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convertx110x0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) (noMeta x4) x5

convertx110x1 :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convertx110x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) x5

convertx1100x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convertx1100x f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) (noMeta x4) (noMeta x5)

convertx11000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convertx11000 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) (noMeta x4) (noMeta x5)

convertx11001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convertx11001 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) (noMeta x5)

convertx1101x :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convertx1101x f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) (noMeta x4) (value x5)

convertx11010 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convertx11010 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) (noMeta x4) (value x5)

convertx11011 :: (a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convertx11011 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) (value x5)

convertx111xx :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convertx111xx f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) (value x4) x5

convertx111x0 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convertx111x0 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) (value x4) x5

convertx111x1 :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convertx111x1 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) (value x4) x5

convertx1110x :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convertx1110x f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) (value x4) (noMeta x5)

convertx11100 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convertx11100 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) (value x4) (noMeta x5)

convertx11101 :: (a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convertx11101 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) (value x4) (noMeta x5)

convertx1111x :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convertx1111x f x1 x2 x3 x4 x5 = f x1 (value x2) (value x3) (value x4) (value x5)

convertx11110 :: (a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convertx11110 f x1 x2 x3 x4 x5 = value $ f x1 (value x2) (value x3) (value x4) (value x5)

convertx11111 :: (a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convertx11111 f x1 x2 x3 x4 x5 = noMeta $ f x1 (value x2) (value x3) (value x4) (value x5)

convert0xxxxx :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> f
convert0xxxxx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 x4 x5

convert0xxxx0 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0xxxx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 x4 x5

convert0xxxx1 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0xxxx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 x4 x5

convert0xxx0x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convert0xxx0x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 x4 (noMeta x5)

convert0xxx00 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0xxx00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 x4 (noMeta x5)

convert0xxx01 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0xxx01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 x4 (noMeta x5)

convert0xxx1x :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convert0xxx1x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 x4 (value x5)

convert0xxx10 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convert0xxx10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 x4 (value x5)

convert0xxx11 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convert0xxx11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 x4 (value x5)

convert0xx0xx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> f
convert0xx0xx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 (noMeta x4) x5

convert0xx0x0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0xx0x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 (noMeta x4) x5

convert0xx0x1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0xx0x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) x5

convert0xx00x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convert0xx00x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5)

convert0xx000 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0xx000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5)

convert0xx001 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0xx001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5)

convert0xx01x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convert0xx01x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 (noMeta x4) (value x5)

convert0xx010 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convert0xx010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 (noMeta x4) (value x5)

convert0xx011 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convert0xx011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) (value x5)

convert0xx1xx :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convert0xx1xx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 (value x4) x5

convert0xx1x0 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convert0xx1x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 (value x4) x5

convert0xx1x1 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convert0xx1x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 (value x4) x5

convert0xx10x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convert0xx10x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 (value x4) (noMeta x5)

convert0xx100 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convert0xx100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 (value x4) (noMeta x5)

convert0xx101 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convert0xx101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 (value x4) (noMeta x5)

convert0xx11x :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convert0xx11x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 x3 (value x4) (value x5)

convert0xx110 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convert0xx110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 x3 (value x4) (value x5)

convert0xx111 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert0xx111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 x3 (value x4) (value x5)

convert0x0xxx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> e -> f
convert0x0xxx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) x4 x5

convert0x0xx0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0x0xx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) x4 x5

convert0x0xx1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0x0xx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 x5

convert0x0x0x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convert0x0x0x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5)

convert0x0x00 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0x0x00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5)

convert0x0x01 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0x0x01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5)

convert0x0x1x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convert0x0x1x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) x4 (value x5)

convert0x0x10 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convert0x0x10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) x4 (value x5)

convert0x0x11 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convert0x0x11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 (value x5)

convert0x00xx :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> f
convert0x00xx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5

convert0x00x0 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0x00x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5

convert0x00x1 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0x00x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5

convert0x000x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convert0x000x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5)

convert0x0000 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0x0000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5)

convert0x0001 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0x0001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5)

convert0x001x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convert0x001x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5)

convert0x0010 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convert0x0010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5)

convert0x0011 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convert0x0011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5)

convert0x01xx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convert0x01xx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) (value x4) x5

convert0x01x0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convert0x01x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) x5

convert0x01x1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convert0x01x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) x5

convert0x010x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convert0x010x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5)

convert0x0100 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convert0x0100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5)

convert0x0101 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convert0x0101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5)

convert0x011x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convert0x011x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (noMeta x3) (value x4) (value x5)

convert0x0110 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convert0x0110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) (value x5)

convert0x0111 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert0x0111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) (value x5)

convert0x1xxx :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convert0x1xxx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) x4 x5

convert0x1xx0 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convert0x1xx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) x4 x5

convert0x1xx1 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convert0x1xx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) x4 x5

convert0x1x0x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convert0x1x0x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) x4 (noMeta x5)

convert0x1x00 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convert0x1x00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) x4 (noMeta x5)

convert0x1x01 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convert0x1x01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) x4 (noMeta x5)

convert0x1x1x :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convert0x1x1x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) x4 (value x5)

convert0x1x10 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convert0x1x10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) x4 (value x5)

convert0x1x11 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert0x1x11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) x4 (value x5)

convert0x10xx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convert0x10xx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) (noMeta x4) x5

convert0x10x0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convert0x10x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) x5

convert0x10x1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convert0x10x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) x5

convert0x100x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convert0x100x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5)

convert0x1000 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convert0x1000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5)

convert0x1001 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convert0x1001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5)

convert0x101x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convert0x101x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) (noMeta x4) (value x5)

convert0x1010 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convert0x1010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) (value x5)

convert0x1011 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert0x1011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) (value x5)

convert0x11xx :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convert0x11xx f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) (value x4) x5

convert0x11x0 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convert0x11x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) (value x4) x5

convert0x11x1 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert0x11x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) x5

convert0x110x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convert0x110x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) (value x4) (noMeta x5)

convert0x1100 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convert0x1100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) (value x4) (noMeta x5)

convert0x1101 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert0x1101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) (noMeta x5)

convert0x111x :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert0x111x f x1 x2 x3 x4 x5 = f (noMeta x1) x2 (value x3) (value x4) (value x5)

convert0x1110 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert0x1110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) x2 (value x3) (value x4) (value x5)

convert0x1111 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convert0x1111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) (value x5)

convert00xxxx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> f
convert00xxxx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 x4 x5

convert00xxx0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert00xxx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 x4 x5

convert00xxx1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert00xxx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 x5

convert00xx0x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convert00xx0x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5)

convert00xx00 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert00xx00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5)

convert00xx01 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert00xx01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5)

convert00xx1x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convert00xx1x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 x4 (value x5)

convert00xx10 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convert00xx10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 x4 (value x5)

convert00xx11 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convert00xx11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 (value x5)

convert00x0xx :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> f
convert00x0xx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5

convert00x0x0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert00x0x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5

convert00x0x1 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert00x0x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5

convert00x00x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convert00x00x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5)

convert00x000 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert00x000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5)

convert00x001 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert00x001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5)

convert00x01x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convert00x01x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5)

convert00x010 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convert00x010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5)

convert00x011 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convert00x011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5)

convert00x1xx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convert00x1xx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 (value x4) x5

convert00x1x0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convert00x1x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) x5

convert00x1x1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convert00x1x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) x5

convert00x10x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convert00x10x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5)

convert00x100 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convert00x100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5)

convert00x101 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convert00x101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5)

convert00x11x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convert00x11x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) x3 (value x4) (value x5)

convert00x110 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convert00x110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) (value x5)

convert00x111 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert00x111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) (value x5)

convert000xxx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> e -> f
convert000xxx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5

convert000xx0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert000xx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5

convert000xx1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert000xx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5

convert000x0x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convert000x0x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5)

convert000x00 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert000x00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5)

convert000x01 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert000x01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5)

convert000x1x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convert000x1x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5)

convert000x10 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convert000x10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5)

convert000x11 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convert000x11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5)

convert0000xx :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> f
convert0000xx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5

convert0000x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert0000x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5

convert0000x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert0000x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5

convert00000x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> f
convert00000x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert000000 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> c -> d -> e -> f
convert000000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert000001 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> b -> c -> d -> e -> WithMeta f
convert000001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert00001x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> f
convert00001x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convert000010 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> b -> c -> d -> WithMeta e -> f
convert000010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convert000011 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> a -> b -> c -> d -> WithMeta e -> WithMeta f
convert000011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convert0001xx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convert0001xx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5

convert0001x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convert0001x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5

convert0001x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convert0001x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5

convert00010x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> f
convert00010x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convert000100 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> b -> c -> WithMeta d -> e -> f
convert000100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convert000101 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> a -> b -> c -> WithMeta d -> e -> WithMeta f
convert000101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convert00011x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convert00011x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5)

convert000110 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> a -> b -> c -> WithMeta d -> WithMeta e -> f
convert000110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5)

convert000111 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert000111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5)

convert001xxx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convert001xxx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) x4 x5

convert001xx0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convert001xx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 x5

convert001xx1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convert001xx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 x5

convert001x0x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convert001x0x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5)

convert001x00 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convert001x00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5)

convert001x01 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convert001x01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5)

convert001x1x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convert001x1x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) x4 (value x5)

convert001x10 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convert001x10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 (value x5)

convert001x11 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert001x11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 (value x5)

convert0010xx :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convert0010xx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5

convert0010x0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convert0010x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5

convert0010x1 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convert0010x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5

convert00100x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> f
convert00100x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convert001000 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> d -> e -> f
convert001000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convert001001 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> a -> b -> WithMeta c -> d -> e -> WithMeta f
convert001001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convert00101x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convert00101x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5)

convert001010 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> a -> b -> WithMeta c -> d -> WithMeta e -> f
convert001010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5)

convert001011 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert001011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5)

convert0011xx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convert0011xx f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) (value x4) x5

convert0011x0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convert0011x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) x5

convert0011x1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert0011x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) x5

convert00110x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convert00110x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5)

convert001100 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> e -> f
convert001100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5)

convert001101 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert001101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5)

convert00111x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert00111x f x1 x2 x3 x4 x5 = f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5)

convert001110 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert001110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5)

convert001111 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convert001111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5)

convert01xxxx :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convert01xxxx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 x4 x5

convert01xxx0 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convert01xxx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 x4 x5

convert01xxx1 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convert01xxx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 x4 x5

convert01xx0x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convert01xx0x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 x4 (noMeta x5)

convert01xx00 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convert01xx00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 x4 (noMeta x5)

convert01xx01 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convert01xx01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 x4 (noMeta x5)

convert01xx1x :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convert01xx1x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 x4 (value x5)

convert01xx10 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convert01xx10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 x4 (value x5)

convert01xx11 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convert01xx11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 x4 (value x5)

convert01x0xx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convert01x0xx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 (noMeta x4) x5

convert01x0x0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convert01x0x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) x5

convert01x0x1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convert01x0x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) x5

convert01x00x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convert01x00x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5)

convert01x000 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convert01x000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5)

convert01x001 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convert01x001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5)

convert01x01x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convert01x01x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 (noMeta x4) (value x5)

convert01x010 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convert01x010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) (value x5)

convert01x011 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convert01x011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) (value x5)

convert01x1xx :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convert01x1xx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 (value x4) x5

convert01x1x0 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convert01x1x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 (value x4) x5

convert01x1x1 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convert01x1x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) x5

convert01x10x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convert01x10x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 (value x4) (noMeta x5)

convert01x100 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convert01x100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 (value x4) (noMeta x5)

convert01x101 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convert01x101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) (noMeta x5)

convert01x11x :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convert01x11x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) x3 (value x4) (value x5)

convert01x110 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convert01x110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) x3 (value x4) (value x5)

convert01x111 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert01x111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) (value x5)

convert010xxx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convert010xxx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) x4 x5

convert010xx0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convert010xx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 x5

convert010xx1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convert010xx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 x5

convert010x0x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convert010x0x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5)

convert010x00 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convert010x00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5)

convert010x01 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convert010x01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5)

convert010x1x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convert010x1x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) x4 (value x5)

convert010x10 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convert010x10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 (value x5)

convert010x11 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convert010x11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 (value x5)

convert0100xx :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convert0100xx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5

convert0100x0 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convert0100x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5

convert0100x1 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convert0100x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5

convert01000x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> f
convert01000x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert010000 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> d -> e -> f
convert010000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert010001 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> c -> d -> e -> WithMeta f
convert010001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert01001x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convert01001x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5)

convert010010 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> c -> d -> WithMeta e -> f
convert010010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5)

convert010011 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convert010011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5)

convert0101xx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convert0101xx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) (value x4) x5

convert0101x0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convert0101x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) x5

convert0101x1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convert0101x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) x5

convert01010x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convert01010x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5)

convert010100 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> e -> f
convert010100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5)

convert010101 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convert010101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5)

convert01011x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convert01011x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5)

convert010110 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convert010110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5)

convert010111 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert010111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5)

convert011xxx :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convert011xxx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) x4 x5

convert011xx0 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convert011xx0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) x4 x5

convert011xx1 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convert011xx1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 x5

convert011x0x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convert011x0x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) x4 (noMeta x5)

convert011x00 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convert011x00 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) x4 (noMeta x5)

convert011x01 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convert011x01 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 (noMeta x5)

convert011x1x :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convert011x1x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) x4 (value x5)

convert011x10 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convert011x10 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) x4 (value x5)

convert011x11 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert011x11 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 (value x5)

convert0110xx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convert0110xx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) (noMeta x4) x5

convert0110x0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convert0110x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) x5

convert0110x1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convert0110x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) x5

convert01100x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convert01100x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5)

convert011000 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> e -> f
convert011000 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5)

convert011001 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convert011001 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5)

convert01101x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convert01101x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5)

convert011010 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convert011010 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5)

convert011011 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert011011 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5)

convert0111xx :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convert0111xx f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) (value x4) x5

convert0111x0 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convert0111x0 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) (value x4) x5

convert0111x1 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert0111x1 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) x5

convert01110x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convert01110x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5)

convert011100 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convert011100 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5)

convert011101 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert011101 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5)

convert01111x :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert01111x f x1 x2 x3 x4 x5 = f (noMeta x1) (value x2) (value x3) (value x4) (value x5)

convert011110 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert011110 f x1 x2 x3 x4 x5 = value $ f (noMeta x1) (value x2) (value x3) (value x4) (value x5)

convert011111 :: (WithMeta a -> b -> c -> d -> e -> f) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convert011111 f x1 x2 x3 x4 x5 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) (value x5)

convert1xxxxx :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1xxxxx f x1 x2 x3 x4 x5 = f (value x1) x2 x3 x4 x5

convert1xxxx0 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1xxxx0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 x4 x5

convert1xxxx1 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1xxxx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 x4 x5

convert1xxx0x :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1xxx0x f x1 x2 x3 x4 x5 = f (value x1) x2 x3 x4 (noMeta x5)

convert1xxx00 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1xxx00 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 x4 (noMeta x5)

convert1xxx01 :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1xxx01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 x4 (noMeta x5)

convert1xxx1x :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert1xxx1x f x1 x2 x3 x4 x5 = f (value x1) x2 x3 x4 (value x5)

convert1xxx10 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert1xxx10 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 x4 (value x5)

convert1xxx11 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f
convert1xxx11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 x4 (value x5)

convert1xx0xx :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1xx0xx f x1 x2 x3 x4 x5 = f (value x1) x2 x3 (noMeta x4) x5

convert1xx0x0 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1xx0x0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 (noMeta x4) x5

convert1xx0x1 :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1xx0x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 (noMeta x4) x5

convert1xx00x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1xx00x f x1 x2 x3 x4 x5 = f (value x1) x2 x3 (noMeta x4) (noMeta x5)

convert1xx000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1xx000 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 (noMeta x4) (noMeta x5)

convert1xx001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1xx001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 (noMeta x4) (noMeta x5)

convert1xx01x :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert1xx01x f x1 x2 x3 x4 x5 = f (value x1) x2 x3 (noMeta x4) (value x5)

convert1xx010 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert1xx010 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 (noMeta x4) (value x5)

convert1xx011 :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f
convert1xx011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 (noMeta x4) (value x5)

convert1xx1xx :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1xx1xx f x1 x2 x3 x4 x5 = f (value x1) x2 x3 (value x4) x5

convert1xx1x0 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1xx1x0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 (value x4) x5

convert1xx1x1 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f
convert1xx1x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 (value x4) x5

convert1xx10x :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1xx10x f x1 x2 x3 x4 x5 = f (value x1) x2 x3 (value x4) (noMeta x5)

convert1xx100 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1xx100 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 (value x4) (noMeta x5)

convert1xx101 :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f
convert1xx101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 (value x4) (noMeta x5)

convert1xx11x :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f
convert1xx11x f x1 x2 x3 x4 x5 = f (value x1) x2 x3 (value x4) (value x5)

convert1xx110 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f
convert1xx110 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 x3 (value x4) (value x5)

convert1xx111 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert1xx111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 x3 (value x4) (value x5)

convert1x0xxx :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1x0xxx f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) x4 x5

convert1x0xx0 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1x0xx0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) x4 x5

convert1x0xx1 :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1x0xx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) x4 x5

convert1x0x0x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1x0x0x f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) x4 (noMeta x5)

convert1x0x00 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1x0x00 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) x4 (noMeta x5)

convert1x0x01 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1x0x01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) x4 (noMeta x5)

convert1x0x1x :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert1x0x1x f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) x4 (value x5)

convert1x0x10 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert1x0x10 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) x4 (value x5)

convert1x0x11 :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f
convert1x0x11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) x4 (value x5)

convert1x00xx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1x00xx f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) (noMeta x4) x5

convert1x00x0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1x00x0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) x5

convert1x00x1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1x00x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) x5

convert1x000x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1x000x f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5)

convert1x0000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1x0000 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5)

convert1x0001 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1x0001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5)

convert1x001x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert1x001x f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) (noMeta x4) (value x5)

convert1x0010 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert1x0010 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) (value x5)

convert1x0011 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f
convert1x0011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) (value x5)

convert1x01xx :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1x01xx f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) (value x4) x5

convert1x01x0 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1x01x0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) (value x4) x5

convert1x01x1 :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f
convert1x01x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) x5

convert1x010x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1x010x f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) (value x4) (noMeta x5)

convert1x0100 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1x0100 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) (value x4) (noMeta x5)

convert1x0101 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f
convert1x0101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) (noMeta x5)

convert1x011x :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f
convert1x011x f x1 x2 x3 x4 x5 = f (value x1) x2 (noMeta x3) (value x4) (value x5)

convert1x0110 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f
convert1x0110 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (noMeta x3) (value x4) (value x5)

convert1x0111 :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert1x0111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) (value x5)

convert1x1xxx :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1x1xxx f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) x4 x5

convert1x1xx0 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1x1xx0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) x4 x5

convert1x1xx1 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f
convert1x1xx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) x4 x5

convert1x1x0x :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1x1x0x f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) x4 (noMeta x5)

convert1x1x00 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1x1x00 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) x4 (noMeta x5)

convert1x1x01 :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f
convert1x1x01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) x4 (noMeta x5)

convert1x1x1x :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f
convert1x1x1x f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) x4 (value x5)

convert1x1x10 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f
convert1x1x10 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) x4 (value x5)

convert1x1x11 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert1x1x11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) x4 (value x5)

convert1x10xx :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1x10xx f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) (noMeta x4) x5

convert1x10x0 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1x10x0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) (noMeta x4) x5

convert1x10x1 :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f
convert1x10x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) x5

convert1x100x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1x100x f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) (noMeta x4) (noMeta x5)

convert1x1000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1x1000 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) (noMeta x4) (noMeta x5)

convert1x1001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f
convert1x1001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) (noMeta x5)

convert1x101x :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f
convert1x101x f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) (noMeta x4) (value x5)

convert1x1010 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f
convert1x1010 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) (noMeta x4) (value x5)

convert1x1011 :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert1x1011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) (value x5)

convert1x11xx :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f
convert1x11xx f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) (value x4) x5

convert1x11x0 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f
convert1x11x0 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) (value x4) x5

convert1x11x1 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert1x11x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) (value x4) x5

convert1x110x :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f
convert1x110x f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) (value x4) (noMeta x5)

convert1x1100 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f
convert1x1100 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) (value x4) (noMeta x5)

convert1x1101 :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert1x1101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) (value x4) (noMeta x5)

convert1x111x :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert1x111x f x1 x2 x3 x4 x5 = f (value x1) x2 (value x3) (value x4) (value x5)

convert1x1110 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert1x1110 f x1 x2 x3 x4 x5 = value $ f (value x1) x2 (value x3) (value x4) (value x5)

convert1x1111 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convert1x1111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) x2 (value x3) (value x4) (value x5)

convert10xxxx :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert10xxxx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 x4 x5

convert10xxx0 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert10xxx0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 x4 x5

convert10xxx1 :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert10xxx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 x4 x5

convert10xx0x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert10xx0x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 x4 (noMeta x5)

convert10xx00 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert10xx00 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 x4 (noMeta x5)

convert10xx01 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert10xx01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 x4 (noMeta x5)

convert10xx1x :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert10xx1x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 x4 (value x5)

convert10xx10 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert10xx10 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 x4 (value x5)

convert10xx11 :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f
convert10xx11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 x4 (value x5)

convert10x0xx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert10x0xx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 (noMeta x4) x5

convert10x0x0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert10x0x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) x5

convert10x0x1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert10x0x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) x5

convert10x00x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert10x00x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5)

convert10x000 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert10x000 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5)

convert10x001 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert10x001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5)

convert10x01x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert10x01x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 (noMeta x4) (value x5)

convert10x010 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert10x010 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) (value x5)

convert10x011 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f
convert10x011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) (value x5)

convert10x1xx :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert10x1xx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 (value x4) x5

convert10x1x0 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert10x1x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 (value x4) x5

convert10x1x1 :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f
convert10x1x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) x5

convert10x10x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert10x10x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 (value x4) (noMeta x5)

convert10x100 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert10x100 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 (value x4) (noMeta x5)

convert10x101 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f
convert10x101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) (noMeta x5)

convert10x11x :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f
convert10x11x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) x3 (value x4) (value x5)

convert10x110 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f
convert10x110 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) x3 (value x4) (value x5)

convert10x111 :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert10x111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) (value x5)

convert100xxx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert100xxx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) x4 x5

convert100xx0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert100xx0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 x5

convert100xx1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert100xx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 x5

convert100x0x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert100x0x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5)

convert100x00 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert100x00 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5)

convert100x01 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert100x01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5)

convert100x1x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert100x1x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) x4 (value x5)

convert100x10 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert100x10 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 (value x5)

convert100x11 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f
convert100x11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 (value x5)

convert1000xx :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert1000xx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5

convert1000x0 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert1000x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5

convert1000x1 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert1000x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5

convert10000x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> f
convert10000x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert100000 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> d -> e -> f
convert100000 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert100001 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> c -> d -> e -> WithMeta f
convert100001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert10001x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert10001x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convert100010 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> c -> d -> WithMeta e -> f
convert100010 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convert100011 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f
convert100011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5)

convert1001xx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1001xx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) (value x4) x5

convert1001x0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert1001x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) x5

convert1001x1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f
convert1001x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) x5

convert10010x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert10010x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convert100100 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> e -> f
convert100100 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convert100101 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f
convert100101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5)

convert10011x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f
convert10011x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5)

convert100110 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f
convert100110 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5)

convert100111 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert100111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5)

convert101xxx :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert101xxx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) x4 x5

convert101xx0 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert101xx0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) x4 x5

convert101xx1 :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f
convert101xx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 x5

convert101x0x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert101x0x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) x4 (noMeta x5)

convert101x00 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert101x00 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) x4 (noMeta x5)

convert101x01 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f
convert101x01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 (noMeta x5)

convert101x1x :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f
convert101x1x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) x4 (value x5)

convert101x10 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f
convert101x10 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) x4 (value x5)

convert101x11 :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert101x11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 (value x5)

convert1010xx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1010xx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) (noMeta x4) x5

convert1010x0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert1010x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) x5

convert1010x1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f
convert1010x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) x5

convert10100x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert10100x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convert101000 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> e -> f
convert101000 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convert101001 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f
convert101001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5)

convert10101x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f
convert10101x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5)

convert101010 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f
convert101010 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5)

convert101011 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert101011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5)

convert1011xx :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f
convert1011xx f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) (value x4) x5

convert1011x0 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f
convert1011x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) (value x4) x5

convert1011x1 :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert1011x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) x5

convert10110x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f
convert10110x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5)

convert101100 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f
convert101100 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5)

convert101101 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert101101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5)

convert10111x :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert10111x f x1 x2 x3 x4 x5 = f (value x1) (noMeta x2) (value x3) (value x4) (value x5)

convert101110 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert101110 f x1 x2 x3 x4 x5 = value $ f (value x1) (noMeta x2) (value x3) (value x4) (value x5)

convert101111 :: (a -> WithMeta b -> c -> d -> e -> f) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convert101111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) (value x5)

convert11xxxx :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11xxxx f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 x4 x5

convert11xxx0 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11xxx0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 x4 x5

convert11xxx1 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f
convert11xxx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 x4 x5

convert11xx0x :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11xx0x f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 x4 (noMeta x5)

convert11xx00 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11xx00 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 x4 (noMeta x5)

convert11xx01 :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f
convert11xx01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 x4 (noMeta x5)

convert11xx1x :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f
convert11xx1x f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 x4 (value x5)

convert11xx10 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f
convert11xx10 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 x4 (value x5)

convert11xx11 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convert11xx11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 x4 (value x5)

convert11x0xx :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11x0xx f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 (noMeta x4) x5

convert11x0x0 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11x0x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 (noMeta x4) x5

convert11x0x1 :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f
convert11x0x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) x5

convert11x00x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11x00x f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 (noMeta x4) (noMeta x5)

convert11x000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11x000 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 (noMeta x4) (noMeta x5)

convert11x001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f
convert11x001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) (noMeta x5)

convert11x01x :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f
convert11x01x f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 (noMeta x4) (value x5)

convert11x010 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f
convert11x010 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 (noMeta x4) (value x5)

convert11x011 :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convert11x011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) (value x5)

convert11x1xx :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f
convert11x1xx f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 (value x4) x5

convert11x1x0 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f
convert11x1x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 (value x4) x5

convert11x1x1 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convert11x1x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 (value x4) x5

convert11x10x :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f
convert11x10x f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 (value x4) (noMeta x5)

convert11x100 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f
convert11x100 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 (value x4) (noMeta x5)

convert11x101 :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convert11x101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 (value x4) (noMeta x5)

convert11x11x :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convert11x11x f x1 x2 x3 x4 x5 = f (value x1) (value x2) x3 (value x4) (value x5)

convert11x110 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convert11x110 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) x3 (value x4) (value x5)

convert11x111 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert11x111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) x3 (value x4) (value x5)

convert110xxx :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert110xxx f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) x4 x5

convert110xx0 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert110xx0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) x4 x5

convert110xx1 :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f
convert110xx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 x5

convert110x0x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert110x0x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) x4 (noMeta x5)

convert110x00 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert110x00 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) x4 (noMeta x5)

convert110x01 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f
convert110x01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 (noMeta x5)

convert110x1x :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f
convert110x1x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) x4 (value x5)

convert110x10 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f
convert110x10 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) x4 (value x5)

convert110x11 :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convert110x11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 (value x5)

convert1100xx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert1100xx f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) (noMeta x4) x5

convert1100x0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert1100x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) x5

convert1100x1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f
convert1100x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) x5

convert11000x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert11000x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert110000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> e -> f
convert110000 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert110001 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f
convert110001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5)

convert11001x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f
convert11001x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5)

convert110010 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f
convert110010 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5)

convert110011 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f
convert110011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5)

convert1101xx :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f
convert1101xx f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) (value x4) x5

convert1101x0 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f
convert1101x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) (value x4) x5

convert1101x1 :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convert1101x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) x5

convert11010x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f
convert11010x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5)

convert110100 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f
convert110100 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5)

convert110101 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f
convert110101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5)

convert11011x :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convert11011x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (noMeta x3) (value x4) (value x5)

convert110110 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f
convert110110 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (noMeta x3) (value x4) (value x5)

convert110111 :: (a -> b -> WithMeta c -> d -> e -> f) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f
convert110111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) (value x5)

convert111xxx :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f
convert111xxx f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) x4 x5

convert111xx0 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f
convert111xx0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) x4 x5

convert111xx1 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convert111xx1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) x4 x5

convert111x0x :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f
convert111x0x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) x4 (noMeta x5)

convert111x00 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f
convert111x00 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) x4 (noMeta x5)

convert111x01 :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convert111x01 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) x4 (noMeta x5)

convert111x1x :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convert111x1x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) x4 (value x5)

convert111x10 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convert111x10 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) x4 (value x5)

convert111x11 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert111x11 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) x4 (value x5)

convert1110xx :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f
convert1110xx f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) (noMeta x4) x5

convert1110x0 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f
convert1110x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) (noMeta x4) x5

convert1110x1 :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convert1110x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) x5

convert11100x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f
convert11100x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5)

convert111000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f
convert111000 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5)

convert111001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f
convert111001 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5)

convert11101x :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convert11101x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) (noMeta x4) (value x5)

convert111010 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f
convert111010 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) (noMeta x4) (value x5)

convert111011 :: (a -> b -> c -> WithMeta d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f
convert111011 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) (value x5)

convert1111xx :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convert1111xx f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) (value x4) x5

convert1111x0 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convert1111x0 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) (value x4) x5

convert1111x1 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert1111x1 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) (value x4) x5

convert11110x :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convert11110x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) (value x4) (noMeta x5)

convert111100 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f
convert111100 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) (value x4) (noMeta x5)

convert111101 :: (a -> b -> c -> d -> WithMeta e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f
convert111101 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) (value x4) (noMeta x5)

convert11111x :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert11111x f x1 x2 x3 x4 x5 = f (value x1) (value x2) (value x3) (value x4) (value x5)

convert111110 :: (a -> b -> c -> d -> e -> WithMeta f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f
convert111110 f x1 x2 x3 x4 x5 = value $ f (value x1) (value x2) (value x3) (value x4) (value x5)

convert111111 :: (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
convert111111 f x1 x2 x3 x4 x5 = noMeta $ f (value x1) (value x2) (value x3) (value x4) (value x5)

convertxxxxxx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxxxxxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 x5 x6

convertxxxxxx1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxxxxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 x5 x6

convertxxxxx0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxxxxx0x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 x4 x5 (noMeta x6)

convertxxxxx00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxxxxx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 x5 (noMeta x6)

convertxxxxx01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxxxxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 x5 (noMeta x6)

convertxxxxx1x :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxxxxx1x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 x4 x5 (value x6)

convertxxxxx10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxxxxx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 x5 (value x6)

convertxxxxx11 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertxxxxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 x5 (value x6)

convertxxxx0xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxxxx0xx f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 x4 (noMeta x5) x6

convertxxxx0x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxxxx0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 (noMeta x5) x6

convertxxxx0x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxxxx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 (noMeta x5) x6

convertxxxx00x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxxxx00x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 x4 (noMeta x5) (noMeta x6)

convertxxxx000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxxxx000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 (noMeta x5) (noMeta x6)

convertxxxx001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxxxx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 (noMeta x5) (noMeta x6)

convertxxxx01x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxxxx01x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 x4 (noMeta x5) (value x6)

convertxxxx010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxxxx010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 (noMeta x5) (value x6)

convertxxxx011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertxxxx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 (noMeta x5) (value x6)

convertxxxx1xx :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxxxx1xx f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 x4 (value x5) x6

convertxxxx1x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxxxx1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 (value x5) x6

convertxxxx1x1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertxxxx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 (value x5) x6

convertxxxx10x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxxxx10x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 x4 (value x5) (noMeta x6)

convertxxxx100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxxxx100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 (value x5) (noMeta x6)

convertxxxx101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertxxxx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 (value x5) (noMeta x6)

convertxxxx11x :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertxxxx11x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 x4 (value x5) (value x6)

convertxxxx110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertxxxx110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 x4 (value x5) (value x6)

convertxxxx111 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertxxxx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 x4 (value x5) (value x6)

convertxxx0xxx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxxx0xxx f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) x5 x6

convertxxx0xx0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxxx0xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) x5 x6

convertxxx0xx1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxxx0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) x5 x6

convertxxx0x0x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxxx0x0x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) x5 (noMeta x6)

convertxxx0x00 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxxx0x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) x5 (noMeta x6)

convertxxx0x01 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxxx0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) x5 (noMeta x6)

convertxxx0x1x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxxx0x1x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) x5 (value x6)

convertxxx0x10 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxxx0x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) x5 (value x6)

convertxxx0x11 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertxxx0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) x5 (value x6)

convertxxx00xx :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxxx00xx f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) (noMeta x5) x6

convertxxx00x0 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxxx00x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) (noMeta x5) x6

convertxxx00x1 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxxx00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) (noMeta x5) x6

convertxxx000x :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxxx000x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertxxx0000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxxx0000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertxxx0001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxxx0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertxxx001x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxxx001x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) (noMeta x5) (value x6)

convertxxx0010 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxxx0010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) (noMeta x5) (value x6)

convertxxx0011 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertxxx0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) (noMeta x5) (value x6)

convertxxx01xx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxxx01xx f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) (value x5) x6

convertxxx01x0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxxx01x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) (value x5) x6

convertxxx01x1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertxxx01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) (value x5) x6

convertxxx010x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxxx010x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) (value x5) (noMeta x6)

convertxxx0100 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxxx0100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) (value x5) (noMeta x6)

convertxxx0101 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertxxx0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) (value x5) (noMeta x6)

convertxxx011x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertxxx011x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (noMeta x4) (value x5) (value x6)

convertxxx0110 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertxxx0110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (noMeta x4) (value x5) (value x6)

convertxxx0111 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertxxx0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (noMeta x4) (value x5) (value x6)

convertxxx1xxx :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxxx1xxx f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) x5 x6

convertxxx1xx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxxx1xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) x5 x6

convertxxx1xx1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertxxx1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) x5 x6

convertxxx1x0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxxx1x0x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) x5 (noMeta x6)

convertxxx1x00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxxx1x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) x5 (noMeta x6)

convertxxx1x01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertxxx1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) x5 (noMeta x6)

convertxxx1x1x :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertxxx1x1x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) x5 (value x6)

convertxxx1x10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertxxx1x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) x5 (value x6)

convertxxx1x11 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertxxx1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) x5 (value x6)

convertxxx10xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxxx10xx f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) (noMeta x5) x6

convertxxx10x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxxx10x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) (noMeta x5) x6

convertxxx10x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertxxx10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) (noMeta x5) x6

convertxxx100x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxxx100x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) (noMeta x5) (noMeta x6)

convertxxx1000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxxx1000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) (noMeta x5) (noMeta x6)

convertxxx1001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertxxx1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) (noMeta x5) (noMeta x6)

convertxxx101x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertxxx101x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) (noMeta x5) (value x6)

convertxxx1010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertxxx1010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) (noMeta x5) (value x6)

convertxxx1011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertxxx1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) (noMeta x5) (value x6)

convertxxx11xx :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertxxx11xx f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) (value x5) x6

convertxxx11x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertxxx11x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) (value x5) x6

convertxxx11x1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertxxx11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) (value x5) x6

convertxxx110x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertxxx110x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) (value x5) (noMeta x6)

convertxxx1100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertxxx1100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) (value x5) (noMeta x6)

convertxxx1101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertxxx1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) (value x5) (noMeta x6)

convertxxx111x :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertxxx111x f x1 x2 x3 x4 x5 x6 = f x1 x2 x3 (value x4) (value x5) (value x6)

convertxxx1110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertxxx1110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 x3 (value x4) (value x5) (value x6)

convertxxx1111 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertxxx1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 x3 (value x4) (value x5) (value x6)

convertxx0xxxx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxx0xxxx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 x5 x6

convertxx0xxx0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxx0xxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 x5 x6

convertxx0xxx1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxx0xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 x5 x6

convertxx0xx0x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxx0xx0x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 x5 (noMeta x6)

convertxx0xx00 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxx0xx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 x5 (noMeta x6)

convertxx0xx01 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxx0xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 x5 (noMeta x6)

convertxx0xx1x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxx0xx1x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 x5 (value x6)

convertxx0xx10 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxx0xx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 x5 (value x6)

convertxx0xx11 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertxx0xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 x5 (value x6)

convertxx0x0xx :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxx0x0xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 (noMeta x5) x6

convertxx0x0x0 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxx0x0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 (noMeta x5) x6

convertxx0x0x1 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxx0x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 (noMeta x5) x6

convertxx0x00x :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxx0x00x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertxx0x000 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxx0x000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertxx0x001 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxx0x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertxx0x01x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxx0x01x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 (noMeta x5) (value x6)

convertxx0x010 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxx0x010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 (noMeta x5) (value x6)

convertxx0x011 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertxx0x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 (noMeta x5) (value x6)

convertxx0x1xx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxx0x1xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 (value x5) x6

convertxx0x1x0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxx0x1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 (value x5) x6

convertxx0x1x1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertxx0x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 (value x5) x6

convertxx0x10x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxx0x10x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 (value x5) (noMeta x6)

convertxx0x100 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxx0x100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 (value x5) (noMeta x6)

convertxx0x101 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertxx0x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 (value x5) (noMeta x6)

convertxx0x11x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertxx0x11x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) x4 (value x5) (value x6)

convertxx0x110 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertxx0x110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) x4 (value x5) (value x6)

convertxx0x111 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertxx0x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) x4 (value x5) (value x6)

convertxx00xxx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxx00xxx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) x5 x6

convertxx00xx0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxx00xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) x5 x6

convertxx00xx1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxx00xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) x5 x6

convertxx00x0x :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxx00x0x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertxx00x00 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxx00x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertxx00x01 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxx00x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertxx00x1x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxx00x1x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) x5 (value x6)

convertxx00x10 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxx00x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) x5 (value x6)

convertxx00x11 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertxx00x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) x5 (value x6)

convertxx000xx :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxx000xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convertxx000x0 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxx000x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convertxx000x1 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxx000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convertxx0000x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertxx0000x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertxx00000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertxx00000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertxx00001 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertxx00001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertxx0001x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxx0001x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertxx00010 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertxx00010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertxx00011 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertxx00011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertxx001xx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxx001xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) (value x5) x6

convertxx001x0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxx001x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) (value x5) x6

convertxx001x1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertxx001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) (value x5) x6

convertxx0010x :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxx0010x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertxx00100 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertxx00100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertxx00101 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertxx00101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertxx0011x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertxx0011x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convertxx00110 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertxx00110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convertxx00111 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertxx00111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convertxx01xxx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxx01xxx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) x5 x6

convertxx01xx0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxx01xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) x5 x6

convertxx01xx1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertxx01xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) x5 x6

convertxx01x0x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxx01x0x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) x5 (noMeta x6)

convertxx01x00 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxx01x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) x5 (noMeta x6)

convertxx01x01 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertxx01x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) x5 (noMeta x6)

convertxx01x1x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertxx01x1x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) x5 (value x6)

convertxx01x10 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertxx01x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) x5 (value x6)

convertxx01x11 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertxx01x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) x5 (value x6)

convertxx010xx :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxx010xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) (noMeta x5) x6

convertxx010x0 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxx010x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) (noMeta x5) x6

convertxx010x1 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertxx010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) (noMeta x5) x6

convertxx0100x :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxx0100x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertxx01000 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertxx01000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertxx01001 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertxx01001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertxx0101x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertxx0101x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convertxx01010 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertxx01010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convertxx01011 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertxx01011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convertxx011xx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertxx011xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) (value x5) x6

convertxx011x0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertxx011x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) (value x5) x6

convertxx011x1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertxx011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) (value x5) x6

convertxx0110x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertxx0110x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convertxx01100 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertxx01100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convertxx01101 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertxx01101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convertxx0111x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertxx0111x f x1 x2 x3 x4 x5 x6 = f x1 x2 (noMeta x3) (value x4) (value x5) (value x6)

convertxx01110 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertxx01110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (noMeta x3) (value x4) (value x5) (value x6)

convertxx01111 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertxx01111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (noMeta x3) (value x4) (value x5) (value x6)

convertxx1xxxx :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1xxxx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 x5 x6

convertxx1xxx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1xxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 x5 x6

convertxx1xxx1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertxx1xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 x5 x6

convertxx1xx0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1xx0x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 x5 (noMeta x6)

convertxx1xx00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1xx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 x5 (noMeta x6)

convertxx1xx01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertxx1xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 x5 (noMeta x6)

convertxx1xx1x :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertxx1xx1x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 x5 (value x6)

convertxx1xx10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertxx1xx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 x5 (value x6)

convertxx1xx11 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertxx1xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 x5 (value x6)

convertxx1x0xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1x0xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 (noMeta x5) x6

convertxx1x0x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1x0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 (noMeta x5) x6

convertxx1x0x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertxx1x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 (noMeta x5) x6

convertxx1x00x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1x00x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 (noMeta x5) (noMeta x6)

convertxx1x000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1x000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 (noMeta x5) (noMeta x6)

convertxx1x001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertxx1x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 (noMeta x5) (noMeta x6)

convertxx1x01x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertxx1x01x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 (noMeta x5) (value x6)

convertxx1x010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertxx1x010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 (noMeta x5) (value x6)

convertxx1x011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertxx1x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 (noMeta x5) (value x6)

convertxx1x1xx :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertxx1x1xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 (value x5) x6

convertxx1x1x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertxx1x1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 (value x5) x6

convertxx1x1x1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertxx1x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 (value x5) x6

convertxx1x10x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertxx1x10x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 (value x5) (noMeta x6)

convertxx1x100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertxx1x100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 (value x5) (noMeta x6)

convertxx1x101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertxx1x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 (value x5) (noMeta x6)

convertxx1x11x :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertxx1x11x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) x4 (value x5) (value x6)

convertxx1x110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertxx1x110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) x4 (value x5) (value x6)

convertxx1x111 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertxx1x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) x4 (value x5) (value x6)

convertxx10xxx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx10xxx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) x5 x6

convertxx10xx0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx10xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) x5 x6

convertxx10xx1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertxx10xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) x5 x6

convertxx10x0x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx10x0x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) x5 (noMeta x6)

convertxx10x00 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx10x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) x5 (noMeta x6)

convertxx10x01 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertxx10x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) x5 (noMeta x6)

convertxx10x1x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertxx10x1x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) x5 (value x6)

convertxx10x10 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertxx10x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) x5 (value x6)

convertxx10x11 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertxx10x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) x5 (value x6)

convertxx100xx :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx100xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) (noMeta x5) x6

convertxx100x0 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx100x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) (noMeta x5) x6

convertxx100x1 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertxx100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) (noMeta x5) x6

convertxx1000x :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx1000x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertxx10000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertxx10000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertxx10001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertxx10001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertxx1001x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertxx1001x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convertxx10010 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertxx10010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convertxx10011 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertxx10011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convertxx101xx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertxx101xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) (value x5) x6

convertxx101x0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertxx101x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) (value x5) x6

convertxx101x1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertxx101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) (value x5) x6

convertxx1010x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertxx1010x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convertxx10100 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertxx10100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convertxx10101 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertxx10101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convertxx1011x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertxx1011x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (noMeta x4) (value x5) (value x6)

convertxx10110 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertxx10110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (noMeta x4) (value x5) (value x6)

convertxx10111 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertxx10111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (noMeta x4) (value x5) (value x6)

convertxx11xxx :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertxx11xxx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) x5 x6

convertxx11xx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertxx11xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) x5 x6

convertxx11xx1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertxx11xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) x5 x6

convertxx11x0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertxx11x0x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) x5 (noMeta x6)

convertxx11x00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertxx11x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) x5 (noMeta x6)

convertxx11x01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertxx11x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) x5 (noMeta x6)

convertxx11x1x :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertxx11x1x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) x5 (value x6)

convertxx11x10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertxx11x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) x5 (value x6)

convertxx11x11 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertxx11x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) x5 (value x6)

convertxx110xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertxx110xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) (noMeta x5) x6

convertxx110x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertxx110x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) (noMeta x5) x6

convertxx110x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertxx110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) (noMeta x5) x6

convertxx1100x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertxx1100x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convertxx11000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertxx11000 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convertxx11001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertxx11001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convertxx1101x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertxx1101x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) (noMeta x5) (value x6)

convertxx11010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertxx11010 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) (noMeta x5) (value x6)

convertxx11011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertxx11011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) (noMeta x5) (value x6)

convertxx111xx :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertxx111xx f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) (value x5) x6

convertxx111x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertxx111x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) (value x5) x6

convertxx111x1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertxx111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) (value x5) x6

convertxx1110x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertxx1110x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) (value x5) (noMeta x6)

convertxx11100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertxx11100 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) (value x5) (noMeta x6)

convertxx11101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertxx11101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) (value x5) (noMeta x6)

convertxx1111x :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertxx1111x f x1 x2 x3 x4 x5 x6 = f x1 x2 (value x3) (value x4) (value x5) (value x6)

convertxx11110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertxx11110 f x1 x2 x3 x4 x5 x6 = value $ f x1 x2 (value x3) (value x4) (value x5) (value x6)

convertxx11111 :: (a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertxx11111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 x2 (value x3) (value x4) (value x5) (value x6)

convertx0xxxxx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0xxxxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 x5 x6

convertx0xxxx0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0xxxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 x5 x6

convertx0xxxx1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0xxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 x5 x6

convertx0xxx0x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0xxx0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 x5 (noMeta x6)

convertx0xxx00 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0xxx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 x5 (noMeta x6)

convertx0xxx01 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0xxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 x5 (noMeta x6)

convertx0xxx1x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx0xxx1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 x5 (value x6)

convertx0xxx10 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx0xxx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 x5 (value x6)

convertx0xxx11 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx0xxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 x5 (value x6)

convertx0xx0xx :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0xx0xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 (noMeta x5) x6

convertx0xx0x0 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0xx0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 (noMeta x5) x6

convertx0xx0x1 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0xx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 (noMeta x5) x6

convertx0xx00x :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0xx00x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convertx0xx000 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0xx000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convertx0xx001 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0xx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convertx0xx01x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx0xx01x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 (noMeta x5) (value x6)

convertx0xx010 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx0xx010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 (noMeta x5) (value x6)

convertx0xx011 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx0xx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 (noMeta x5) (value x6)

convertx0xx1xx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0xx1xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 (value x5) x6

convertx0xx1x0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0xx1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 (value x5) x6

convertx0xx1x1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx0xx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 (value x5) x6

convertx0xx10x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0xx10x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 (value x5) (noMeta x6)

convertx0xx100 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0xx100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 (value x5) (noMeta x6)

convertx0xx101 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx0xx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 (value x5) (noMeta x6)

convertx0xx11x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx0xx11x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 x4 (value x5) (value x6)

convertx0xx110 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx0xx110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 x4 (value x5) (value x6)

convertx0xx111 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx0xx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 x4 (value x5) (value x6)

convertx0x0xxx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0x0xxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) x5 x6

convertx0x0xx0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0x0xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) x5 x6

convertx0x0xx1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0x0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) x5 x6

convertx0x0x0x :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0x0x0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convertx0x0x00 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0x0x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convertx0x0x01 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0x0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convertx0x0x1x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx0x0x1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) x5 (value x6)

convertx0x0x10 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx0x0x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) x5 (value x6)

convertx0x0x11 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx0x0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) x5 (value x6)

convertx0x00xx :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0x00xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convertx0x00x0 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0x00x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convertx0x00x1 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0x00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convertx0x000x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0x000x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertx0x0000 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0x0000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertx0x0001 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0x0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertx0x001x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx0x001x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convertx0x0010 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx0x0010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convertx0x0011 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx0x0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convertx0x01xx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0x01xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) (value x5) x6

convertx0x01x0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0x01x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) (value x5) x6

convertx0x01x1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx0x01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) (value x5) x6

convertx0x010x :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0x010x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convertx0x0100 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0x0100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convertx0x0101 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx0x0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convertx0x011x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx0x011x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convertx0x0110 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx0x0110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convertx0x0111 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx0x0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convertx0x1xxx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0x1xxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) x5 x6

convertx0x1xx0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0x1xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) x5 x6

convertx0x1xx1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx0x1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) x5 x6

convertx0x1x0x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0x1x0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) x5 (noMeta x6)

convertx0x1x00 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0x1x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) x5 (noMeta x6)

convertx0x1x01 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx0x1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) x5 (noMeta x6)

convertx0x1x1x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx0x1x1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) x5 (value x6)

convertx0x1x10 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx0x1x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) x5 (value x6)

convertx0x1x11 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx0x1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) x5 (value x6)

convertx0x10xx :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0x10xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) (noMeta x5) x6

convertx0x10x0 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0x10x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) (noMeta x5) x6

convertx0x10x1 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx0x10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) (noMeta x5) x6

convertx0x100x :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0x100x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convertx0x1000 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0x1000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convertx0x1001 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx0x1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convertx0x101x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx0x101x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convertx0x1010 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx0x1010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convertx0x1011 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx0x1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convertx0x11xx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx0x11xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) (value x5) x6

convertx0x11x0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx0x11x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) (value x5) x6

convertx0x11x1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx0x11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) (value x5) x6

convertx0x110x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx0x110x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convertx0x1100 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx0x1100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convertx0x1101 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx0x1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convertx0x111x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx0x111x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) x3 (value x4) (value x5) (value x6)

convertx0x1110 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx0x1110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) x3 (value x4) (value x5) (value x6)

convertx0x1111 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertx0x1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) x3 (value x4) (value x5) (value x6)

convertx00xxxx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx00xxxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 x5 x6

convertx00xxx0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx00xxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 x5 x6

convertx00xxx1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx00xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 x5 x6

convertx00xx0x :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx00xx0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convertx00xx00 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx00xx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convertx00xx01 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx00xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convertx00xx1x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx00xx1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 x5 (value x6)

convertx00xx10 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx00xx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 x5 (value x6)

convertx00xx11 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx00xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 x5 (value x6)

convertx00x0xx :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx00x0xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convertx00x0x0 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx00x0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convertx00x0x1 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx00x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convertx00x00x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx00x00x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertx00x000 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx00x000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertx00x001 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx00x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertx00x01x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx00x01x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convertx00x010 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx00x010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convertx00x011 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx00x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convertx00x1xx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx00x1xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 (value x5) x6

convertx00x1x0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx00x1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 (value x5) x6

convertx00x1x1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx00x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 (value x5) x6

convertx00x10x :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx00x10x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convertx00x100 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx00x100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convertx00x101 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx00x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convertx00x11x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx00x11x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convertx00x110 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx00x110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convertx00x111 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx00x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convertx000xxx :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx000xxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convertx000xx0 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx000xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convertx000xx1 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx000xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convertx000x0x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx000x0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertx000x00 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx000x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertx000x01 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx000x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertx000x1x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx000x1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convertx000x10 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx000x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convertx000x11 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx000x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convertx0000xx :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx0000xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convertx0000x0 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx0000x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convertx0000x1 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx0000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convertx00000x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convertx00000x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx000000 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convertx000000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx000001 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convertx000001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx00001x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx00001x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertx000010 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convertx000010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertx000011 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx000011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertx0001xx :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0001xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convertx0001x0 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx0001x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convertx0001x1 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx0001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convertx00010x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx00010x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertx000100 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convertx000100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertx000101 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx000101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertx00011x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx00011x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convertx000110 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx000110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convertx000111 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx000111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convertx001xxx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx001xxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) x5 x6

convertx001xx0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx001xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) x5 x6

convertx001xx1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx001xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) x5 x6

convertx001x0x :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx001x0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convertx001x00 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx001x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convertx001x01 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx001x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convertx001x1x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx001x1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convertx001x10 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx001x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convertx001x11 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx001x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convertx0010xx :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0010xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convertx0010x0 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx0010x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convertx0010x1 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx0010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convertx00100x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx00100x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertx001000 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convertx001000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertx001001 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx001001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertx00101x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx00101x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convertx001010 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx001010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convertx001011 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx001011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convertx0011xx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx0011xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convertx0011x0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx0011x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convertx0011x1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx0011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convertx00110x :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx00110x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convertx001100 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx001100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convertx001101 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx001101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convertx00111x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx00111x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convertx001110 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx001110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convertx001111 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertx001111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convertx01xxxx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01xxxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 x5 x6

convertx01xxx0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01xxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 x5 x6

convertx01xxx1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx01xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 x5 x6

convertx01xx0x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01xx0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 x5 (noMeta x6)

convertx01xx00 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01xx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 x5 (noMeta x6)

convertx01xx01 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx01xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 x5 (noMeta x6)

convertx01xx1x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx01xx1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 x5 (value x6)

convertx01xx10 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx01xx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 x5 (value x6)

convertx01xx11 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertx01xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 x5 (value x6)

convertx01x0xx :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01x0xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 (noMeta x5) x6

convertx01x0x0 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01x0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 (noMeta x5) x6

convertx01x0x1 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx01x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 (noMeta x5) x6

convertx01x00x :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01x00x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convertx01x000 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01x000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convertx01x001 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx01x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convertx01x01x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx01x01x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convertx01x010 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx01x010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convertx01x011 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertx01x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convertx01x1xx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx01x1xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 (value x5) x6

convertx01x1x0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx01x1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 (value x5) x6

convertx01x1x1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertx01x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 (value x5) x6

convertx01x10x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx01x10x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convertx01x100 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx01x100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convertx01x101 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertx01x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convertx01x11x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertx01x11x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) x4 (value x5) (value x6)

convertx01x110 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertx01x110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) x4 (value x5) (value x6)

convertx01x111 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx01x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) x4 (value x5) (value x6)

convertx010xxx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx010xxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) x5 x6

convertx010xx0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx010xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) x5 x6

convertx010xx1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx010xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) x5 x6

convertx010x0x :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx010x0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convertx010x00 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx010x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convertx010x01 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx010x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convertx010x1x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx010x1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convertx010x10 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx010x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convertx010x11 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertx010x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convertx0100xx :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx0100xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convertx0100x0 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx0100x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convertx0100x1 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx0100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convertx01000x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx01000x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx010000 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convertx010000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx010001 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx010001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx01001x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx01001x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convertx010010 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx010010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convertx010011 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertx010011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convertx0101xx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx0101xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convertx0101x0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx0101x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convertx0101x1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertx0101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convertx01010x :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx01010x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convertx010100 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx010100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convertx010101 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertx010101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convertx01011x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertx01011x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convertx010110 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertx010110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convertx010111 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx010111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convertx011xxx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx011xxx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) x5 x6

convertx011xx0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx011xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) x5 x6

convertx011xx1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertx011xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) x5 x6

convertx011x0x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx011x0x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convertx011x00 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx011x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convertx011x01 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertx011x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convertx011x1x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertx011x1x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) x5 (value x6)

convertx011x10 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertx011x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) x5 (value x6)

convertx011x11 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx011x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) x5 (value x6)

convertx0110xx :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx0110xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convertx0110x0 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx0110x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convertx0110x1 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertx0110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convertx01100x :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx01100x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convertx011000 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx011000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convertx011001 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertx011001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convertx01101x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertx01101x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convertx011010 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertx011010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convertx011011 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx011011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convertx0111xx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertx0111xx f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) (value x5) x6

convertx0111x0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertx0111x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) (value x5) x6

convertx0111x1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx0111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) (value x5) x6

convertx01110x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertx01110x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convertx011100 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertx011100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convertx011101 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx011101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convertx01111x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx01111x f x1 x2 x3 x4 x5 x6 = f x1 (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convertx011110 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx011110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convertx011111 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertx011111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convertx1xxxxx :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1xxxxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 x5 x6

convertx1xxxx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1xxxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 x5 x6

convertx1xxxx1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1xxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 x5 x6

convertx1xxx0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1xxx0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 x5 (noMeta x6)

convertx1xxx00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1xxx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 x5 (noMeta x6)

convertx1xxx01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1xxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 x5 (noMeta x6)

convertx1xxx1x :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx1xxx1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 x5 (value x6)

convertx1xxx10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx1xxx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 x5 (value x6)

convertx1xxx11 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx1xxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 x5 (value x6)

convertx1xx0xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1xx0xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 (noMeta x5) x6

convertx1xx0x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1xx0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 (noMeta x5) x6

convertx1xx0x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1xx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 (noMeta x5) x6

convertx1xx00x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1xx00x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 (noMeta x5) (noMeta x6)

convertx1xx000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1xx000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 (noMeta x5) (noMeta x6)

convertx1xx001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1xx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 (noMeta x5) (noMeta x6)

convertx1xx01x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx1xx01x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 (noMeta x5) (value x6)

convertx1xx010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx1xx010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 (noMeta x5) (value x6)

convertx1xx011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx1xx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 (noMeta x5) (value x6)

convertx1xx1xx :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1xx1xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 (value x5) x6

convertx1xx1x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1xx1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 (value x5) x6

convertx1xx1x1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx1xx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 (value x5) x6

convertx1xx10x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1xx10x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 (value x5) (noMeta x6)

convertx1xx100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1xx100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 (value x5) (noMeta x6)

convertx1xx101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx1xx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 (value x5) (noMeta x6)

convertx1xx11x :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx1xx11x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 x4 (value x5) (value x6)

convertx1xx110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx1xx110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 x4 (value x5) (value x6)

convertx1xx111 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx1xx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 x4 (value x5) (value x6)

convertx1x0xxx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1x0xxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) x5 x6

convertx1x0xx0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1x0xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) x5 x6

convertx1x0xx1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1x0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) x5 x6

convertx1x0x0x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1x0x0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) x5 (noMeta x6)

convertx1x0x00 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1x0x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) x5 (noMeta x6)

convertx1x0x01 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1x0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) x5 (noMeta x6)

convertx1x0x1x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx1x0x1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) x5 (value x6)

convertx1x0x10 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx1x0x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) x5 (value x6)

convertx1x0x11 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx1x0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) x5 (value x6)

convertx1x00xx :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1x00xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) (noMeta x5) x6

convertx1x00x0 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1x00x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) (noMeta x5) x6

convertx1x00x1 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1x00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) (noMeta x5) x6

convertx1x000x :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1x000x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertx1x0000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1x0000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertx1x0001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1x0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convertx1x001x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx1x001x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convertx1x0010 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx1x0010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convertx1x0011 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx1x0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convertx1x01xx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1x01xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) (value x5) x6

convertx1x01x0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1x01x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) (value x5) x6

convertx1x01x1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx1x01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) (value x5) x6

convertx1x010x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1x010x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convertx1x0100 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1x0100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convertx1x0101 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx1x0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convertx1x011x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx1x011x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (noMeta x4) (value x5) (value x6)

convertx1x0110 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx1x0110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (noMeta x4) (value x5) (value x6)

convertx1x0111 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx1x0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (noMeta x4) (value x5) (value x6)

convertx1x1xxx :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1x1xxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) x5 x6

convertx1x1xx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1x1xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) x5 x6

convertx1x1xx1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx1x1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) x5 x6

convertx1x1x0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1x1x0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) x5 (noMeta x6)

convertx1x1x00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1x1x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) x5 (noMeta x6)

convertx1x1x01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx1x1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) x5 (noMeta x6)

convertx1x1x1x :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx1x1x1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) x5 (value x6)

convertx1x1x10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx1x1x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) x5 (value x6)

convertx1x1x11 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx1x1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) x5 (value x6)

convertx1x10xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1x10xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) (noMeta x5) x6

convertx1x10x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1x10x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) (noMeta x5) x6

convertx1x10x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx1x10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) (noMeta x5) x6

convertx1x100x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1x100x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convertx1x1000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1x1000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convertx1x1001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx1x1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convertx1x101x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx1x101x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) (noMeta x5) (value x6)

convertx1x1010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx1x1010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) (noMeta x5) (value x6)

convertx1x1011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx1x1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) (noMeta x5) (value x6)

convertx1x11xx :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx1x11xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) (value x5) x6

convertx1x11x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx1x11x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) (value x5) x6

convertx1x11x1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx1x11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) (value x5) x6

convertx1x110x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx1x110x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) (value x5) (noMeta x6)

convertx1x1100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx1x1100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) (value x5) (noMeta x6)

convertx1x1101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx1x1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) (value x5) (noMeta x6)

convertx1x111x :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx1x111x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) x3 (value x4) (value x5) (value x6)

convertx1x1110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx1x1110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) x3 (value x4) (value x5) (value x6)

convertx1x1111 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertx1x1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) x3 (value x4) (value x5) (value x6)

convertx10xxxx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10xxxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 x5 x6

convertx10xxx0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10xxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 x5 x6

convertx10xxx1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx10xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 x5 x6

convertx10xx0x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10xx0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 x5 (noMeta x6)

convertx10xx00 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10xx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 x5 (noMeta x6)

convertx10xx01 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx10xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 x5 (noMeta x6)

convertx10xx1x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx10xx1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 x5 (value x6)

convertx10xx10 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx10xx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 x5 (value x6)

convertx10xx11 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx10xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 x5 (value x6)

convertx10x0xx :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10x0xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 (noMeta x5) x6

convertx10x0x0 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10x0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 (noMeta x5) x6

convertx10x0x1 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx10x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 (noMeta x5) x6

convertx10x00x :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10x00x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertx10x000 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10x000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertx10x001 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx10x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convertx10x01x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx10x01x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convertx10x010 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx10x010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convertx10x011 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx10x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convertx10x1xx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx10x1xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 (value x5) x6

convertx10x1x0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx10x1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 (value x5) x6

convertx10x1x1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx10x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 (value x5) x6

convertx10x10x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx10x10x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convertx10x100 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx10x100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convertx10x101 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx10x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convertx10x11x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx10x11x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) x4 (value x5) (value x6)

convertx10x110 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx10x110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) x4 (value x5) (value x6)

convertx10x111 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx10x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) x4 (value x5) (value x6)

convertx100xxx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx100xxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) x5 x6

convertx100xx0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx100xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) x5 x6

convertx100xx1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx100xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) x5 x6

convertx100x0x :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx100x0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertx100x00 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx100x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertx100x01 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx100x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convertx100x1x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx100x1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convertx100x10 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx100x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convertx100x11 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx100x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convertx1000xx :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1000xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convertx1000x0 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx1000x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convertx1000x1 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx1000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convertx10000x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx10000x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx100000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convertx100000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx100001 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convertx100001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx10001x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx10001x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertx100010 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convertx100010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertx100011 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convertx100011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convertx1001xx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1001xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convertx1001x0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx1001x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convertx1001x1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx1001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convertx10010x :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx10010x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertx100100 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convertx100100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertx100101 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convertx100101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convertx10011x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx10011x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convertx100110 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convertx100110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convertx100111 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx100111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convertx101xxx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx101xxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) x5 x6

convertx101xx0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx101xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) x5 x6

convertx101xx1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx101xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) x5 x6

convertx101x0x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx101x0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convertx101x00 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx101x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convertx101x01 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx101x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convertx101x1x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx101x1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) x5 (value x6)

convertx101x10 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx101x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) x5 (value x6)

convertx101x11 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx101x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) x5 (value x6)

convertx1010xx :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1010xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convertx1010x0 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx1010x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convertx1010x1 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx1010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convertx10100x :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx10100x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertx101000 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convertx101000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertx101001 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convertx101001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convertx10101x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx10101x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convertx101010 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convertx101010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convertx101011 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx101011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convertx1011xx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx1011xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) (value x5) x6

convertx1011x0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx1011x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) (value x5) x6

convertx1011x1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx1011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) (value x5) x6

convertx10110x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx10110x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convertx101100 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convertx101100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convertx101101 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx101101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convertx10111x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx10111x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convertx101110 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx101110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convertx101111 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertx101111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convertx11xxxx :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11xxxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 x5 x6

convertx11xxx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11xxx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 x5 x6

convertx11xxx1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx11xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 x5 x6

convertx11xx0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11xx0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 x5 (noMeta x6)

convertx11xx00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11xx00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 x5 (noMeta x6)

convertx11xx01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx11xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 x5 (noMeta x6)

convertx11xx1x :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx11xx1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 x5 (value x6)

convertx11xx10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx11xx10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 x5 (value x6)

convertx11xx11 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertx11xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 x5 (value x6)

convertx11x0xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11x0xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 (noMeta x5) x6

convertx11x0x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11x0x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 (noMeta x5) x6

convertx11x0x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx11x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 (noMeta x5) x6

convertx11x00x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11x00x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convertx11x000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11x000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convertx11x001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx11x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convertx11x01x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx11x01x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 (noMeta x5) (value x6)

convertx11x010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx11x010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 (noMeta x5) (value x6)

convertx11x011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertx11x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 (noMeta x5) (value x6)

convertx11x1xx :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx11x1xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 (value x5) x6

convertx11x1x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx11x1x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 (value x5) x6

convertx11x1x1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertx11x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 (value x5) x6

convertx11x10x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx11x10x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 (value x5) (noMeta x6)

convertx11x100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx11x100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 (value x5) (noMeta x6)

convertx11x101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertx11x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 (value x5) (noMeta x6)

convertx11x11x :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertx11x11x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) x4 (value x5) (value x6)

convertx11x110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertx11x110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) x4 (value x5) (value x6)

convertx11x111 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx11x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) x4 (value x5) (value x6)

convertx110xxx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx110xxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) x5 x6

convertx110xx0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx110xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) x5 x6

convertx110xx1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx110xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) x5 x6

convertx110x0x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx110x0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convertx110x00 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx110x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convertx110x01 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx110x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convertx110x1x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx110x1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) x5 (value x6)

convertx110x10 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx110x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) x5 (value x6)

convertx110x11 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertx110x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) x5 (value x6)

convertx1100xx :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx1100xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convertx1100x0 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx1100x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convertx1100x1 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx1100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convertx11000x :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx11000x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx110000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convertx110000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx110001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convertx110001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convertx11001x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx11001x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convertx110010 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convertx110010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convertx110011 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convertx110011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convertx1101xx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx1101xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) (value x5) x6

convertx1101x0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx1101x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) (value x5) x6

convertx1101x1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertx1101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) (value x5) x6

convertx11010x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx11010x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convertx110100 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convertx110100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convertx110101 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convertx110101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convertx11011x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertx11011x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convertx110110 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convertx110110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convertx110111 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convertx110111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convertx111xxx :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx111xxx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) x5 x6

convertx111xx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx111xx0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) x5 x6

convertx111xx1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertx111xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) x5 x6

convertx111x0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx111x0x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) x5 (noMeta x6)

convertx111x00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx111x00 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) x5 (noMeta x6)

convertx111x01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertx111x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) x5 (noMeta x6)

convertx111x1x :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertx111x1x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) x5 (value x6)

convertx111x10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertx111x10 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) x5 (value x6)

convertx111x11 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx111x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) x5 (value x6)

convertx1110xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx1110xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) (noMeta x5) x6

convertx1110x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx1110x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) (noMeta x5) x6

convertx1110x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertx1110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) (noMeta x5) x6

convertx11100x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx11100x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convertx111000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convertx111000 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convertx111001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convertx111001 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convertx11101x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertx11101x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convertx111010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convertx111010 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convertx111011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convertx111011 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convertx1111xx :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertx1111xx f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) (value x5) x6

convertx1111x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertx1111x0 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) (value x5) x6

convertx1111x1 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx1111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) (value x5) x6

convertx11110x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertx11110x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convertx111100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convertx111100 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convertx111101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convertx111101 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convertx11111x :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx11111x f x1 x2 x3 x4 x5 x6 = f x1 (value x2) (value x3) (value x4) (value x5) (value x6)

convertx111110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convertx111110 f x1 x2 x3 x4 x5 x6 = value $ f x1 (value x2) (value x3) (value x4) (value x5) (value x6)

convertx111111 :: (a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convertx111111 f x1 x2 x3 x4 x5 x6 = noMeta $ f x1 (value x2) (value x3) (value x4) (value x5) (value x6)

convert0xxxxxx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0xxxxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 x5 x6

convert0xxxxx0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0xxxxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 x5 x6

convert0xxxxx1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0xxxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 x5 x6

convert0xxxx0x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0xxxx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 x5 (noMeta x6)

convert0xxxx00 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0xxxx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 x5 (noMeta x6)

convert0xxxx01 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0xxxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 x5 (noMeta x6)

convert0xxxx1x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0xxxx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 x5 (value x6)

convert0xxxx10 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0xxxx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 x5 (value x6)

convert0xxxx11 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0xxxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 x5 (value x6)

convert0xxx0xx :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0xxx0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 (noMeta x5) x6

convert0xxx0x0 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0xxx0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 (noMeta x5) x6

convert0xxx0x1 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0xxx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 (noMeta x5) x6

convert0xxx00x :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0xxx00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 (noMeta x5) (noMeta x6)

convert0xxx000 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0xxx000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 (noMeta x5) (noMeta x6)

convert0xxx001 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0xxx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 (noMeta x5) (noMeta x6)

convert0xxx01x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0xxx01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 (noMeta x5) (value x6)

convert0xxx010 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0xxx010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 (noMeta x5) (value x6)

convert0xxx011 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0xxx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 (noMeta x5) (value x6)

convert0xxx1xx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0xxx1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 (value x5) x6

convert0xxx1x0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0xxx1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 (value x5) x6

convert0xxx1x1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0xxx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 (value x5) x6

convert0xxx10x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0xxx10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 (value x5) (noMeta x6)

convert0xxx100 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0xxx100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 (value x5) (noMeta x6)

convert0xxx101 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0xxx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 (value x5) (noMeta x6)

convert0xxx11x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0xxx11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 x4 (value x5) (value x6)

convert0xxx110 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0xxx110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 x4 (value x5) (value x6)

convert0xxx111 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0xxx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 x4 (value x5) (value x6)

convert0xx0xxx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0xx0xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) x5 x6

convert0xx0xx0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0xx0xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) x5 x6

convert0xx0xx1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0xx0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) x5 x6

convert0xx0x0x :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0xx0x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) x5 (noMeta x6)

convert0xx0x00 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0xx0x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) x5 (noMeta x6)

convert0xx0x01 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0xx0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) x5 (noMeta x6)

convert0xx0x1x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0xx0x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) x5 (value x6)

convert0xx0x10 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0xx0x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) x5 (value x6)

convert0xx0x11 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0xx0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) x5 (value x6)

convert0xx00xx :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0xx00xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) x6

convert0xx00x0 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0xx00x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) x6

convert0xx00x1 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0xx00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) x6

convert0xx000x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0xx000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert0xx0000 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0xx0000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert0xx0001 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0xx0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert0xx001x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0xx001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) (value x6)

convert0xx0010 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0xx0010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) (value x6)

convert0xx0011 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0xx0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) (noMeta x5) (value x6)

convert0xx01xx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0xx01xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) (value x5) x6

convert0xx01x0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0xx01x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) (value x5) x6

convert0xx01x1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0xx01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) (value x5) x6

convert0xx010x :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0xx010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) (value x5) (noMeta x6)

convert0xx0100 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0xx0100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) (value x5) (noMeta x6)

convert0xx0101 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0xx0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) (value x5) (noMeta x6)

convert0xx011x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0xx011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (noMeta x4) (value x5) (value x6)

convert0xx0110 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0xx0110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (noMeta x4) (value x5) (value x6)

convert0xx0111 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0xx0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (noMeta x4) (value x5) (value x6)

convert0xx1xxx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0xx1xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) x5 x6

convert0xx1xx0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0xx1xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) x5 x6

convert0xx1xx1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0xx1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) x5 x6

convert0xx1x0x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0xx1x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) x5 (noMeta x6)

convert0xx1x00 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0xx1x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) x5 (noMeta x6)

convert0xx1x01 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0xx1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) x5 (noMeta x6)

convert0xx1x1x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0xx1x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) x5 (value x6)

convert0xx1x10 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0xx1x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) x5 (value x6)

convert0xx1x11 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0xx1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) x5 (value x6)

convert0xx10xx :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0xx10xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) (noMeta x5) x6

convert0xx10x0 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0xx10x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) (noMeta x5) x6

convert0xx10x1 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0xx10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) (noMeta x5) x6

convert0xx100x :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0xx100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) (noMeta x5) (noMeta x6)

convert0xx1000 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0xx1000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) (noMeta x5) (noMeta x6)

convert0xx1001 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0xx1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) (noMeta x5) (noMeta x6)

convert0xx101x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0xx101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) (noMeta x5) (value x6)

convert0xx1010 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0xx1010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) (noMeta x5) (value x6)

convert0xx1011 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0xx1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) (noMeta x5) (value x6)

convert0xx11xx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0xx11xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) (value x5) x6

convert0xx11x0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0xx11x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) (value x5) x6

convert0xx11x1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0xx11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) (value x5) x6

convert0xx110x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0xx110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) (value x5) (noMeta x6)

convert0xx1100 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0xx1100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) (value x5) (noMeta x6)

convert0xx1101 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0xx1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) (value x5) (noMeta x6)

convert0xx111x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0xx111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 x3 (value x4) (value x5) (value x6)

convert0xx1110 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0xx1110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 x3 (value x4) (value x5) (value x6)

convert0xx1111 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert0xx1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 x3 (value x4) (value x5) (value x6)

convert0x0xxxx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0x0xxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 x5 x6

convert0x0xxx0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0x0xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 x5 x6

convert0x0xxx1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0x0xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 x5 x6

convert0x0xx0x :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0x0xx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 x5 (noMeta x6)

convert0x0xx00 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0x0xx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 x5 (noMeta x6)

convert0x0xx01 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0x0xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 x5 (noMeta x6)

convert0x0xx1x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0x0xx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 x5 (value x6)

convert0x0xx10 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0x0xx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 x5 (value x6)

convert0x0xx11 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0x0xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 x5 (value x6)

convert0x0x0xx :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0x0x0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) x6

convert0x0x0x0 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0x0x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) x6

convert0x0x0x1 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0x0x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) x6

convert0x0x00x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0x0x00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert0x0x000 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0x0x000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert0x0x001 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0x0x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert0x0x01x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0x0x01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) (value x6)

convert0x0x010 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0x0x010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) (value x6)

convert0x0x011 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0x0x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 (noMeta x5) (value x6)

convert0x0x1xx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0x0x1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 (value x5) x6

convert0x0x1x0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0x0x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 (value x5) x6

convert0x0x1x1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0x0x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 (value x5) x6

convert0x0x10x :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0x0x10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 (value x5) (noMeta x6)

convert0x0x100 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0x0x100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 (value x5) (noMeta x6)

convert0x0x101 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0x0x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 (value x5) (noMeta x6)

convert0x0x11x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0x0x11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) x4 (value x5) (value x6)

convert0x0x110 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0x0x110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) x4 (value x5) (value x6)

convert0x0x111 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0x0x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) x4 (value x5) (value x6)

convert0x00xxx :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0x00xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 x6

convert0x00xx0 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0x00xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 x6

convert0x00xx1 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0x00xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 x6

convert0x00x0x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0x00x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0x00x00 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0x00x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0x00x01 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0x00x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0x00x1x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0x00x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 (value x6)

convert0x00x10 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0x00x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 (value x6)

convert0x00x11 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0x00x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) x5 (value x6)

convert0x000xx :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0x000xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convert0x000x0 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0x000x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convert0x000x1 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0x000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convert0x0000x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0x0000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0x00000 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0x00000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0x00001 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0x00001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0x0001x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0x0001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert0x00010 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0x00010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert0x00011 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0x00011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert0x001xx :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0x001xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) x6

convert0x001x0 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0x001x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) x6

convert0x001x1 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0x001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) x6

convert0x0010x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0x0010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert0x00100 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0x00100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert0x00101 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0x00101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert0x0011x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0x0011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0x00110 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0x00110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0x00111 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0x00111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0x01xxx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0x01xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) x5 x6

convert0x01xx0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0x01xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) x5 x6

convert0x01xx1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0x01xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) x5 x6

convert0x01x0x :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0x01x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) x5 (noMeta x6)

convert0x01x00 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0x01x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) x5 (noMeta x6)

convert0x01x01 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0x01x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) x5 (noMeta x6)

convert0x01x1x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0x01x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) x5 (value x6)

convert0x01x10 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0x01x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) x5 (value x6)

convert0x01x11 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0x01x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) x5 (value x6)

convert0x010xx :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0x010xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) x6

convert0x010x0 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0x010x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) x6

convert0x010x1 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0x010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) x6

convert0x0100x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0x0100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert0x01000 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0x01000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert0x01001 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0x01001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert0x0101x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0x0101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convert0x01010 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0x01010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convert0x01011 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0x01011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convert0x011xx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0x011xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) x6

convert0x011x0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0x011x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) x6

convert0x011x1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0x011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) x6

convert0x0110x :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0x0110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convert0x01100 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0x01100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convert0x01101 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0x01101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convert0x0111x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0x0111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) (value x6)

convert0x01110 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0x01110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) (value x6)

convert0x01111 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert0x01111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (noMeta x3) (value x4) (value x5) (value x6)

convert0x1xxxx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1xxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 x5 x6

convert0x1xxx0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 x5 x6

convert0x1xxx1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0x1xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 x5 x6

convert0x1xx0x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1xx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 x5 (noMeta x6)

convert0x1xx00 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1xx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 x5 (noMeta x6)

convert0x1xx01 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0x1xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 x5 (noMeta x6)

convert0x1xx1x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0x1xx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 x5 (value x6)

convert0x1xx10 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0x1xx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 x5 (value x6)

convert0x1xx11 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert0x1xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 x5 (value x6)

convert0x1x0xx :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1x0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 (noMeta x5) x6

convert0x1x0x0 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 (noMeta x5) x6

convert0x1x0x1 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0x1x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 (noMeta x5) x6

convert0x1x00x :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1x00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 (noMeta x5) (noMeta x6)

convert0x1x000 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1x000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 (noMeta x5) (noMeta x6)

convert0x1x001 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0x1x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 (noMeta x5) (noMeta x6)

convert0x1x01x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0x1x01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 (noMeta x5) (value x6)

convert0x1x010 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0x1x010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 (noMeta x5) (value x6)

convert0x1x011 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert0x1x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 (noMeta x5) (value x6)

convert0x1x1xx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0x1x1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 (value x5) x6

convert0x1x1x0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0x1x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 (value x5) x6

convert0x1x1x1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert0x1x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 (value x5) x6

convert0x1x10x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0x1x10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 (value x5) (noMeta x6)

convert0x1x100 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0x1x100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 (value x5) (noMeta x6)

convert0x1x101 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert0x1x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 (value x5) (noMeta x6)

convert0x1x11x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert0x1x11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) x4 (value x5) (value x6)

convert0x1x110 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert0x1x110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) x4 (value x5) (value x6)

convert0x1x111 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0x1x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) x4 (value x5) (value x6)

convert0x10xxx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x10xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) x5 x6

convert0x10xx0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x10xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) x5 x6

convert0x10xx1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0x10xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) x5 x6

convert0x10x0x :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x10x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) x5 (noMeta x6)

convert0x10x00 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x10x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) x5 (noMeta x6)

convert0x10x01 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0x10x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) x5 (noMeta x6)

convert0x10x1x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0x10x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) x5 (value x6)

convert0x10x10 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0x10x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) x5 (value x6)

convert0x10x11 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert0x10x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) x5 (value x6)

convert0x100xx :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x100xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) x6

convert0x100x0 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x100x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) x6

convert0x100x1 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0x100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) x6

convert0x1000x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x1000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0x10000 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0x10000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0x10001 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0x10001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0x1001x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0x1001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convert0x10010 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0x10010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convert0x10011 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert0x10011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convert0x101xx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0x101xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) x6

convert0x101x0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0x101x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) x6

convert0x101x1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert0x101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) x6

convert0x1010x :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0x1010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convert0x10100 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0x10100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convert0x10101 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert0x10101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convert0x1011x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert0x1011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) (value x6)

convert0x10110 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert0x10110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) (value x6)

convert0x10111 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0x10111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (noMeta x4) (value x5) (value x6)

convert0x11xxx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0x11xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) x5 x6

convert0x11xx0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0x11xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) x5 x6

convert0x11xx1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0x11xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) x5 x6

convert0x11x0x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0x11x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) x5 (noMeta x6)

convert0x11x00 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0x11x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) x5 (noMeta x6)

convert0x11x01 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0x11x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) x5 (noMeta x6)

convert0x11x1x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0x11x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) x5 (value x6)

convert0x11x10 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0x11x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) x5 (value x6)

convert0x11x11 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0x11x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) x5 (value x6)

convert0x110xx :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0x110xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) x6

convert0x110x0 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0x110x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) x6

convert0x110x1 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0x110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) x6

convert0x1100x :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0x1100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convert0x11000 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0x11000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convert0x11001 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0x11001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convert0x1101x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0x1101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) (value x6)

convert0x11010 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0x11010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) (value x6)

convert0x11011 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0x11011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) (noMeta x5) (value x6)

convert0x111xx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert0x111xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) (value x5) x6

convert0x111x0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert0x111x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) (value x5) x6

convert0x111x1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0x111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) (value x5) x6

convert0x1110x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert0x1110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) (value x5) (noMeta x6)

convert0x11100 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert0x11100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) (value x5) (noMeta x6)

convert0x11101 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0x11101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) (value x5) (noMeta x6)

convert0x1111x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0x1111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) x2 (value x3) (value x4) (value x5) (value x6)

convert0x11110 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0x11110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) x2 (value x3) (value x4) (value x5) (value x6)

convert0x11111 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert0x11111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) x2 (value x3) (value x4) (value x5) (value x6)

convert00xxxxx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00xxxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 x5 x6

convert00xxxx0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00xxxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 x5 x6

convert00xxxx1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00xxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 x5 x6

convert00xxx0x :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00xxx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 x5 (noMeta x6)

convert00xxx00 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00xxx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 x5 (noMeta x6)

convert00xxx01 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00xxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 x5 (noMeta x6)

convert00xxx1x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert00xxx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 x5 (value x6)

convert00xxx10 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert00xxx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 x5 (value x6)

convert00xxx11 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert00xxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 x5 (value x6)

convert00xx0xx :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00xx0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) x6

convert00xx0x0 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00xx0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) x6

convert00xx0x1 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00xx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) x6

convert00xx00x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00xx00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convert00xx000 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00xx000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convert00xx001 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00xx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convert00xx01x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert00xx01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) (value x6)

convert00xx010 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert00xx010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) (value x6)

convert00xx011 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert00xx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 (noMeta x5) (value x6)

convert00xx1xx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00xx1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 (value x5) x6

convert00xx1x0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00xx1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 (value x5) x6

convert00xx1x1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert00xx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 (value x5) x6

convert00xx10x :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00xx10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 (value x5) (noMeta x6)

convert00xx100 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00xx100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 (value x5) (noMeta x6)

convert00xx101 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert00xx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 (value x5) (noMeta x6)

convert00xx11x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert00xx11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 x4 (value x5) (value x6)

convert00xx110 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert00xx110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 x4 (value x5) (value x6)

convert00xx111 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert00xx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 x4 (value x5) (value x6)

convert00x0xxx :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00x0xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 x6

convert00x0xx0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00x0xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 x6

convert00x0xx1 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00x0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 x6

convert00x0x0x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00x0x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convert00x0x00 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00x0x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convert00x0x01 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00x0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convert00x0x1x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert00x0x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 (value x6)

convert00x0x10 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert00x0x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 (value x6)

convert00x0x11 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert00x0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) x5 (value x6)

convert00x00xx :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00x00xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convert00x00x0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00x00x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convert00x00x1 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00x00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convert00x000x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00x000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert00x0000 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00x0000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert00x0001 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00x0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert00x001x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert00x001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert00x0010 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert00x0010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert00x0011 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert00x0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert00x01xx :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00x01xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) x6

convert00x01x0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00x01x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) x6

convert00x01x1 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert00x01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) x6

convert00x010x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00x010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert00x0100 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00x0100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert00x0101 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert00x0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert00x011x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert00x011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convert00x0110 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert00x0110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convert00x0111 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert00x0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convert00x1xxx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00x1xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) x5 x6

convert00x1xx0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00x1xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) x5 x6

convert00x1xx1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert00x1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) x5 x6

convert00x1x0x :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00x1x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) x5 (noMeta x6)

convert00x1x00 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00x1x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) x5 (noMeta x6)

convert00x1x01 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert00x1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) x5 (noMeta x6)

convert00x1x1x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert00x1x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) x5 (value x6)

convert00x1x10 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert00x1x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) x5 (value x6)

convert00x1x11 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert00x1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) x5 (value x6)

convert00x10xx :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00x10xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) x6

convert00x10x0 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00x10x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) x6

convert00x10x1 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert00x10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) x6

convert00x100x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00x100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert00x1000 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00x1000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert00x1001 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert00x1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert00x101x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert00x101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convert00x1010 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert00x1010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convert00x1011 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert00x1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convert00x11xx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert00x11xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) x6

convert00x11x0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert00x11x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) x6

convert00x11x1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert00x11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) x6

convert00x110x :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert00x110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convert00x1100 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert00x1100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convert00x1101 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert00x1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convert00x111x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert00x111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) (value x6)

convert00x1110 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert00x1110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) (value x6)

convert00x1111 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert00x1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) x3 (value x4) (value x5) (value x6)

convert000xxxx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert000xxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 x6

convert000xxx0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert000xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 x6

convert000xxx1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert000xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 x6

convert000xx0x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert000xx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convert000xx00 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert000xx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convert000xx01 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert000xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convert000xx1x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert000xx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 (value x6)

convert000xx10 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert000xx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 (value x6)

convert000xx11 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert000xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 x5 (value x6)

convert000x0xx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert000x0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convert000x0x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert000x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convert000x0x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert000x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convert000x00x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert000x00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert000x000 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert000x000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert000x001 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert000x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert000x01x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert000x01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert000x010 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert000x010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert000x011 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert000x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert000x1xx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert000x1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) x6

convert000x1x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert000x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) x6

convert000x1x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert000x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) x6

convert000x10x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert000x10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert000x100 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert000x100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert000x101 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert000x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert000x11x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert000x11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convert000x110 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert000x110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convert000x111 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert000x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convert0000xxx :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0000xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convert0000xx0 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0000xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convert0000xx1 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0000xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convert0000x0x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert0000x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0000x00 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0000x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0000x01 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0000x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0000x1x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0000x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert0000x10 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0000x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert0000x11 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0000x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert00000xx :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
convert00000xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert00000x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert00000x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert00000x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert00000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert000000x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> g
convert000000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0000000 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> e -> f -> g
convert0000000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0000001 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> d -> e -> f -> WithMeta g
convert0000001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert000001x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert000001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert0000010 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> d -> e -> WithMeta f -> g
convert0000010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert0000011 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0000011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert00001xx :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00001xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convert00001x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert00001x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convert00001x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert00001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convert000010x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert000010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert0000100 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> f -> g
convert0000100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert0000101 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0000101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert000011x :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert000011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0000110 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0000110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0000111 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0000111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0001xxx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0001xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 x6

convert0001xx0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0001xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 x6

convert0001xx1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0001xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 x6

convert0001x0x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0001x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert0001x00 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0001x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert0001x01 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0001x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert0001x1x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0001x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convert0001x10 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0001x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convert0001x11 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0001x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convert00010xx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00010xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convert00010x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert00010x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convert00010x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert00010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convert000100x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert000100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert0001000 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> f -> g
convert0001000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert0001001 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0001001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert000101x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert000101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert0001010 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0001010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert0001011 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0001011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert00011xx :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert00011xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convert00011x0 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert00011x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convert00011x1 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert00011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convert000110x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert000110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert0001100 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0001100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert0001101 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0001101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert000111x :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert000111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convert0001110 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0001110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convert0001111 :: (WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert0001111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convert001xxxx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001xxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 x5 x6

convert001xxx0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 x5 x6

convert001xxx1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert001xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 x5 x6

convert001xx0x :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001xx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 x5 (noMeta x6)

convert001xx00 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001xx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 x5 (noMeta x6)

convert001xx01 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert001xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 x5 (noMeta x6)

convert001xx1x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert001xx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 x5 (value x6)

convert001xx10 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert001xx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 x5 (value x6)

convert001xx11 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert001xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 x5 (value x6)

convert001x0xx :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001x0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) x6

convert001x0x0 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) x6

convert001x0x1 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert001x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) x6

convert001x00x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001x00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert001x000 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001x000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert001x001 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert001x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert001x01x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert001x01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convert001x010 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert001x010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convert001x011 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert001x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convert001x1xx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert001x1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) x6

convert001x1x0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert001x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) x6

convert001x1x1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert001x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) x6

convert001x10x :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert001x10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convert001x100 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert001x100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convert001x101 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert001x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convert001x11x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert001x11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) (value x6)

convert001x110 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert001x110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) (value x6)

convert001x111 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert001x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) x4 (value x5) (value x6)

convert0010xxx :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0010xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 x6

convert0010xx0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0010xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 x6

convert0010xx1 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0010xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 x6

convert0010x0x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0010x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert0010x00 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0010x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert0010x01 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0010x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert0010x1x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0010x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convert0010x10 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0010x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convert0010x11 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert0010x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convert00100xx :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert00100xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convert00100x0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert00100x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convert00100x1 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert00100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convert001000x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert001000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0010000 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> f -> g
convert0010000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0010001 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0010001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert001001x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert001001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert0010010 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0010010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert0010011 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert0010011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert00101xx :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert00101xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convert00101x0 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert00101x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convert00101x1 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert00101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convert001010x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert001010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert0010100 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0010100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert0010101 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert0010101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert001011x :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert001011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convert0010110 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert0010110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convert0010111 :: (WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0010111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convert0011xxx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0011xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 x6

convert0011xx0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0011xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 x6

convert0011xx1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0011xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 x6

convert0011x0x :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0011x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convert0011x00 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0011x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convert0011x01 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0011x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convert0011x1x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0011x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 (value x6)

convert0011x10 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0011x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 (value x6)

convert0011x11 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0011x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) x5 (value x6)

convert00110xx :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert00110xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convert00110x0 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert00110x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convert00110x1 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert00110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convert001100x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert001100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert0011000 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0011000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert0011001 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0011001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert001101x :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert001101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convert0011010 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0011010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convert0011011 :: (WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0011011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convert00111xx :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert00111xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) x6

convert00111x0 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert00111x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) x6

convert00111x1 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert00111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) x6

convert001110x :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert001110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convert0011100 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert0011100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convert0011101 :: (WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0011101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convert001111x :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert001111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convert0011110 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0011110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convert0011111 :: (WithMeta a -> WithMeta b -> c -> d -> e -> f -> g) -> a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert0011111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convert01xxxxx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01xxxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 x5 x6

convert01xxxx0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01xxxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 x5 x6

convert01xxxx1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01xxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 x5 x6

convert01xxx0x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01xxx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 x5 (noMeta x6)

convert01xxx00 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01xxx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 x5 (noMeta x6)

convert01xxx01 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01xxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 x5 (noMeta x6)

convert01xxx1x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert01xxx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 x5 (value x6)

convert01xxx10 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert01xxx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 x5 (value x6)

convert01xxx11 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert01xxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 x5 (value x6)

convert01xx0xx :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01xx0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 (noMeta x5) x6

convert01xx0x0 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01xx0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 (noMeta x5) x6

convert01xx0x1 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01xx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 (noMeta x5) x6

convert01xx00x :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01xx00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 (noMeta x5) (noMeta x6)

convert01xx000 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01xx000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 (noMeta x5) (noMeta x6)

convert01xx001 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01xx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 (noMeta x5) (noMeta x6)

convert01xx01x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert01xx01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 (noMeta x5) (value x6)

convert01xx010 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert01xx010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 (noMeta x5) (value x6)

convert01xx011 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert01xx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 (noMeta x5) (value x6)

convert01xx1xx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01xx1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 (value x5) x6

convert01xx1x0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01xx1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 (value x5) x6

convert01xx1x1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert01xx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 (value x5) x6

convert01xx10x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01xx10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 (value x5) (noMeta x6)

convert01xx100 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01xx100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 (value x5) (noMeta x6)

convert01xx101 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert01xx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 (value x5) (noMeta x6)

convert01xx11x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert01xx11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 x4 (value x5) (value x6)

convert01xx110 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert01xx110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 x4 (value x5) (value x6)

convert01xx111 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert01xx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 x4 (value x5) (value x6)

convert01x0xxx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01x0xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) x5 x6

convert01x0xx0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01x0xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) x5 x6

convert01x0xx1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01x0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) x5 x6

convert01x0x0x :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01x0x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) x5 (noMeta x6)

convert01x0x00 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01x0x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) x5 (noMeta x6)

convert01x0x01 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01x0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) x5 (noMeta x6)

convert01x0x1x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert01x0x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) x5 (value x6)

convert01x0x10 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert01x0x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) x5 (value x6)

convert01x0x11 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert01x0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) x5 (value x6)

convert01x00xx :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01x00xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) x6

convert01x00x0 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01x00x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) x6

convert01x00x1 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01x00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) x6

convert01x000x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01x000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert01x0000 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01x0000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert01x0001 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01x0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert01x001x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert01x001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert01x0010 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert01x0010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert01x0011 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert01x0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert01x01xx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01x01xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) x6

convert01x01x0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01x01x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) x6

convert01x01x1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert01x01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) x6

convert01x010x :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01x010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert01x0100 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01x0100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert01x0101 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert01x0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert01x011x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert01x011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) (value x6)

convert01x0110 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert01x0110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) (value x6)

convert01x0111 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert01x0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (noMeta x4) (value x5) (value x6)

convert01x1xxx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01x1xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) x5 x6

convert01x1xx0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01x1xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) x5 x6

convert01x1xx1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert01x1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) x5 x6

convert01x1x0x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01x1x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) x5 (noMeta x6)

convert01x1x00 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01x1x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) x5 (noMeta x6)

convert01x1x01 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert01x1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) x5 (noMeta x6)

convert01x1x1x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert01x1x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) x5 (value x6)

convert01x1x10 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert01x1x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) x5 (value x6)

convert01x1x11 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert01x1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) x5 (value x6)

convert01x10xx :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01x10xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) x6

convert01x10x0 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01x10x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) x6

convert01x10x1 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert01x10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) x6

convert01x100x :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01x100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert01x1000 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01x1000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert01x1001 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert01x1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert01x101x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert01x101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) (value x6)

convert01x1010 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert01x1010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) (value x6)

convert01x1011 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert01x1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) (noMeta x5) (value x6)

convert01x11xx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert01x11xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) (value x5) x6

convert01x11x0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert01x11x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) (value x5) x6

convert01x11x1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert01x11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) (value x5) x6

convert01x110x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert01x110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) (value x5) (noMeta x6)

convert01x1100 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert01x1100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) (value x5) (noMeta x6)

convert01x1101 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert01x1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) (value x5) (noMeta x6)

convert01x111x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert01x111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) x3 (value x4) (value x5) (value x6)

convert01x1110 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert01x1110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) x3 (value x4) (value x5) (value x6)

convert01x1111 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert01x1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) x3 (value x4) (value x5) (value x6)

convert010xxxx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010xxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 x5 x6

convert010xxx0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 x5 x6

convert010xxx1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert010xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 x5 x6

convert010xx0x :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010xx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 x5 (noMeta x6)

convert010xx00 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010xx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 x5 (noMeta x6)

convert010xx01 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert010xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 x5 (noMeta x6)

convert010xx1x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert010xx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 x5 (value x6)

convert010xx10 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert010xx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 x5 (value x6)

convert010xx11 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert010xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 x5 (value x6)

convert010x0xx :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010x0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) x6

convert010x0x0 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) x6

convert010x0x1 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert010x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) x6

convert010x00x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010x00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert010x000 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010x000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert010x001 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert010x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert010x01x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert010x01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert010x010 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert010x010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert010x011 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert010x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert010x1xx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert010x1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) x6

convert010x1x0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert010x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) x6

convert010x1x1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert010x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) x6

convert010x10x :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert010x10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert010x100 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert010x100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert010x101 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert010x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert010x11x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert010x11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) (value x6)

convert010x110 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert010x110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) (value x6)

convert010x111 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert010x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) x4 (value x5) (value x6)

convert0100xxx :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert0100xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 x6

convert0100xx0 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert0100xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 x6

convert0100xx1 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert0100xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 x6

convert0100x0x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert0100x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0100x00 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert0100x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0100x01 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert0100x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert0100x1x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert0100x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert0100x10 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert0100x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert0100x11 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0100x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert01000xx :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01000xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert01000x0 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert01000x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert01000x1 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert01000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert010000x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert010000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0100000 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> f -> g
convert0100000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0100001 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert0100001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert010001x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert010001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert0100010 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert0100010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert0100011 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert0100011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert01001xx :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01001xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convert01001x0 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert01001x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convert01001x1 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert01001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convert010010x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert010010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert0100100 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert0100100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert0100101 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert0100101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert010011x :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert010011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0100110 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert0100110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0100111 :: (WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0100111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert0101xxx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert0101xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 x6

convert0101xx0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert0101xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 x6

convert0101xx1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0101xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 x6

convert0101x0x :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert0101x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert0101x00 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert0101x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert0101x01 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0101x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert0101x1x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0101x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 (value x6)

convert0101x10 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0101x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 (value x6)

convert0101x11 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0101x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) x5 (value x6)

convert01010xx :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01010xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convert01010x0 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert01010x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convert01010x1 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert01010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convert010100x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert010100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert0101000 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert0101000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert0101001 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert0101001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert010101x :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert010101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert0101010 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert0101010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert0101011 :: (WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0101011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert01011xx :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert01011xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) x6

convert01011x0 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert01011x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) x6

convert01011x1 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert01011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) x6

convert010110x :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert010110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert0101100 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert0101100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert0101101 :: (WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0101101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert010111x :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert010111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convert0101110 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0101110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convert0101111 :: (WithMeta a -> b -> WithMeta c -> d -> e -> f -> g) -> a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert0101111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convert011xxxx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011xxxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 x5 x6

convert011xxx0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 x5 x6

convert011xxx1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert011xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 x5 x6

convert011xx0x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011xx0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 x5 (noMeta x6)

convert011xx00 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011xx00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 x5 (noMeta x6)

convert011xx01 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert011xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 x5 (noMeta x6)

convert011xx1x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert011xx1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 x5 (value x6)

convert011xx10 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert011xx10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 x5 (value x6)

convert011xx11 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert011xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 x5 (value x6)

convert011x0xx :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011x0xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) x6

convert011x0x0 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) x6

convert011x0x1 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert011x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) x6

convert011x00x :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011x00x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert011x000 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011x000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert011x001 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert011x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert011x01x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert011x01x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) (value x6)

convert011x010 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert011x010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) (value x6)

convert011x011 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert011x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 (noMeta x5) (value x6)

convert011x1xx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert011x1xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 (value x5) x6

convert011x1x0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert011x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 (value x5) x6

convert011x1x1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert011x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 (value x5) x6

convert011x10x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert011x10x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 (value x5) (noMeta x6)

convert011x100 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert011x100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 (value x5) (noMeta x6)

convert011x101 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert011x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 (value x5) (noMeta x6)

convert011x11x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert011x11x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) x4 (value x5) (value x6)

convert011x110 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert011x110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) x4 (value x5) (value x6)

convert011x111 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert011x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) x4 (value x5) (value x6)

convert0110xxx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert0110xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 x6

convert0110xx0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert0110xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 x6

convert0110xx1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0110xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 x6

convert0110x0x :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert0110x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert0110x00 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert0110x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert0110x01 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0110x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert0110x1x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0110x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 (value x6)

convert0110x10 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0110x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 (value x6)

convert0110x11 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert0110x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) x5 (value x6)

convert01100xx :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert01100xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convert01100x0 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert01100x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convert01100x1 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert01100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convert011000x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert011000x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0110000 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert0110000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert0110001 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert0110001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert011001x :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert011001x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert0110010 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert0110010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert0110011 :: (WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert0110011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert01101xx :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert01101xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) x6

convert01101x0 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert01101x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) x6

convert01101x1 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert01101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) x6

convert011010x :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert011010x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert0110100 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert0110100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert0110101 :: (WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert0110101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert011011x :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert011011x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convert0110110 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert0110110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convert0110111 :: (WithMeta a -> b -> c -> WithMeta d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert0110111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convert0111xxx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0111xxx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) x5 x6

convert0111xx0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0111xx0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) x5 x6

convert0111xx1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0111xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) x5 x6

convert0111x0x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0111x0x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) x5 (noMeta x6)

convert0111x00 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0111x00 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) x5 (noMeta x6)

convert0111x01 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0111x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) x5 (noMeta x6)

convert0111x1x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0111x1x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) x5 (value x6)

convert0111x10 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0111x10 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) x5 (value x6)

convert0111x11 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0111x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) x5 (value x6)

convert01110xx :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert01110xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) x6

convert01110x0 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert01110x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) x6

convert01110x1 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert01110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) x6

convert011100x :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert011100x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert0111000 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert0111000 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert0111001 :: (WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert0111001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert011101x :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert011101x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convert0111010 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert0111010 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convert0111011 :: (WithMeta a -> b -> c -> d -> WithMeta e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert0111011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convert01111xx :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert01111xx f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) (value x5) x6

convert01111x0 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert01111x0 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) (value x5) x6

convert01111x1 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert01111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) (value x5) x6

convert011110x :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert011110x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convert0111100 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert0111100 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convert0111101 :: (WithMeta a -> b -> c -> d -> e -> WithMeta f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert0111101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convert011111x :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert011111x f x1 x2 x3 x4 x5 x6 = f (noMeta x1) (value x2) (value x3) (value x4) (value x5) (value x6)

convert0111110 :: (WithMeta a -> b -> c -> d -> e -> f -> WithMeta g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert0111110 f x1 x2 x3 x4 x5 x6 = value $ f (noMeta x1) (value x2) (value x3) (value x4) (value x5) (value x6)

convert0111111 :: (WithMeta a -> b -> c -> d -> e -> f -> g) -> a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert0111111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (noMeta x1) (value x2) (value x3) (value x4) (value x5) (value x6)

convert1xxxxxx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xxxxxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 x5 x6

convert1xxxxx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xxxxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 x5 x6

convert1xxxxx1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1xxxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 x5 x6

convert1xxxx0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xxxx0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 x5 (noMeta x6)

convert1xxxx00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xxxx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 x5 (noMeta x6)

convert1xxxx01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1xxxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 x5 (noMeta x6)

convert1xxxx1x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1xxxx1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 x5 (value x6)

convert1xxxx10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1xxxx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 x5 (value x6)

convert1xxxx11 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1xxxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 x5 (value x6)

convert1xxx0xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xxx0xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 (noMeta x5) x6

convert1xxx0x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xxx0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 (noMeta x5) x6

convert1xxx0x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1xxx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 (noMeta x5) x6

convert1xxx00x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xxx00x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 (noMeta x5) (noMeta x6)

convert1xxx000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xxx000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 (noMeta x5) (noMeta x6)

convert1xxx001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1xxx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 (noMeta x5) (noMeta x6)

convert1xxx01x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1xxx01x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 (noMeta x5) (value x6)

convert1xxx010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1xxx010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 (noMeta x5) (value x6)

convert1xxx011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1xxx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 (noMeta x5) (value x6)

convert1xxx1xx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1xxx1xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 (value x5) x6

convert1xxx1x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1xxx1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 (value x5) x6

convert1xxx1x1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1xxx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 (value x5) x6

convert1xxx10x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1xxx10x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 (value x5) (noMeta x6)

convert1xxx100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1xxx100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 (value x5) (noMeta x6)

convert1xxx101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1xxx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 (value x5) (noMeta x6)

convert1xxx11x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1xxx11x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 x4 (value x5) (value x6)

convert1xxx110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1xxx110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 x4 (value x5) (value x6)

convert1xxx111 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1xxx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 x4 (value x5) (value x6)

convert1xx0xxx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xx0xxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) x5 x6

convert1xx0xx0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xx0xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) x5 x6

convert1xx0xx1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1xx0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) x5 x6

convert1xx0x0x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xx0x0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) x5 (noMeta x6)

convert1xx0x00 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xx0x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) x5 (noMeta x6)

convert1xx0x01 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1xx0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) x5 (noMeta x6)

convert1xx0x1x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1xx0x1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) x5 (value x6)

convert1xx0x10 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1xx0x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) x5 (value x6)

convert1xx0x11 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1xx0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) x5 (value x6)

convert1xx00xx :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xx00xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) (noMeta x5) x6

convert1xx00x0 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xx00x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) (noMeta x5) x6

convert1xx00x1 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1xx00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) (noMeta x5) x6

convert1xx000x :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xx000x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert1xx0000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1xx0000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert1xx0001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1xx0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert1xx001x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1xx001x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) (noMeta x5) (value x6)

convert1xx0010 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1xx0010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) (noMeta x5) (value x6)

convert1xx0011 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1xx0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) (noMeta x5) (value x6)

convert1xx01xx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1xx01xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) (value x5) x6

convert1xx01x0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1xx01x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) (value x5) x6

convert1xx01x1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1xx01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) (value x5) x6

convert1xx010x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1xx010x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) (value x5) (noMeta x6)

convert1xx0100 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1xx0100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) (value x5) (noMeta x6)

convert1xx0101 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1xx0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) (value x5) (noMeta x6)

convert1xx011x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1xx011x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (noMeta x4) (value x5) (value x6)

convert1xx0110 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1xx0110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (noMeta x4) (value x5) (value x6)

convert1xx0111 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1xx0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (noMeta x4) (value x5) (value x6)

convert1xx1xxx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1xx1xxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) x5 x6

convert1xx1xx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1xx1xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) x5 x6

convert1xx1xx1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1xx1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) x5 x6

convert1xx1x0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1xx1x0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) x5 (noMeta x6)

convert1xx1x00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1xx1x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) x5 (noMeta x6)

convert1xx1x01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1xx1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) x5 (noMeta x6)

convert1xx1x1x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1xx1x1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) x5 (value x6)

convert1xx1x10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1xx1x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) x5 (value x6)

convert1xx1x11 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1xx1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) x5 (value x6)

convert1xx10xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1xx10xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) (noMeta x5) x6

convert1xx10x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1xx10x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) (noMeta x5) x6

convert1xx10x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1xx10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) (noMeta x5) x6

convert1xx100x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1xx100x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) (noMeta x5) (noMeta x6)

convert1xx1000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1xx1000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) (noMeta x5) (noMeta x6)

convert1xx1001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1xx1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) (noMeta x5) (noMeta x6)

convert1xx101x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1xx101x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) (noMeta x5) (value x6)

convert1xx1010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1xx1010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) (noMeta x5) (value x6)

convert1xx1011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1xx1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) (noMeta x5) (value x6)

convert1xx11xx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1xx11xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) (value x5) x6

convert1xx11x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1xx11x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) (value x5) x6

convert1xx11x1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1xx11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) (value x5) x6

convert1xx110x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1xx110x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) (value x5) (noMeta x6)

convert1xx1100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1xx1100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) (value x5) (noMeta x6)

convert1xx1101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1xx1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) (value x5) (noMeta x6)

convert1xx111x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1xx111x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 x3 (value x4) (value x5) (value x6)

convert1xx1110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1xx1110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 x3 (value x4) (value x5) (value x6)

convert1xx1111 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert1xx1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 x3 (value x4) (value x5) (value x6)

convert1x0xxxx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0xxxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 x5 x6

convert1x0xxx0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 x5 x6

convert1x0xxx1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1x0xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 x5 x6

convert1x0xx0x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0xx0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 x5 (noMeta x6)

convert1x0xx00 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0xx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 x5 (noMeta x6)

convert1x0xx01 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1x0xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 x5 (noMeta x6)

convert1x0xx1x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1x0xx1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 x5 (value x6)

convert1x0xx10 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1x0xx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 x5 (value x6)

convert1x0xx11 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1x0xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 x5 (value x6)

convert1x0x0xx :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0x0xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 (noMeta x5) x6

convert1x0x0x0 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 (noMeta x5) x6

convert1x0x0x1 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1x0x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 (noMeta x5) x6

convert1x0x00x :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0x00x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert1x0x000 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0x000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert1x0x001 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1x0x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert1x0x01x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1x0x01x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 (noMeta x5) (value x6)

convert1x0x010 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1x0x010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 (noMeta x5) (value x6)

convert1x0x011 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1x0x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 (noMeta x5) (value x6)

convert1x0x1xx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1x0x1xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 (value x5) x6

convert1x0x1x0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1x0x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 (value x5) x6

convert1x0x1x1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1x0x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 (value x5) x6

convert1x0x10x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1x0x10x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 (value x5) (noMeta x6)

convert1x0x100 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1x0x100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 (value x5) (noMeta x6)

convert1x0x101 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1x0x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 (value x5) (noMeta x6)

convert1x0x11x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1x0x11x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) x4 (value x5) (value x6)

convert1x0x110 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1x0x110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) x4 (value x5) (value x6)

convert1x0x111 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1x0x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) x4 (value x5) (value x6)

convert1x00xxx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x00xxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) x5 x6

convert1x00xx0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x00xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) x5 x6

convert1x00xx1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1x00xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) x5 x6

convert1x00x0x :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x00x0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1x00x00 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x00x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1x00x01 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1x00x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1x00x1x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1x00x1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) x5 (value x6)

convert1x00x10 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1x00x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) x5 (value x6)

convert1x00x11 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1x00x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) x5 (value x6)

convert1x000xx :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x000xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convert1x000x0 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x000x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convert1x000x1 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1x000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) x6

convert1x0000x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x0000x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1x00000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1x00000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1x00001 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1x00001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1x0001x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1x0001x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert1x00010 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1x00010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert1x00011 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1x00011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert1x001xx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1x001xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) x6

convert1x001x0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1x001x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) x6

convert1x001x1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1x001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) x6

convert1x0010x :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1x0010x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert1x00100 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1x00100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert1x00101 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1x00101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert1x0011x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1x0011x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1x00110 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1x00110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1x00111 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1x00111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1x01xxx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1x01xxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) x5 x6

convert1x01xx0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1x01xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) x5 x6

convert1x01xx1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1x01xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) x5 x6

convert1x01x0x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1x01x0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) x5 (noMeta x6)

convert1x01x00 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1x01x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) x5 (noMeta x6)

convert1x01x01 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1x01x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) x5 (noMeta x6)

convert1x01x1x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1x01x1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) x5 (value x6)

convert1x01x10 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1x01x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) x5 (value x6)

convert1x01x11 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1x01x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) x5 (value x6)

convert1x010xx :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1x010xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) x6

convert1x010x0 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1x010x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) x6

convert1x010x1 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1x010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) x6

convert1x0100x :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1x0100x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert1x01000 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1x01000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert1x01001 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1x01001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert1x0101x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1x0101x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convert1x01010 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1x01010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convert1x01011 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1x01011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) (noMeta x5) (value x6)

convert1x011xx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1x011xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) (value x5) x6

convert1x011x0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1x011x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) (value x5) x6

convert1x011x1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1x011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) (value x5) x6

convert1x0110x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1x0110x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convert1x01100 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1x01100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convert1x01101 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1x01101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) (value x5) (noMeta x6)

convert1x0111x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1x0111x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (noMeta x3) (value x4) (value x5) (value x6)

convert1x01110 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1x01110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (noMeta x3) (value x4) (value x5) (value x6)

convert1x01111 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert1x01111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (noMeta x3) (value x4) (value x5) (value x6)

convert1x1xxxx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1xxxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 x5 x6

convert1x1xxx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 x5 x6

convert1x1xxx1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1x1xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 x5 x6

convert1x1xx0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1xx0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 x5 (noMeta x6)

convert1x1xx00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1xx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 x5 (noMeta x6)

convert1x1xx01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1x1xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 x5 (noMeta x6)

convert1x1xx1x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1x1xx1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 x5 (value x6)

convert1x1xx10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1x1xx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 x5 (value x6)

convert1x1xx11 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert1x1xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 x5 (value x6)

convert1x1x0xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1x0xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 (noMeta x5) x6

convert1x1x0x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 (noMeta x5) x6

convert1x1x0x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1x1x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 (noMeta x5) x6

convert1x1x00x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1x00x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 (noMeta x5) (noMeta x6)

convert1x1x000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1x000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 (noMeta x5) (noMeta x6)

convert1x1x001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1x1x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 (noMeta x5) (noMeta x6)

convert1x1x01x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1x1x01x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 (noMeta x5) (value x6)

convert1x1x010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1x1x010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 (noMeta x5) (value x6)

convert1x1x011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert1x1x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 (noMeta x5) (value x6)

convert1x1x1xx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1x1x1xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 (value x5) x6

convert1x1x1x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1x1x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 (value x5) x6

convert1x1x1x1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert1x1x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 (value x5) x6

convert1x1x10x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1x1x10x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 (value x5) (noMeta x6)

convert1x1x100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1x1x100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 (value x5) (noMeta x6)

convert1x1x101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert1x1x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 (value x5) (noMeta x6)

convert1x1x11x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert1x1x11x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) x4 (value x5) (value x6)

convert1x1x110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert1x1x110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) x4 (value x5) (value x6)

convert1x1x111 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1x1x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) x4 (value x5) (value x6)

convert1x10xxx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x10xxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) x5 x6

convert1x10xx0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x10xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) x5 x6

convert1x10xx1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1x10xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) x5 x6

convert1x10x0x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x10x0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) x5 (noMeta x6)

convert1x10x00 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x10x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) x5 (noMeta x6)

convert1x10x01 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1x10x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) x5 (noMeta x6)

convert1x10x1x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1x10x1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) x5 (value x6)

convert1x10x10 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1x10x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) x5 (value x6)

convert1x10x11 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert1x10x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) x5 (value x6)

convert1x100xx :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x100xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) x6

convert1x100x0 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x100x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) x6

convert1x100x1 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1x100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) x6

convert1x1000x :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x1000x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1x10000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1x10000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1x10001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1x10001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1x1001x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1x1001x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convert1x10010 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1x10010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convert1x10011 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert1x10011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) (noMeta x5) (value x6)

convert1x101xx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1x101xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) (value x5) x6

convert1x101x0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1x101x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) (value x5) x6

convert1x101x1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert1x101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) (value x5) x6

convert1x1010x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1x1010x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convert1x10100 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1x10100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convert1x10101 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert1x10101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) (value x5) (noMeta x6)

convert1x1011x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert1x1011x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (noMeta x4) (value x5) (value x6)

convert1x10110 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert1x10110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (noMeta x4) (value x5) (value x6)

convert1x10111 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1x10111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (noMeta x4) (value x5) (value x6)

convert1x11xxx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1x11xxx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) x5 x6

convert1x11xx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1x11xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) x5 x6

convert1x11xx1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1x11xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) x5 x6

convert1x11x0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1x11x0x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) x5 (noMeta x6)

convert1x11x00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1x11x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) x5 (noMeta x6)

convert1x11x01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1x11x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) x5 (noMeta x6)

convert1x11x1x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1x11x1x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) x5 (value x6)

convert1x11x10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1x11x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) x5 (value x6)

convert1x11x11 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1x11x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) x5 (value x6)

convert1x110xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1x110xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) (noMeta x5) x6

convert1x110x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1x110x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) (noMeta x5) x6

convert1x110x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1x110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) (noMeta x5) x6

convert1x1100x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1x1100x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convert1x11000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1x11000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convert1x11001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1x11001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) (noMeta x5) (noMeta x6)

convert1x1101x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1x1101x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) (noMeta x5) (value x6)

convert1x11010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1x11010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) (noMeta x5) (value x6)

convert1x11011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1x11011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) (noMeta x5) (value x6)

convert1x111xx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert1x111xx f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) (value x5) x6

convert1x111x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert1x111x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) (value x5) x6

convert1x111x1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1x111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) (value x5) x6

convert1x1110x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert1x1110x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) (value x5) (noMeta x6)

convert1x11100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert1x11100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) (value x5) (noMeta x6)

convert1x11101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1x11101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) (value x5) (noMeta x6)

convert1x1111x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1x1111x f x1 x2 x3 x4 x5 x6 = f (value x1) x2 (value x3) (value x4) (value x5) (value x6)

convert1x11110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1x11110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) x2 (value x3) (value x4) (value x5) (value x6)

convert1x11111 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert1x11111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) x2 (value x3) (value x4) (value x5) (value x6)

convert10xxxxx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10xxxxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 x5 x6

convert10xxxx0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10xxxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 x5 x6

convert10xxxx1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10xxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 x5 x6

convert10xxx0x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10xxx0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 x5 (noMeta x6)

convert10xxx00 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10xxx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 x5 (noMeta x6)

convert10xxx01 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10xxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 x5 (noMeta x6)

convert10xxx1x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert10xxx1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 x5 (value x6)

convert10xxx10 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert10xxx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 x5 (value x6)

convert10xxx11 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert10xxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 x5 (value x6)

convert10xx0xx :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10xx0xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 (noMeta x5) x6

convert10xx0x0 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10xx0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 (noMeta x5) x6

convert10xx0x1 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10xx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 (noMeta x5) x6

convert10xx00x :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10xx00x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convert10xx000 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10xx000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convert10xx001 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10xx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 (noMeta x5) (noMeta x6)

convert10xx01x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert10xx01x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 (noMeta x5) (value x6)

convert10xx010 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert10xx010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 (noMeta x5) (value x6)

convert10xx011 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert10xx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 (noMeta x5) (value x6)

convert10xx1xx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10xx1xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 (value x5) x6

convert10xx1x0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10xx1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 (value x5) x6

convert10xx1x1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert10xx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 (value x5) x6

convert10xx10x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10xx10x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 (value x5) (noMeta x6)

convert10xx100 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10xx100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 (value x5) (noMeta x6)

convert10xx101 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert10xx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 (value x5) (noMeta x6)

convert10xx11x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert10xx11x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 x4 (value x5) (value x6)

convert10xx110 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert10xx110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 x4 (value x5) (value x6)

convert10xx111 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert10xx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 x4 (value x5) (value x6)

convert10x0xxx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10x0xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) x5 x6

convert10x0xx0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10x0xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) x5 x6

convert10x0xx1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10x0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) x5 x6

convert10x0x0x :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10x0x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convert10x0x00 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10x0x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convert10x0x01 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10x0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) x5 (noMeta x6)

convert10x0x1x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert10x0x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) x5 (value x6)

convert10x0x10 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert10x0x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) x5 (value x6)

convert10x0x11 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert10x0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) x5 (value x6)

convert10x00xx :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10x00xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convert10x00x0 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10x00x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convert10x00x1 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10x00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) x6

convert10x000x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10x000x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert10x0000 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10x0000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert10x0001 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10x0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert10x001x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert10x001x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert10x0010 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert10x0010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert10x0011 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert10x0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert10x01xx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10x01xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) x6

convert10x01x0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10x01x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) x6

convert10x01x1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert10x01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) x6

convert10x010x :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10x010x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert10x0100 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10x0100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert10x0101 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert10x0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert10x011x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert10x011x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convert10x0110 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert10x0110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convert10x0111 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert10x0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (noMeta x4) (value x5) (value x6)

convert10x1xxx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10x1xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) x5 x6

convert10x1xx0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10x1xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) x5 x6

convert10x1xx1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert10x1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) x5 x6

convert10x1x0x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10x1x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) x5 (noMeta x6)

convert10x1x00 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10x1x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) x5 (noMeta x6)

convert10x1x01 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert10x1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) x5 (noMeta x6)

convert10x1x1x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert10x1x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) x5 (value x6)

convert10x1x10 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert10x1x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) x5 (value x6)

convert10x1x11 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert10x1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) x5 (value x6)

convert10x10xx :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10x10xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) x6

convert10x10x0 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10x10x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) x6

convert10x10x1 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert10x10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) x6

convert10x100x :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10x100x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert10x1000 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10x1000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert10x1001 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert10x1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert10x101x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert10x101x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convert10x1010 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert10x1010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convert10x1011 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert10x1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) (noMeta x5) (value x6)

convert10x11xx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert10x11xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) (value x5) x6

convert10x11x0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert10x11x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) (value x5) x6

convert10x11x1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert10x11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) (value x5) x6

convert10x110x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert10x110x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convert10x1100 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert10x1100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convert10x1101 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert10x1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) (value x5) (noMeta x6)

convert10x111x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert10x111x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) x3 (value x4) (value x5) (value x6)

convert10x1110 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert10x1110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) x3 (value x4) (value x5) (value x6)

convert10x1111 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert10x1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) x3 (value x4) (value x5) (value x6)

convert100xxxx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100xxxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 x5 x6

convert100xxx0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 x5 x6

convert100xxx1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert100xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 x5 x6

convert100xx0x :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100xx0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convert100xx00 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100xx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convert100xx01 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert100xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 x5 (noMeta x6)

convert100xx1x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert100xx1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 x5 (value x6)

convert100xx10 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert100xx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 x5 (value x6)

convert100xx11 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert100xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 x5 (value x6)

convert100x0xx :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100x0xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convert100x0x0 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convert100x0x1 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert100x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) x6

convert100x00x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100x00x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert100x000 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100x000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert100x001 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert100x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert100x01x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert100x01x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert100x010 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert100x010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert100x011 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert100x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert100x1xx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert100x1xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) x6

convert100x1x0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert100x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) x6

convert100x1x1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert100x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) x6

convert100x10x :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert100x10x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert100x100 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert100x100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert100x101 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert100x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert100x11x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert100x11x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convert100x110 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert100x110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convert100x111 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert100x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) x4 (value x5) (value x6)

convert1000xxx :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1000xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convert1000xx0 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1000xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convert1000xx1 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1000xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 x6

convert1000x0x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1000x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1000x00 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1000x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1000x01 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1000x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1000x1x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1000x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert1000x10 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1000x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert1000x11 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1000x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert10000xx :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10000xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert10000x0 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert10000x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert10000x1 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert10000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert100000x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert100000x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1000000 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> f -> g
convert1000000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1000001 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> e -> f -> WithMeta g
convert1000001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert100001x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert100001x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert1000010 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> g
convert1000010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert1000011 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1000011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert10001xx :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10001xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convert10001x0 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert10001x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convert10001x1 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert10001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) x6

convert100010x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert100010x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert1000100 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> g
convert1000100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert1000101 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1000101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert100011x :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert100011x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1000110 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1000110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1000111 :: (a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1000111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1001xxx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1001xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 x6

convert1001xx0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1001xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 x6

convert1001xx1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1001xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 x6

convert1001x0x :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1001x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert1001x00 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1001x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert1001x01 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1001x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert1001x1x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1001x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convert1001x10 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1001x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convert1001x11 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1001x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) x5 (value x6)

convert10010xx :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10010xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convert10010x0 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert10010x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convert10010x1 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert10010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) x6

convert100100x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert100100x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert1001000 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> g
convert1001000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert1001001 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1001001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert100101x :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert100101x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert1001010 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1001010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert1001011 :: (a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1001011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert10011xx :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert10011xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convert10011x0 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert10011x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convert10011x1 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert10011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) x6

convert100110x :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert100110x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert1001100 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1001100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert1001101 :: (a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1001101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert100111x :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert100111x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convert1001110 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1001110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convert1001111 :: (a -> WithMeta b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert1001111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (noMeta x3) (value x4) (value x5) (value x6)

convert101xxxx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101xxxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 x5 x6

convert101xxx0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 x5 x6

convert101xxx1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert101xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 x5 x6

convert101xx0x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101xx0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 x5 (noMeta x6)

convert101xx00 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101xx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 x5 (noMeta x6)

convert101xx01 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert101xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 x5 (noMeta x6)

convert101xx1x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert101xx1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 x5 (value x6)

convert101xx10 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert101xx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 x5 (value x6)

convert101xx11 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert101xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 x5 (value x6)

convert101x0xx :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101x0xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) x6

convert101x0x0 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) x6

convert101x0x1 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert101x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) x6

convert101x00x :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101x00x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert101x000 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101x000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert101x001 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert101x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert101x01x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert101x01x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convert101x010 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert101x010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convert101x011 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert101x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 (noMeta x5) (value x6)

convert101x1xx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert101x1xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 (value x5) x6

convert101x1x0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert101x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 (value x5) x6

convert101x1x1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert101x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 (value x5) x6

convert101x10x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert101x10x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convert101x100 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert101x100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convert101x101 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert101x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 (value x5) (noMeta x6)

convert101x11x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert101x11x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) x4 (value x5) (value x6)

convert101x110 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert101x110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) x4 (value x5) (value x6)

convert101x111 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert101x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) x4 (value x5) (value x6)

convert1010xxx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1010xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 x6

convert1010xx0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1010xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 x6

convert1010xx1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1010xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 x6

convert1010x0x :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1010x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert1010x00 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1010x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert1010x01 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1010x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert1010x1x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1010x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convert1010x10 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1010x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convert1010x11 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert1010x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) x5 (value x6)

convert10100xx :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert10100xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convert10100x0 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert10100x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convert10100x1 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert10100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) x6

convert101000x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert101000x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1010000 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> g
convert1010000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1010001 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1010001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert101001x :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert101001x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert1010010 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1010010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert1010011 :: (a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert1010011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert10101xx :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert10101xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convert10101x0 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert10101x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convert10101x1 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert10101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) x6

convert101010x :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert101010x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert1010100 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1010100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert1010101 :: (a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert1010101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert101011x :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert101011x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convert1010110 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert1010110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convert1010111 :: (a -> WithMeta b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1010111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (noMeta x4) (value x5) (value x6)

convert1011xxx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1011xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) x5 x6

convert1011xx0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1011xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) x5 x6

convert1011xx1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1011xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) x5 x6

convert1011x0x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1011x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convert1011x00 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1011x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convert1011x01 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1011x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) x5 (noMeta x6)

convert1011x1x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1011x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) x5 (value x6)

convert1011x10 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1011x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) x5 (value x6)

convert1011x11 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1011x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) x5 (value x6)

convert10110xx :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert10110xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convert10110x0 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert10110x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convert10110x1 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert10110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) x6

convert101100x :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert101100x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert1011000 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1011000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert1011001 :: (a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1011001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert101101x :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert101101x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convert1011010 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1011010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convert1011011 :: (a -> WithMeta b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1011011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) (noMeta x5) (value x6)

convert10111xx :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert10111xx f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) (value x5) x6

convert10111x0 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert10111x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) (value x5) x6

convert10111x1 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert10111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) (value x5) x6

convert101110x :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert101110x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convert1011100 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert1011100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convert1011101 :: (a -> WithMeta b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1011101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) (value x5) (noMeta x6)

convert101111x :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert101111x f x1 x2 x3 x4 x5 x6 = f (value x1) (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convert1011110 :: (a -> WithMeta b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1011110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convert1011111 :: (a -> WithMeta b -> c -> d -> e -> f -> g) -> WithMeta a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert1011111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (noMeta x2) (value x3) (value x4) (value x5) (value x6)

convert11xxxxx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11xxxxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 x5 x6

convert11xxxx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11xxxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 x5 x6

convert11xxxx1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11xxxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 x5 x6

convert11xxx0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11xxx0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 x5 (noMeta x6)

convert11xxx00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11xxx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 x5 (noMeta x6)

convert11xxx01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11xxx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 x5 (noMeta x6)

convert11xxx1x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert11xxx1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 x5 (value x6)

convert11xxx10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert11xxx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 x5 (value x6)

convert11xxx11 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert11xxx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 x5 (value x6)

convert11xx0xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11xx0xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 (noMeta x5) x6

convert11xx0x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11xx0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 (noMeta x5) x6

convert11xx0x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11xx0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 (noMeta x5) x6

convert11xx00x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11xx00x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 (noMeta x5) (noMeta x6)

convert11xx000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11xx000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 (noMeta x5) (noMeta x6)

convert11xx001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11xx001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 (noMeta x5) (noMeta x6)

convert11xx01x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert11xx01x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 (noMeta x5) (value x6)

convert11xx010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert11xx010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 (noMeta x5) (value x6)

convert11xx011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert11xx011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 (noMeta x5) (value x6)

convert11xx1xx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11xx1xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 (value x5) x6

convert11xx1x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11xx1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 (value x5) x6

convert11xx1x1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert11xx1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 (value x5) x6

convert11xx10x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11xx10x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 (value x5) (noMeta x6)

convert11xx100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11xx100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 (value x5) (noMeta x6)

convert11xx101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert11xx101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 (value x5) (noMeta x6)

convert11xx11x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert11xx11x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 x4 (value x5) (value x6)

convert11xx110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert11xx110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 x4 (value x5) (value x6)

convert11xx111 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert11xx111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 x4 (value x5) (value x6)

convert11x0xxx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11x0xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) x5 x6

convert11x0xx0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11x0xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) x5 x6

convert11x0xx1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11x0xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) x5 x6

convert11x0x0x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11x0x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) x5 (noMeta x6)

convert11x0x00 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11x0x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) x5 (noMeta x6)

convert11x0x01 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11x0x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) x5 (noMeta x6)

convert11x0x1x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert11x0x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) x5 (value x6)

convert11x0x10 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert11x0x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) x5 (value x6)

convert11x0x11 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert11x0x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) x5 (value x6)

convert11x00xx :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11x00xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) x6

convert11x00x0 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11x00x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) x6

convert11x00x1 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11x00x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) x6

convert11x000x :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11x000x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert11x0000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11x0000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert11x0001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11x0001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) (noMeta x6)

convert11x001x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert11x001x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert11x0010 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert11x0010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert11x0011 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert11x0011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) (noMeta x5) (value x6)

convert11x01xx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11x01xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) (value x5) x6

convert11x01x0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11x01x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) (value x5) x6

convert11x01x1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert11x01x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) (value x5) x6

convert11x010x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11x010x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert11x0100 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11x0100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert11x0101 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert11x0101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) (value x5) (noMeta x6)

convert11x011x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert11x011x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (noMeta x4) (value x5) (value x6)

convert11x0110 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert11x0110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (noMeta x4) (value x5) (value x6)

convert11x0111 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert11x0111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (noMeta x4) (value x5) (value x6)

convert11x1xxx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11x1xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) x5 x6

convert11x1xx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11x1xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) x5 x6

convert11x1xx1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert11x1xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) x5 x6

convert11x1x0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11x1x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) x5 (noMeta x6)

convert11x1x00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11x1x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) x5 (noMeta x6)

convert11x1x01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert11x1x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) x5 (noMeta x6)

convert11x1x1x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert11x1x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) x5 (value x6)

convert11x1x10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert11x1x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) x5 (value x6)

convert11x1x11 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert11x1x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) x5 (value x6)

convert11x10xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11x10xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) (noMeta x5) x6

convert11x10x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11x10x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) (noMeta x5) x6

convert11x10x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert11x10x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) (noMeta x5) x6

convert11x100x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11x100x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert11x1000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11x1000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert11x1001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert11x1001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) (noMeta x5) (noMeta x6)

convert11x101x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert11x101x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) (noMeta x5) (value x6)

convert11x1010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert11x1010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) (noMeta x5) (value x6)

convert11x1011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert11x1011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) (noMeta x5) (value x6)

convert11x11xx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert11x11xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) (value x5) x6

convert11x11x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert11x11x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) (value x5) x6

convert11x11x1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert11x11x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) (value x5) x6

convert11x110x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert11x110x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) (value x5) (noMeta x6)

convert11x1100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert11x1100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) (value x5) (noMeta x6)

convert11x1101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert11x1101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) (value x5) (noMeta x6)

convert11x111x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert11x111x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) x3 (value x4) (value x5) (value x6)

convert11x1110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert11x1110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) x3 (value x4) (value x5) (value x6)

convert11x1111 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert11x1111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) x3 (value x4) (value x5) (value x6)

convert110xxxx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110xxxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 x5 x6

convert110xxx0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 x5 x6

convert110xxx1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert110xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 x5 x6

convert110xx0x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110xx0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 x5 (noMeta x6)

convert110xx00 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110xx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 x5 (noMeta x6)

convert110xx01 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert110xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 x5 (noMeta x6)

convert110xx1x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert110xx1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 x5 (value x6)

convert110xx10 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert110xx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 x5 (value x6)

convert110xx11 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert110xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 x5 (value x6)

convert110x0xx :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110x0xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) x6

convert110x0x0 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) x6

convert110x0x1 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert110x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) x6

convert110x00x :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110x00x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert110x000 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110x000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert110x001 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert110x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) (noMeta x6)

convert110x01x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert110x01x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert110x010 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert110x010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert110x011 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert110x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 (noMeta x5) (value x6)

convert110x1xx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert110x1xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 (value x5) x6

convert110x1x0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert110x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 (value x5) x6

convert110x1x1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert110x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 (value x5) x6

convert110x10x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert110x10x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert110x100 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert110x100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert110x101 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert110x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 (value x5) (noMeta x6)

convert110x11x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert110x11x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) x4 (value x5) (value x6)

convert110x110 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert110x110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) x4 (value x5) (value x6)

convert110x111 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert110x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) x4 (value x5) (value x6)

convert1100xxx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert1100xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 x6

convert1100xx0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert1100xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 x6

convert1100xx1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert1100xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 x6

convert1100x0x :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert1100x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1100x00 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert1100x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1100x01 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert1100x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 (noMeta x6)

convert1100x1x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert1100x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert1100x10 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert1100x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert1100x11 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1100x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) x5 (value x6)

convert11000xx :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11000xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert11000x0 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert11000x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert11000x1 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert11000x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) x6

convert110000x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert110000x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1100000 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> g
convert1100000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1100001 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> f -> WithMeta g
convert1100001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert110001x :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert110001x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert1100010 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> g
convert1100010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert1100011 :: (a -> b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> e -> WithMeta f -> WithMeta g
convert1100011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (noMeta x5) (value x6)

convert11001xx :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11001xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convert11001x0 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert11001x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convert11001x1 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert11001x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) x6

convert110010x :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert110010x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert1100100 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> g
convert1100100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert1100101 :: (a -> b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> f -> WithMeta g
convert1100101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (noMeta x6)

convert110011x :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert110011x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1100110 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> g
convert1100110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1100111 :: (a -> b -> WithMeta c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1100111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (noMeta x4) (value x5) (value x6)

convert1101xxx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert1101xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) x5 x6

convert1101xx0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert1101xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) x5 x6

convert1101xx1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1101xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) x5 x6

convert1101x0x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert1101x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert1101x00 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert1101x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert1101x01 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1101x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) x5 (noMeta x6)

convert1101x1x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1101x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) x5 (value x6)

convert1101x10 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1101x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) x5 (value x6)

convert1101x11 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1101x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) x5 (value x6)

convert11010xx :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11010xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convert11010x0 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert11010x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convert11010x1 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert11010x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) x6

convert110100x :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert110100x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert1101000 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> g
convert1101000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert1101001 :: (a -> b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> f -> WithMeta g
convert1101001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (noMeta x6)

convert110101x :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert110101x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert1101010 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> g
convert1101010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert1101011 :: (a -> b -> WithMeta c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1101011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) (noMeta x5) (value x6)

convert11011xx :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert11011xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) (value x5) x6

convert11011x0 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert11011x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) (value x5) x6

convert11011x1 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert11011x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) (value x5) x6

convert110110x :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert110110x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert1101100 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> g
convert1101100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert1101101 :: (a -> b -> WithMeta c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1101101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) (value x5) (noMeta x6)

convert110111x :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert110111x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convert1101110 :: (a -> b -> WithMeta c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1101110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convert1101111 :: (a -> b -> WithMeta c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert1101111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (noMeta x3) (value x4) (value x5) (value x6)

convert111xxxx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111xxxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 x5 x6

convert111xxx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111xxx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 x5 x6

convert111xxx1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert111xxx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 x5 x6

convert111xx0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111xx0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 x5 (noMeta x6)

convert111xx00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111xx00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 x5 (noMeta x6)

convert111xx01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert111xx01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 x5 (noMeta x6)

convert111xx1x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert111xx1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 x5 (value x6)

convert111xx10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert111xx10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 x5 (value x6)

convert111xx11 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert111xx11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 x5 (value x6)

convert111x0xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111x0xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 (noMeta x5) x6

convert111x0x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111x0x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 (noMeta x5) x6

convert111x0x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert111x0x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 (noMeta x5) x6

convert111x00x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111x00x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert111x000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111x000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert111x001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert111x001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 (noMeta x5) (noMeta x6)

convert111x01x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert111x01x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 (noMeta x5) (value x6)

convert111x010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert111x010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 (noMeta x5) (value x6)

convert111x011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert111x011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 (noMeta x5) (value x6)

convert111x1xx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert111x1xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 (value x5) x6

convert111x1x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert111x1x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 (value x5) x6

convert111x1x1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert111x1x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 (value x5) x6

convert111x10x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert111x10x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 (value x5) (noMeta x6)

convert111x100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert111x100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 (value x5) (noMeta x6)

convert111x101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert111x101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 (value x5) (noMeta x6)

convert111x11x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert111x11x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) x4 (value x5) (value x6)

convert111x110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert111x110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) x4 (value x5) (value x6)

convert111x111 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert111x111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) x4 (value x5) (value x6)

convert1110xxx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert1110xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) x5 x6

convert1110xx0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert1110xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) x5 x6

convert1110xx1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1110xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) x5 x6

convert1110x0x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert1110x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert1110x00 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert1110x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert1110x01 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1110x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) x5 (noMeta x6)

convert1110x1x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1110x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) x5 (value x6)

convert1110x10 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1110x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) x5 (value x6)

convert1110x11 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert1110x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) x5 (value x6)

convert11100xx :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert11100xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convert11100x0 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert11100x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convert11100x1 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert11100x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) x6

convert111000x :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert111000x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1110000 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> g
convert1110000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert1110001 :: (a -> b -> c -> WithMeta d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> f -> WithMeta g
convert1110001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (noMeta x6)

convert111001x :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert111001x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert1110010 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> g
convert1110010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert1110011 :: (a -> b -> c -> WithMeta d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> e -> WithMeta f -> WithMeta g
convert1110011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) (noMeta x5) (value x6)

convert11101xx :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert11101xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) (value x5) x6

convert11101x0 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert11101x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) (value x5) x6

convert11101x1 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert11101x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) (value x5) x6

convert111010x :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert111010x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert1110100 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> g
convert1110100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert1110101 :: (a -> b -> c -> WithMeta d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> f -> WithMeta g
convert1110101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) (value x5) (noMeta x6)

convert111011x :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert111011x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convert1110110 :: (a -> b -> c -> WithMeta d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> g
convert1110110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convert1110111 :: (a -> b -> c -> WithMeta d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> d -> WithMeta e -> WithMeta f -> WithMeta g
convert1110111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (noMeta x4) (value x5) (value x6)

convert1111xxx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1111xxx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) x5 x6

convert1111xx0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1111xx0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) x5 x6

convert1111xx1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1111xx1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) x5 x6

convert1111x0x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1111x0x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) x5 (noMeta x6)

convert1111x00 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1111x00 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) x5 (noMeta x6)

convert1111x01 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1111x01 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) x5 (noMeta x6)

convert1111x1x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1111x1x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) x5 (value x6)

convert1111x10 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1111x10 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) x5 (value x6)

convert1111x11 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1111x11 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) x5 (value x6)

convert11110xx :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert11110xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) (noMeta x5) x6

convert11110x0 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert11110x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) (noMeta x5) x6

convert11110x1 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert11110x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) (noMeta x5) x6

convert111100x :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert111100x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert1111000 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> g
convert1111000 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert1111001 :: (a -> b -> c -> d -> WithMeta e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> f -> WithMeta g
convert1111001 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) (noMeta x5) (noMeta x6)

convert111101x :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert111101x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convert1111010 :: (a -> b -> c -> d -> WithMeta e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> g
convert1111010 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convert1111011 :: (a -> b -> c -> d -> WithMeta e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> e -> WithMeta f -> WithMeta g
convert1111011 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) (noMeta x5) (value x6)

convert11111xx :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert11111xx f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) (value x5) x6

convert11111x0 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert11111x0 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) (value x5) x6

convert11111x1 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert11111x1 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) (value x5) x6

convert111110x :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert111110x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convert1111100 :: (a -> b -> c -> d -> e -> WithMeta f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> g
convert1111100 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convert1111101 :: (a -> b -> c -> d -> e -> WithMeta f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> f -> WithMeta g
convert1111101 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) (value x5) (noMeta x6)

convert111111x :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert111111x f x1 x2 x3 x4 x5 x6 = f (value x1) (value x2) (value x3) (value x4) (value x5) (value x6)

convert1111110 :: (a -> b -> c -> d -> e -> f -> WithMeta g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> g
convert1111110 f x1 x2 x3 x4 x5 x6 = value $ f (value x1) (value x2) (value x3) (value x4) (value x5) (value x6)

convert1111111 :: (a -> b -> c -> d -> e -> f -> g) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f -> WithMeta g
convert1111111 f x1 x2 x3 x4 x5 x6 = noMeta $ f (value x1) (value x2) (value x3) (value x4) (value x5) (value x6)
