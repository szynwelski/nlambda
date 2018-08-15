{-# LANGUAGE KindSignatures, DefaultSignatures, FlexibleContexts, TypeOperators, RankNTypes #-}

module Meta where

import Data.Char (toLower)
import Data.Foldable (fold, foldl', foldr', toList)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Ratio (Ratio)
import GHC.Generics
import GHC.Read (readPrec, readListPrec)
import GHC.Show (showList__)
import Var
import Text.ParserCombinators.ReadPrec (ReadPrec)

------------------------------------------------------------------------------------------
-- Meta information
------------------------------------------------------------------------------------------

data Meta = Meta {toRename :: IdMap, renamed :: Set (Identifier, Identifier), renameTree :: RenameTree} deriving Show

metaFromMap :: IdMap -> Meta
metaFromMap map = Meta map Set.empty Empty

emptyMeta :: Meta
emptyMeta = metaFromMap Map.empty

------------------------------------------------------------------------------------------
-- Class WithMeta and instances
------------------------------------------------------------------------------------------

data WithMeta a = WithMeta {value :: a, meta :: Meta}

instance (Var a, Show a) => Show (WithMeta a) where
    showsPrec n = noMetaResOp $ showsPrec n
    show (WithMeta x m) = show x ++ " | meta:" ++ show m
    showList = showList . value . lift

instance (Monoid a, Var a) => Monoid (WithMeta a) where
    mempty = noMeta mempty
    mappend = renameAndApply2 mappend
    mconcat = idOp mconcat . lift

noMeta :: a -> WithMeta a
noMeta x = WithMeta x emptyMeta

create :: a -> Meta -> WithMeta a
create x m = WithMeta x m

------------------------------------------------------------------------------------------
-- Rename methods
------------------------------------------------------------------------------------------

union :: Bool -> Meta -> Meta -> Meta
union join (Meta map1 set1 tree1) (Meta map2 set2 tree2)
    = Meta (Map.union (Map.difference map1 toRenameNow1) (Map.difference map2 toRenameNow2))
           (Set.unions [set1, set2, Set.fromList $ Map.assocs toRenameNow1, Set.fromList $ Map.assocs toRenameNow2])
           (createNode (not join) children)
    where partitionByRenamed map set = Map.partitionWithKey (\k v -> Set.member (k,v) set) map
          findConflicts mapToRestrict map = Map.filterWithKey (\k v -> Map.findWithDefault v k map /= v) mapToRestrict
          (inRenamed1, notInRenamed1) = partitionByRenamed map1 set2
          (inRenamed2, notInRenamed2) = partitionByRenamed map2 set1
          (conflicts1, conflicts2) = (findConflicts notInRenamed1 notInRenamed2, findConflicts notInRenamed2 notInRenamed1)
          (toRenameNow1, toRenameNow2) = if Map.null inRenamed1
                                         then (Map.empty, Map.union inRenamed2 conflicts2)
                                         else (Map.union inRenamed1 conflicts1, inRenamed2)
          tree1' = addMapToTree toRenameNow1 tree1
          tree2' = addMapToTree toRenameNow2 tree2
          children = if join then getChildrenOrNode tree1' ++ getChildrenOrNode tree2' else [tree1', tree2']

unions :: [Meta] -> Meta
unions [] = emptyMeta
unions ms = foldr1 (union True) ms

rename :: Var a => WithMeta a -> WithMeta a
rename (WithMeta x m)
    | isTreeEmpty tree = create x m'
    | otherwise = create (renameVariables tree x) m'
    where tree = renameTree m
          m' = m {renameTree = Empty}

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

renameAndApply2 :: (Var a, Var b) => (a -> b -> c) -> WithMeta a -> WithMeta b -> WithMeta c
renameAndApply2 f (WithMeta x1 m1) (WithMeta x2 m2) = create (f x1' x2') m
    where (WithMeta (x1',x2') m) = rename $ create (x1,x2) (unions [m1,m2])

renameAndApply3 :: (Var a, Var b, Var c) => (a -> b -> c -> d) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d
renameAndApply3 f (WithMeta x1 m1) (WithMeta x2 m2) (WithMeta x3 m3) = create (f x1' x2' x3') m
    where (WithMeta (x1',x2',x3') m) = rename $ create (x1,x2,x3) (unions [m1,m2,m3])

renameAndApply4 :: (Var a, Var b, Var c, Var d) => (a -> b -> c -> d -> e) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e
renameAndApply4 f (WithMeta x1 m1) (WithMeta x2 m2) (WithMeta x3 m3) (WithMeta x4 m4) = create (f x1' x2' x3' x4') m
    where (WithMeta (x1',x2',x3',x4') m) = rename $ create (x1,x2,x3,x4) (unions [m1,m2,m3,m4])

renameAndApply5 :: (Var a, Var b, Var c, Var d, Var e) => (a -> b -> c -> d -> e -> f) -> WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d -> WithMeta e -> WithMeta f
renameAndApply5 f (WithMeta x1 m1) (WithMeta x2 m2) (WithMeta x3 m3) (WithMeta x4 m4) (WithMeta x5 m5) = create (f x1' x2' x3' x4' x5') m
    where (WithMeta (x1',x2',x3',x4',x5') m) = rename $ create (x1,x2,x3,x4,x5) (unions [m1,m2,m3,m4,m5])

noMetaRes2ArgOp :: (Var a, Var b) => (a -> b -> c) -> WithMeta a -> WithMeta b -> c
noMetaRes2ArgOp op x = value . renameAndApply2 op x

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
    liftMeta (Rec1 x) = let (WithMeta y m) = liftMeta x in WithMeta (Rec1 y) (addTreeLevel m)
        where addTreeLevel m = m {renameTree = createNode False [renameTree m]}
    dropMeta (WithMeta (Rec1 x) m) = Rec1 (dropMeta (WithMeta x m))

instance MetaLevel U1 where
    liftMeta U1 = noMeta U1
    dropMeta (WithMeta U1 _) = U1

instance MetaLevel (K1 i a) where
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
                         in WithMeta (x' :*: y') (union True m1 m2)
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


lift :: (MetaLevel f, Var (f a)) => f (WithMeta a) -> WithMeta (f a)
lift = rename . liftMeta

------------------------------------------------------------------------------------------
-- Meta classes from Prelude
------------------------------------------------------------------------------------------

class (Applicative f, Functor_nlambda f) => Applicative_nlambda (f :: * -> *) where
  pure_nlambda :: WithMeta a -> WithMeta (f a)
  pure_nlambda = idOp pure
  (<*>###) :: (Var (f (WithMeta a -> WithMeta b)), Var (f a), Var (f b)) => WithMeta (f (WithMeta a -> WithMeta b)) -> WithMeta (f a) -> WithMeta (f b)
  (<*>###) f x = let (WithMeta (f', x') m) = renameAndApply2 (,) f x in lift (fmap (metaFun m) f' <*> x')
  (*>###) :: (Var (f a), Var (f b)) => WithMeta (f a) -> WithMeta (f b) -> WithMeta (f b)
  (*>###) = renameAndApply2 (*>)
  (<*###) :: (Var (f a), Var (f b)) => WithMeta (f a) -> WithMeta (f b) -> WithMeta (f a)
  (<*###) = renameAndApply2 (<*)

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

class (Var a, Enum a) => Enum_nlambda a where
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
  enumFromThen_nlambda = renameAndApply2 enumFromThen
  enumFromTo_nlambda :: WithMeta a -> WithMeta a -> WithMeta [a]
  enumFromTo_nlambda = renameAndApply2 enumFromTo
  enumFromThenTo_nlambda :: WithMeta a -> WithMeta a -> WithMeta a -> WithMeta [a]
  enumFromThenTo_nlambda = renameAndApply3 enumFromThenTo

instance Enum_nlambda Word -- Defined in ‘GHC.Enum’
instance Enum_nlambda Ordering -- Defined in ‘GHC.Enum’
instance Enum_nlambda Integer -- Defined in ‘GHC.Enum’
instance Enum_nlambda Int -- Defined in ‘GHC.Enum’
instance Enum_nlambda Char -- Defined in ‘GHC.Enum’
instance Enum_nlambda Bool -- Defined in ‘GHC.Enum’
instance Enum_nlambda () -- Defined in ‘GHC.Enum’
instance Enum_nlambda Float -- Defined in ‘GHC.Float’
instance Enum_nlambda Double -- Defined in ‘GHC.Float’

class (Var a, Eq a) => Eq_nlambda a where
  (==###) :: WithMeta a -> WithMeta a -> Bool
  (==###) = noMetaRes2ArgOp (==)
  (/=###) :: WithMeta a -> WithMeta a -> Bool
  (/=###) = noMetaRes2ArgOp (/=)

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
instance Eq_nlambda Variable

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
  (**###) = renameAndApply2 (**)
  logBase_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  logBase_nlambda = renameAndApply2 logBase
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
  elem_nlambda :: (Var (t a), Eq_nlambda a) => WithMeta a -> WithMeta (t a) -> Bool
  elem_nlambda = noMetaRes2ArgOp elem
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
  (/###) = renameAndApply2 (/)
  recip_nlambda :: WithMeta a -> WithMeta a
  recip_nlambda = idOp recip
  fromRational_nlambda :: Rational -> WithMeta a
  fromRational_nlambda = noMeta . fromRational

instance Fractional_nlambda Float -- Defined in ‘GHC.Float’
instance Fractional_nlambda Double -- Defined in ‘GHC.Float’

class (MetaLevel f, Functor f) => Functor_nlambda (f :: * -> *) where
  fmap_nlambda :: Var (f b) => (WithMeta a -> WithMeta b) -> WithMeta (f a) -> WithMeta (f b)
  fmap_nlambda = lift .* metaFunOp fmap
  (<$###) :: (Var a, Var (f b)) => WithMeta a -> WithMeta (f b) -> WithMeta (f a)
  (<$###) = renameAndApply2 (<$)

instance Functor_nlambda (Either a) -- Defined in ‘Data.Either’
instance Functor_nlambda [] -- Defined in ‘GHC.Base’
instance Functor_nlambda Maybe -- Defined in ‘GHC.Base’
instance Functor_nlambda IO -- Defined in ‘GHC.Base’
instance Functor_nlambda ((->) r) -- Defined in ‘GHC.Base’
instance Functor_nlambda ((,) a) -- Defined in ‘GHC.Base’

class (Integral a, Real_nlambda a, Enum_nlambda a) => Integral_nlambda a where
  quot_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  quot_nlambda = renameAndApply2 quot
  rem_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  rem_nlambda = renameAndApply2 rem
  div_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  div_nlambda = renameAndApply2 div
  mod_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  mod_nlambda = renameAndApply2 mod
  quotRem_nlambda :: WithMeta a -> WithMeta a -> WithMeta (a, a)
  quotRem_nlambda = renameAndApply2 quotRem
  divMod_nlambda :: WithMeta a -> WithMeta a -> WithMeta (a, a)
  divMod_nlambda = renameAndApply2 divMod
  toInteger_nlambda :: WithMeta a -> Integer
  toInteger_nlambda = noMetaResOp toInteger

instance Integral_nlambda Word -- Defined in ‘GHC.Real’
instance Integral_nlambda Integer -- Defined in ‘GHC.Real’
instance Integral_nlambda Int -- Defined in ‘GHC.Real’

class (Monad m, Applicative_nlambda m) => Monad_nlambda (m :: * -> *) where
  (>>=###) :: Var (m b) => WithMeta (m a) -> (WithMeta a -> WithMeta (m b)) -> WithMeta (m b)
  (>>=###) (WithMeta x m) f = lift $ x >>= (dropMeta . metaFun m f)
  (>>###) :: (Var (m a), Var (m b)) => WithMeta (m a) -> WithMeta (m b) -> WithMeta (m b)
  (>>###) = renameAndApply2 (>>)
  return_nlambda :: WithMeta a -> WithMeta (m a)
  return_nlambda = idOp return
  fail_nlambda :: String -> WithMeta (m a)
  fail_nlambda = noMeta . fail

instance Monad_nlambda (Either e) -- Defined in ‘Data.Either’
instance Monad_nlambda [] -- Defined in ‘GHC.Base’
instance Monad_nlambda Maybe -- Defined in ‘GHC.Base’
instance Monad_nlambda IO -- Defined in ‘GHC.Base’
instance Monad_nlambda ((->) r) -- Defined in ‘GHC.Base’

class (Var a, Monoid a) => Monoid_nlambda a where
  mempty_nlambda :: WithMeta a
  mempty_nlambda = noMeta mempty
  mappend_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  mappend_nlambda = renameAndApply2 mappend
  mconcat_nlambda :: WithMeta [a] -> WithMeta a
  mconcat_nlambda = idOp mconcat

instance Var a => Monoid_nlambda [a] -- Defined in ‘GHC.Base’
instance Monoid_nlambda Ordering -- Defined in ‘GHC.Base’
instance Monoid_nlambda a => Monoid_nlambda (Maybe a) -- Defined in ‘GHC.Base’
instance Monoid_nlambda b => Monoid_nlambda (a -> b) -- Defined in ‘GHC.Base’
instance Monoid_nlambda () -- Defined in ‘GHC.Base’
instance (Monoid_nlambda a, Monoid_nlambda b) => Monoid_nlambda (a, b)  -- Defined in ‘GHC.Base’
instance (Monoid_nlambda a, Monoid_nlambda b, Monoid_nlambda c) => Monoid_nlambda (a, b, c)  -- Defined in ‘GHC.Base’

class (Var a, Num a) => Num_nlambda a where
  (+###) :: WithMeta a -> WithMeta a -> WithMeta a
  (+###) = renameAndApply2 (+)
  (-###) :: WithMeta a -> WithMeta a -> WithMeta a
  (-###) = renameAndApply2 (-)
  (*###) :: WithMeta a -> WithMeta a -> WithMeta a
  (*###) = renameAndApply2 (*)
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
  compare_nlambda = noMetaRes2ArgOp compare
  (<###) :: WithMeta a -> WithMeta a -> Bool
  (<###) = noMetaRes2ArgOp (<)
  (<=###) :: WithMeta a -> WithMeta a -> Bool
  (<=###) = noMetaRes2ArgOp (<=)
  (>###) :: WithMeta a -> WithMeta a -> Bool
  (>###) = noMetaRes2ArgOp (>)
  (>=###) :: WithMeta a -> WithMeta a -> Bool
  (>=###) = noMetaRes2ArgOp (>=)
  max_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  max_nlambda = renameAndApply2 max
  min_nlambda :: WithMeta a -> WithMeta a -> WithMeta a
  min_nlambda = renameAndApply2 min

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
instance Ord_nlambda Variable

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
  atan2_nlambda = renameAndApply2 atan2

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
instance (Show_nlambda a, Show_nlambda b, Show_nlambda c, Show_nlambda d) => Show_nlambda (a, b, c, d)  -- Defined in ‘GHC.Show’
instance (Show_nlambda a, Show_nlambda b, Show_nlambda c, Show_nlambda d, Show_nlambda e) => Show_nlambda (a, b, c, d, e)  -- Defined in ‘GHC.Show’
instance (Show_nlambda a, Show_nlambda b) => Show_nlambda (Either a b)  -- Defined in ‘Data.Either’
instance Show_nlambda Float -- Defined in ‘GHC.Float’
instance Show_nlambda Double -- Defined in ‘GHC.Float’
instance Show_nlambda Variable -- Defined in ‘Var’

class (Traversable t, Functor_nlambda t, Foldable_nlambda t) => Traversable_nlambda (t :: * -> *) where
  traverse_nlambda :: (Var (t b), Var (f (t b)), Applicative_nlambda f) => (WithMeta a -> WithMeta (f b)) -> WithMeta (t a) -> WithMeta (f (t b))
  traverse_nlambda f (WithMeta x m) = lift $ fmap lift $ traverse (dropMeta . metaFun m f) x
  sequenceA_nlambda :: Applicative_nlambda f => WithMeta (t (f a)) -> WithMeta (f (t a))
  sequenceA_nlambda = idOp sequenceA
  mapM_nlambda :: (Var (t b), Var (m (t b)), Monad_nlambda m) => (WithMeta a -> WithMeta (m b)) -> WithMeta (t a) -> WithMeta (m (t b))
  mapM_nlambda f (WithMeta x m) = lift $ fmap lift $ mapM (dropMeta . metaFun m f) x
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

-- TODO ConvertFun could be selected automatically by type
data ConvertFun = NoMeta | IdOp | NoMetaArgOp | NoMetaResOp | LeftIdOp | RightIdOp
    | RenameAndApply2 | RenameAndApply3 | RenameAndApply4 | RenameAndApply5
    | NoMetaRes2ArgOp | MetaFunOp | NoMetaResFunOp deriving (Show, Eq, Ord)

convertFunName :: ConvertFun -> String
convertFunName fun = (toLower $ head $ show fun) : (tail $ show fun)

data MetaEquivalentType = SameOp | ConvertFun ConvertFun | CustomFun String deriving (Eq, Ord)

data MetaEquivalent = NoEquivalent | MetaFun String | MetaConvertFun String | OrigFun deriving Show

type ModuleName = String
type MethodName = String
type MetaEquivalentMap = Map ModuleName (Map MetaEquivalentType [MethodName])

createEquivalentsMap :: ModuleName -> [(MetaEquivalentType, [MethodName])] -> MetaEquivalentMap
createEquivalentsMap mod = Map.singleton mod . Map.fromList

preludeEquivalents :: Map ModuleName (Map MetaEquivalentType [MethodName])
preludeEquivalents = Map.unions [nominalVar, ghcBase, ghcClasses, ghcEnum, ghcErr, ghcFloat, ghcList, ghcNum, ghcPrim, ghcReal, ghcShow, ghcTuple, ghcTypes,
                                 dataEither, dataFoldable, dataMaybe, dataSetBase, dataTuple, controlExceptionBase]

preludeModules :: [ModuleName]
preludeModules = Map.keys preludeEquivalents

metaEquivalent :: ModuleName -> MethodName -> MetaEquivalent
metaEquivalent mod name | isSuffixOf "#" name = OrigFun
metaEquivalent mod name = case maybe Nothing findMethod $ Map.lookup mod preludeEquivalents of
                            Just SameOp -> OrigFun
                            Just (ConvertFun fun) -> MetaConvertFun (convertFunName fun)
                            Just (CustomFun fun) -> MetaFun fun
                            Nothing -> NoEquivalent
    where findMethod = maybe Nothing (Just . fst . fst) . Map.minViewWithKey . Map.filter (elem name)

----------------------------------------------------------------------------------------
-- Meta equivalents methods
----------------------------------------------------------------------------------------

nominalVar :: MetaEquivalentMap
nominalVar = createEquivalentsMap "Var" []

ghcBase :: MetaEquivalentMap
ghcBase = createEquivalentsMap "GHC.Base"
    [(SameOp, ["$", "$!", ".", "const", "flip", "id"]),
     (ConvertFun NoMeta, ["Nothing"]),
     (ConvertFun IdOp, ["Just"]),
     (ConvertFun RenameAndApply2, ["++", "$dm<$", "$dm<*", "$dm*>", "$dm>>"])]

map_nlambda :: Var b => (WithMeta a -> WithMeta b) -> WithMeta [a] -> WithMeta [b]
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
    [(ConvertFun LeftIdOp, ["!!"]),
     (ConvertFun IdOp, ["cycle", "head", "init", "last", "repeat", "reverse", "tail", "unzip", "unzip3"]),
     (ConvertFun RightIdOp, ["drop", "replicate", "splitAt", "take"]),
     (ConvertFun RenameAndApply2, ["lookup", "zip"]),
     (ConvertFun RenameAndApply3, ["zip3"]),
     (ConvertFun NoMetaResFunOp, ["dropWhile", "filter", "span", "takeWhile"])]

scanl_nlambda :: Var b => (WithMeta b -> WithMeta a -> WithMeta b) -> WithMeta b -> WithMeta [a] -> WithMeta [b]
scanl_nlambda f x = lift . scanl f x . dropMeta

scanl1_nlambda :: Var a => (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta [a] -> WithMeta [a]
scanl1_nlambda f = lift . scanl1 f . dropMeta

scanr_nlambda :: Var b => (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta [a] -> WithMeta [b]
scanr_nlambda f x = lift . scanr f x . dropMeta

scanr1_nlambda :: Var a => (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta [a] -> WithMeta [a]
scanr1_nlambda f = lift . scanr1 f . dropMeta

ghcNum :: MetaEquivalentMap
ghcNum = createEquivalentsMap "GHC.Num"
    [(ConvertFun RenameAndApply2, ["$dm-"])]

ghcPrim :: MetaEquivalentMap
ghcPrim = createEquivalentsMap "GHC.Prim"
    [(ConvertFun RightIdOp, ["seq"])]

ghcReal :: MetaEquivalentMap
ghcReal = createEquivalentsMap "GHC.Real"
    [(ConvertFun RenameAndApply2, ["^", "^^"]),
     (ConvertFun NoMetaResOp, ["even", "odd"])]

ghcShow :: MetaEquivalentMap
ghcShow = createEquivalentsMap "GHC.Show" []

ghcTuple :: MetaEquivalentMap
ghcTuple = createEquivalentsMap "GHC.Tuple"
    [(ConvertFun RenameAndApply2, ["(,)"]),
     (ConvertFun RenameAndApply3, ["(,,)"]),
     (ConvertFun RenameAndApply4, ["(,,,)"]),
     (ConvertFun RenameAndApply5, ["(,,,,)"])]

ghcTypes :: MetaEquivalentMap
ghcTypes = createEquivalentsMap "GHC.Types"
    [(ConvertFun NoMeta, ["[]"]),
     (ConvertFun RenameAndApply2, [":"])]

dataEither :: MetaEquivalentMap
dataEither = createEquivalentsMap "Data.Either"
    [(ConvertFun IdOp, ["Left", "Right"]),
     (ConvertFun NoMetaResOp, ["isLeft", "isRight"])]

either_nlambda :: (WithMeta a -> WithMeta c) -> (WithMeta b -> WithMeta c) -> WithMeta (Either a b) -> WithMeta c
either_nlambda f1 f2 (WithMeta e m) = either (metaFun m f1) (metaFun m f2) e

dataFoldable :: MetaEquivalentMap
dataFoldable = createEquivalentsMap "Data.Foldable"
    [(ConvertFun NoMetaResFunOp, ["all", "any"]),
     (ConvertFun IdOp, ["concat", "notElem"])]

concatMap_nlambda :: (Var b, Foldable_nlambda t) => (WithMeta a -> WithMeta [b]) -> WithMeta (t a) -> WithMeta [b]
concatMap_nlambda f = lift . concatMap (dropMeta . f) . dropMeta

dataMaybe :: MetaEquivalentMap
dataMaybe = createEquivalentsMap "Data.Maybe"
    [(ConvertFun NoMetaResOp, ["isJust", "isNothing"]),
     (ConvertFun IdOp, ["catMaybes", "fromJust", "listToMaybe", "maybeToList"]),
     (ConvertFun RenameAndApply2, ["fromMaybe"])]

dataSetBase :: MetaEquivalentMap
dataSetBase = createEquivalentsMap "Data.Set.Base"
    [(ConvertFun NoMeta, ["empty"]),
     (ConvertFun NoMetaResOp, ["null", "size"]),
     (ConvertFun IdOp, ["delete", "deleteMax", "deleteMin", "elems", "findMax", "findMin", "fromAscList", "fromDescList",
                        "fromDistinctAscList", "fromDistinctDescList", "fromList", "insert", "singleton", "toAscList", "toDescList", "toList", "unions"]),
     (ConvertFun RenameAndApply2, ["difference", "intersection", "split", "union"]),
     (ConvertFun NoMetaRes2ArgOp, ["disjoint", "member", "notMember", "isProperSubsetOf", "isSubsetOf"]),
     (ConvertFun NoMetaResFunOp, ["filter", "partition"]),
     (CustomFun "set_map_nlambda", ["map"]),
     (CustomFun "set_foldr_nlambda", ["foldr"]),
     (CustomFun "set_foldl_nlambda", ["foldl"])]

set_map_nlambda :: (Var b, Ord b) => (WithMeta a -> WithMeta b) -> WithMeta (Set a) -> WithMeta (Set b)
set_map_nlambda f (WithMeta s m) = create (Set.fromAscList list) m'
    where (WithMeta list m') = map_nlambda f $ create (Set.toAscList s) m

set_foldr_nlambda :: (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta (Set a) -> WithMeta b
set_foldr_nlambda f x (WithMeta s m) = foldr_nlambda f x $ create (Set.toAscList s) m

set_foldl_nlambda :: (WithMeta a -> WithMeta b -> WithMeta a) -> WithMeta a -> WithMeta (Set b) -> WithMeta a
set_foldl_nlambda f x (WithMeta s m) = foldl_nlambda f x $ create (Set.toAscList s) m

dataTuple :: MetaEquivalentMap
dataTuple = createEquivalentsMap "Data.Tuple"
    [(ConvertFun IdOp, ["fst", "snd"])]

curry_nlambda :: (Var a, Var b) => (WithMeta (a, b) -> WithMeta c) -> WithMeta a -> WithMeta b -> WithMeta c
curry_nlambda f x y = f $ renameAndApply2 (,) x y

uncurry_nlambda :: (WithMeta a -> WithMeta b -> WithMeta c) -> WithMeta (a, b) -> WithMeta c
uncurry_nlambda f p = f (idOp fst p) (idOp snd p)

controlExceptionBase = createEquivalentsMap "Control.Exception.Base"
    [(SameOp, ["patError", "noMethodBindingError"])]
