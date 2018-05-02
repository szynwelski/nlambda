{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module MetaPrelude where

import Meta

---------------------------------------------------------------
-- Auxiliary functions
---------------------------------------------------------------

instance Monoid a => Monoid (WithMeta a) where
    mempty = empty mempty
    mappend = unionOp mappend

idOp :: (a -> b) -> WithMeta a -> WithMeta b
idOp op (WithMeta x m) = WithMeta (op x) m

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
    where u = union [m1, m2]
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

metaFunOpMeta :: ((a -> b) -> c -> d) -> (WithMeta a -> b) -> WithMeta c -> WithMeta d
metaFunOpMeta op f (WithMeta x m) = WithMeta (op (metaFun m f) x) m

---------------------------------------------------------------
-- Prelude functions
---------------------------------------------------------------

nlambda_pair :: WithMeta a -> WithMeta b -> WithMeta (a,b)
nlambda_pair = unionOp (,)

(!!###) :: WithMeta [a] -> Int -> WithMeta a
(!!###) = leftIdOp (!!)

($###) :: (WithMeta a -> WithMeta b) -> WithMeta a -> WithMeta b
($###) = ($)

($!###) :: (WithMeta a -> WithMeta b) -> WithMeta a -> WithMeta b
($!###) = ($!)

(&&###) :: Bool -> Bool -> Bool
(&&###) = (&&)

(*###) :: Num a => WithMeta a -> WithMeta a -> WithMeta a
(*###) = unionOp (*)

(**###) :: Floating a => WithMeta a -> WithMeta a -> WithMeta a
(**###) = unionOp (**)

(*>###) :: Applicative f => WithMeta (f a) -> WithMeta (f b) -> WithMeta (f b)
(*>###) = unionOp (*>)

(+###) :: Num a => WithMeta a -> WithMeta a -> WithMeta a
(+###) = unionOp (+)

(++###) :: WithMeta [a] -> WithMeta [a] -> WithMeta [a]
(++###) = unionOp (++)

(-###) :: Num a => WithMeta a -> WithMeta a -> WithMeta a
(-###) = unionOp (-)

(.###) :: (WithMeta b -> WithMeta c) -> (WithMeta a -> WithMeta b) -> WithMeta a -> WithMeta c
(.###) = (.)

(/###) :: Fractional a => WithMeta a -> WithMeta a -> WithMeta a
(/###) = unionOp (/)

(/=###) :: Eq a => WithMeta a -> WithMeta a -> Bool
(/=###) = noMetaResUnionOp (/=)

(<###) :: Ord a => WithMeta a -> WithMeta a -> Bool
(<###) = noMetaResUnionOp (<)

(<$###) :: Functor f => WithMeta a -> WithMeta (f b) -> WithMeta (f a)
(<$###) = unionOp (<$)

(<$>###) :: (Functor f, MetaLevel f) => (WithMeta a -> WithMeta b) -> WithMeta (f a) -> WithMeta (f b)
(<$>###) = liftMeta .* metaFunOp (<$>)

(<*###) :: Applicative f => WithMeta (f a) -> WithMeta (f b) -> WithMeta (f a)
(<*###) = unionOp (<*)

(<*>###) :: Applicative f => WithMeta (f (a -> b)) -> WithMeta (f a) -> WithMeta (f b)
(<*>###) = unionOp (<*>)

(<=###) :: Ord a => WithMeta a -> WithMeta a -> Bool
(<=###) = noMetaResUnionOp (<=)

(=<<###) :: (Monad m, MetaLevel m) => (WithMeta a -> WithMeta (m b)) -> WithMeta (m a) -> WithMeta (m b)
f =<<### x = x >>=### f

(==###) :: Eq a => WithMeta a -> WithMeta a -> Bool
(==###) = noMetaResUnionOp (==)

(>###) :: Ord a => WithMeta a -> WithMeta a -> Bool
(>###) = noMetaResUnionOp (>)

(>=###) :: Ord a => WithMeta a -> WithMeta a -> Bool
(>=###) = noMetaResUnionOp (>=)

(>>###) :: Monad m => WithMeta (m a) -> WithMeta (m b) -> WithMeta (m b)
(>>###) = unionOp (>>)

(>>=###) :: (Monad m, MetaLevel m) => WithMeta (m a) -> (WithMeta a -> WithMeta (m b)) -> WithMeta (m b)
(>>=###) (WithMeta x m) f = liftMeta $ x >>= (dropMeta . metaFun m f)

(^###) :: (Num a, Integral b) => WithMeta a -> WithMeta b -> WithMeta a
(^###) = unionOp (^)

(^^###) :: (Fractional a, Integral b) => WithMeta a -> WithMeta b -> WithMeta a
(^^###) = unionOp (^^)

nlambda_Nothing :: WithMeta (Maybe a)
nlambda_Nothing = empty Nothing

nlambda_Just :: WithMeta a -> WithMeta (Maybe a)
nlambda_Just = idOp Just

nlambda_Left :: WithMeta a -> WithMeta (Either a b)
nlambda_Left = idOp Left

nlambda_Right :: WithMeta b -> WithMeta (Either a b)
nlambda_Right = idOp Right

nlambda_abs :: Num a => WithMeta a -> WithMeta a
nlambda_abs = idOp abs

nlambda_acos :: Floating a => WithMeta a -> WithMeta a
nlambda_acos = idOp acos

nlambda_acosh :: Floating a => WithMeta a -> WithMeta a
nlambda_acosh = idOp acosh

nlambda_all :: Foldable t => (WithMeta a -> Bool) -> WithMeta (t a) -> Bool
nlambda_all = metaFunOp all

nlambda_and :: Foldable t => WithMeta (t Bool) -> Bool
nlambda_and = noMetaResOp and

nlambda_any :: Foldable t => (WithMeta a -> Bool) -> WithMeta (t a) -> Bool
nlambda_any = metaFunOp any

nlambda_asTypeOf :: WithMeta a -> WithMeta a -> WithMeta a
nlambda_asTypeOf = asTypeOf

nlambda_asin :: Floating a => WithMeta a -> WithMeta a
nlambda_asin = idOp asin

nlambda_asinh :: Floating a => WithMeta a -> WithMeta a
nlambda_asinh = idOp asinh

nlambda_atan :: Floating a => WithMeta a -> WithMeta a
nlambda_atan = idOp atan

nlambda_atan2 :: RealFloat a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_atan2 = unionOp atan2

nlambda_atanh :: Floating a => WithMeta a -> WithMeta a
nlambda_atanh = idOp atanh

nlambda_break :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta ([a], [a])
nlambda_break = metaFunOpMeta break

nlambda_ceiling :: RealFrac a => Integral b => WithMeta a -> WithMeta b
nlambda_ceiling = idOp ceiling

nlambda_compare :: Ord a => WithMeta a -> WithMeta a -> Ordering
nlambda_compare = noMetaResUnionOp compare

nlambda_concat :: Foldable t => WithMeta (t [a]) -> WithMeta [a]
nlambda_concat = idOp concat

nlambda_concatMap :: (Foldable t, MetaLevel t) => (WithMeta a -> WithMeta [b]) -> WithMeta (t a) -> WithMeta [b]
nlambda_concatMap f (WithMeta x m) = liftMeta $ concatMap (dropMeta . metaFun m f) x

nlambda_const :: WithMeta a -> WithMeta b -> WithMeta a
nlambda_const = const

nlambda_cos :: Floating a => WithMeta a -> WithMeta a
nlambda_cos = idOp cos

nlambda_cosh :: Floating a => WithMeta a -> WithMeta a
nlambda_cosh = idOp cosh

nlambda_curry :: (WithMeta (a, b) -> WithMeta c) -> WithMeta a -> WithMeta b -> WithMeta c
nlambda_curry f x y = f (nlambda_pair x y)

nlambda_cycle :: WithMeta [a] -> WithMeta [a]
nlambda_cycle = idOp cycle

nlambda_decodeFloat :: RealFloat a => WithMeta a -> (Integer, Int)
nlambda_decodeFloat = noMetaResOp decodeFloat

nlambda_div :: Integral a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_div = unionOp div

nlambda_divMod :: Integral a => WithMeta a -> WithMeta a -> WithMeta (a, a)
nlambda_divMod = unionOp divMod

nlambda_drop :: Int -> WithMeta [a] -> WithMeta [a]
nlambda_drop = rightIdOp drop

nlambda_dropWhile :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta [a]
nlambda_dropWhile = metaFunOpMeta dropWhile

nlambda_either :: (WithMeta a -> WithMeta c) -> (WithMeta b -> WithMeta c) -> WithMeta (Either a b) -> WithMeta c
nlambda_either f1 f2 (WithMeta x m) = either (metaFun m f1) (metaFun m f2) x

nlambda_elem :: Foldable t => Eq a => WithMeta a -> WithMeta (t a) -> Bool
nlambda_elem = noMetaResUnionOp elem

nlambda_encodeFloat :: RealFloat a => Integer -> Int -> WithMeta a
nlambda_encodeFloat x y = empty $ encodeFloat x y

nlambda_enumFrom :: Enum a => WithMeta a -> WithMeta [a]
nlambda_enumFrom = idOp enumFrom

nlambda_enumFromThen :: Enum a => WithMeta a -> WithMeta a -> WithMeta [a]
nlambda_enumFromThen = unionOp enumFromThen

nlambda_enumFromThenTo :: Enum a => WithMeta a -> WithMeta a -> WithMeta a -> WithMeta [a]
nlambda_enumFromThenTo = union3Op enumFromThenTo

nlambda_enumFromTo :: Enum a => WithMeta a -> WithMeta a -> WithMeta [a]
nlambda_enumFromTo = unionOp enumFromTo

nlambda_error :: [Char] -> WithMeta a
nlambda_error = empty . error

nlambda_even :: Integral a => WithMeta a -> Bool
nlambda_even = noMetaResOp even

nlambda_exp :: Floating a => WithMeta a -> WithMeta a
nlambda_exp = idOp exp

nlambda_exponent :: RealFloat a => WithMeta a -> Int
nlambda_exponent = noMetaResOp exponent

nlambda_fail :: Monad m => String -> WithMeta (m a)
nlambda_fail = empty . fail

nlambda_filter :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta [a]
nlambda_filter = metaFunOpMeta filter

nlambda_flip :: (WithMeta a -> WithMeta b -> WithMeta c) -> WithMeta b -> WithMeta a -> WithMeta c
nlambda_flip = flip

nlambda_floatDigits :: RealFloat a => WithMeta a -> Int
nlambda_floatDigits = noMetaResOp floatDigits

nlambda_floatRadix :: RealFloat a => WithMeta a -> Integer
nlambda_floatRadix = noMetaResOp floatRadix

nlambda_floatRange :: RealFloat a => WithMeta a -> (Int, Int)
nlambda_floatRange = noMetaResOp floatRange

nlambda_floor :: RealFrac a => Integral b => WithMeta a -> WithMeta b
nlambda_floor = idOp floor

nlambda_fmap :: (Functor f, MetaLevel f) => (WithMeta a -> WithMeta b) -> WithMeta (f a) -> WithMeta (f b)
nlambda_fmap = (<$>###)

nlambda_foldMap :: Foldable t => Monoid m => (WithMeta a -> WithMeta m) -> WithMeta (t a) -> WithMeta m
nlambda_foldMap = metaFunOp foldMap

nlambda_foldl :: (Foldable t, MetaLevel t) => (WithMeta b -> WithMeta a -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
nlambda_foldl f x = foldl f x . dropMeta

nlambda_foldl1 :: (Foldable t, MetaLevel t) => (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta (t a) -> WithMeta a
nlambda_foldl1 f = foldl1 f . dropMeta

nlambda_foldr :: (Foldable t, MetaLevel t) => (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta (t a) -> WithMeta b
nlambda_foldr f x = foldr f x . dropMeta

nlambda_foldr1 :: (Foldable t, MetaLevel t) => forall a. (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta (t a) -> WithMeta a
nlambda_foldr1 f = foldr1 f . dropMeta

nlambda_fromEnum :: Enum a => WithMeta a -> Int
nlambda_fromEnum = noMetaResOp fromEnum

nlambda_fromInteger :: Num a => Integer -> WithMeta a
nlambda_fromInteger = empty . fromInteger

nlambda_fromIntegral :: (Integral a, Num b) => WithMeta a -> WithMeta b
nlambda_fromIntegral = idOp fromIntegral

nlambda_fromRational :: Fractional a => Rational -> WithMeta a
nlambda_fromRational = empty . fromRational

nlambda_fst :: WithMeta (a, b) -> WithMeta a
nlambda_fst = idOp fst

nlambda_gcd :: Integral a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_gcd = unionOp gcd

nlambda_head :: WithMeta [a] -> WithMeta a
nlambda_head = idOp head

nlambda_id :: WithMeta a -> WithMeta a
nlambda_id = id

nlambda_init :: WithMeta [a] -> WithMeta [a]
nlambda_init = idOp init

nlambda_ioError :: IOError -> WithMeta (IO a)
nlambda_ioError = empty . ioError

nlambda_isDenormalized :: RealFloat a => WithMeta a -> Bool
nlambda_isDenormalized = noMetaResOp isDenormalized

nlambda_isIEEE :: RealFloat a => WithMeta a -> Bool
nlambda_isIEEE = noMetaResOp isIEEE

nlambda_isInfinite :: RealFloat a => WithMeta a -> Bool
nlambda_isInfinite = noMetaResOp isInfinite

nlambda_isNaN :: RealFloat a => WithMeta a -> Bool
nlambda_isNaN = noMetaResOp isNaN

nlambda_isNegativeZero :: RealFloat a => WithMeta a -> Bool
nlambda_isNegativeZero = noMetaResOp isNegativeZero

nlambda_iterate :: (WithMeta a -> WithMeta a) -> WithMeta a -> WithMeta [a]
nlambda_iterate f = liftMeta . iterate f

nlambda_last :: WithMeta [a] -> WithMeta a
nlambda_last = idOp last

nlambda_lcm :: Integral a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_lcm = unionOp lcm

nlambda_length :: Foldable t => forall a. WithMeta (t a) -> Int
nlambda_length = noMetaResOp length

nlambda_log :: Floating a => WithMeta a -> WithMeta a
nlambda_log = idOp log

nlambda_logBase :: Floating a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_logBase = unionOp logBase

nlambda_lookup :: Eq a => WithMeta a -> WithMeta [(a, b)] -> WithMeta (Maybe b)
nlambda_lookup = unionOp lookup

nlambda_map :: (WithMeta a -> WithMeta b) -> WithMeta [a] -> WithMeta [b]
nlambda_map = (<$>###)

nlambda_mapM :: (Traversable t, MetaLevel t) => forall a (m :: * -> *) b. (Monad m, MetaLevel m) => (WithMeta a -> WithMeta (m b)) -> WithMeta (t a) -> WithMeta (m (t b))
nlambda_mapM f (WithMeta x m) = liftMeta $ fmap liftMeta $ mapM (dropMeta . metaFun m f) x

nlambda_mapM_ :: (Foldable t, Monad m, MetaLevel m) => (WithMeta a -> WithMeta (m b)) -> WithMeta (t a) -> WithMeta (m ())
nlambda_mapM_ f (WithMeta x m) = empty $ mapM_ (dropMeta . metaFun m f) x

nlambda_mappend :: Monoid a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_mappend = unionOp mappend

nlambda_max :: Ord a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_max = unionOp max

nlambda_maxBound :: Bounded a => WithMeta a
nlambda_maxBound = empty $ maxBound

nlambda_maximum :: Foldable t => forall a. Ord a => WithMeta (t a) -> WithMeta a
nlambda_maximum = idOp maximum

nlambda_maybe :: WithMeta b -> (WithMeta a -> WithMeta b) -> WithMeta (Maybe a) -> WithMeta b
nlambda_maybe d f (WithMeta x m) = maybe d (metaFun m f) x

nlambda_mconcat :: Monoid a => WithMeta [a] -> WithMeta a
nlambda_mconcat = idOp mconcat

nlambda_mempty :: Monoid a => WithMeta a
nlambda_mempty = empty $ mempty

nlambda_min :: Ord a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_min = unionOp min

nlambda_minBound :: Bounded a => WithMeta a
nlambda_minBound = empty $ minBound

nlambda_minimum :: Foldable t => forall a. Ord a => WithMeta (t a) -> WithMeta a
nlambda_minimum = idOp minimum

nlambda_mod :: Integral a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_mod = unionOp mod

nlambda_negate :: Num a => WithMeta a -> WithMeta a
nlambda_negate = idOp negate

nlambda_notElem :: (Foldable t, Eq a) => WithMeta a -> WithMeta (t a) -> Bool
nlambda_notElem = noMetaResUnionOp notElem

nlambda_null :: Foldable t => forall a. WithMeta (t a) -> Bool
nlambda_null = noMetaResOp null

nlambda_odd :: Integral a => WithMeta a -> Bool
nlambda_odd = noMetaResOp odd

nlambda_or :: Foldable t => WithMeta (t Bool) -> Bool
nlambda_or = noMetaResOp or

nlambda_pi :: Floating a => WithMeta a
nlambda_pi = empty pi

nlambda_pred :: Enum a => WithMeta a -> WithMeta a
nlambda_pred = idOp pred

nlambda_print :: Show a => WithMeta a -> IO ()
nlambda_print = print

nlambda_product :: Foldable t => forall a. Num a => WithMeta (t a) -> WithMeta a
nlambda_product = idOp product

nlambda_properFraction :: RealFrac a => forall b. Integral b => WithMeta a -> WithMeta (b, a)
nlambda_properFraction = idOp properFraction

nlambda_pure :: Applicative f => forall a. WithMeta a -> WithMeta (f a)
nlambda_pure = idOp pure

nlambda_quot :: Integral a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_quot = unionOp quot

nlambda_quotRem :: Integral a => WithMeta a -> WithMeta a -> WithMeta (a, a)
nlambda_quotRem = unionOp quotRem

nlambda_read :: Read a => String -> WithMeta a
nlambda_read = empty . read

nlambda_readIO :: Read a => String -> WithMeta (IO a)
nlambda_readIO = empty . readIO

nlambda_readList :: Read a => String -> WithMeta [([a], String)]
nlambda_readList = empty . readList

nlambda_readLn :: Read a => WithMeta (IO a)
nlambda_readLn = empty readLn

nlambda_readParen :: Bool -> (String -> WithMeta [(a, String)]) -> String -> WithMeta [(a, String)]
nlambda_readParen b f = empty . readParen b (value . f)

nlambda_reads :: Read a => String -> WithMeta [(a, String)]
nlambda_reads = empty . reads

nlambda_readsPrec :: Read a => Int -> String -> WithMeta [(a, String)]
nlambda_readsPrec n = empty . readsPrec n

nlambda_realToFrac :: (Real a, Fractional b) => WithMeta a -> WithMeta b
nlambda_realToFrac = idOp realToFrac

nlambda_recip :: Fractional a => WithMeta a -> WithMeta a
nlambda_recip = idOp recip

nlambda_rem :: Integral a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_rem = unionOp rem

nlambda_repeat :: WithMeta a -> WithMeta [a]
nlambda_repeat = idOp repeat

nlambda_replicate :: Int -> WithMeta a -> WithMeta [a]
nlambda_replicate = rightIdOp replicate

nlambda_return :: Monad m => forall a. WithMeta a -> WithMeta (m a)
nlambda_return = idOp return

nlambda_reverse :: WithMeta [a] -> WithMeta [a]
nlambda_reverse = idOp reverse

nlambda_round :: RealFrac a => forall b. Integral b => WithMeta a -> WithMeta b
nlambda_round = idOp round

nlambda_scaleFloat :: RealFloat a => Int -> WithMeta a -> WithMeta a
nlambda_scaleFloat = rightIdOp scaleFloat

nlambda_scanl :: (WithMeta b -> WithMeta a -> WithMeta b) -> WithMeta b -> WithMeta [a] -> WithMeta [b]
nlambda_scanl f x = liftMeta . scanl f x . dropMeta

nlambda_scanl1 :: (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta [a] -> WithMeta [a]
nlambda_scanl1 f = liftMeta . scanl1 f . dropMeta

nlambda_scanr :: (WithMeta a -> WithMeta b -> WithMeta b) -> WithMeta b -> WithMeta [a] -> WithMeta [b]
nlambda_scanr f x = liftMeta . scanr f x . dropMeta

nlambda_scanr1 :: (WithMeta a -> WithMeta a -> WithMeta a) -> WithMeta [a] -> WithMeta [a]
nlambda_scanr1 f = liftMeta . scanr1 f . dropMeta

nlambda_seq :: WithMeta a -> WithMeta b -> WithMeta b
nlambda_seq = seq

nlambda_sequence :: Traversable t => forall (m :: * -> *) a. Monad m => WithMeta (t (m a)) -> WithMeta (m (t a))
nlambda_sequence = idOp sequence

nlambda_sequenceA :: Traversable t => forall (f :: * -> *) a. Applicative f => WithMeta (t (f a)) -> WithMeta (f (t a))
nlambda_sequenceA = idOp sequenceA

nlambda_sequence_ :: (Foldable t, Monad m) => WithMeta (t (m a)) -> WithMeta (m ())
nlambda_sequence_ = idOp sequence_

nlambda_show :: Show a => WithMeta a -> String
nlambda_show = show

nlambda_showList :: Show a => WithMeta [a] -> ShowS
nlambda_showList = noMetaResOp showList

nlambda_shows :: Show a => WithMeta a -> ShowS
nlambda_shows = shows

nlambda_showsPrec :: Show a => Int -> WithMeta a -> ShowS
nlambda_showsPrec n = noMetaResOp $ showsPrec n

nlambda_significand :: RealFloat a => WithMeta a -> WithMeta a
nlambda_significand = idOp significand

nlambda_signum :: Num a => WithMeta a -> WithMeta a
nlambda_signum = idOp signum

nlambda_sin :: Floating a => WithMeta a -> WithMeta a
nlambda_sin = idOp sin

nlambda_sinh :: Floating a => WithMeta a -> WithMeta a
nlambda_sinh = idOp sinh

nlambda_snd :: WithMeta (a, b) -> WithMeta b
nlambda_snd = idOp snd

nlambda_span :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta ([a], [a])
nlambda_span = metaFunOpMeta span

nlambda_splitAt :: Int -> WithMeta [a] -> WithMeta ([a], [a])
nlambda_splitAt = rightIdOp splitAt

nlambda_sqrt :: Floating a => WithMeta a -> WithMeta a
nlambda_sqrt = idOp sqrt

nlambda_subtract :: Num a => WithMeta a -> WithMeta a -> WithMeta a
nlambda_subtract = unionOp subtract

nlambda_succ :: Enum a => WithMeta a -> WithMeta a
nlambda_succ = idOp succ

nlambda_sum :: Foldable t => forall a. Num a => WithMeta (t a) -> WithMeta a
nlambda_sum = idOp sum

nlambda_tail :: WithMeta [a] -> WithMeta [a]
nlambda_tail = idOp tail

nlambda_take :: Int -> WithMeta [a] -> WithMeta [a]
nlambda_take = rightIdOp take

nlambda_takeWhile :: (WithMeta a -> Bool) -> WithMeta [a] -> WithMeta [a]
nlambda_takeWhile = metaFunOpMeta takeWhile

nlambda_tan :: Floating a => WithMeta a -> WithMeta a
nlambda_tan = idOp tan

nlambda_tanh :: Floating a => WithMeta a -> WithMeta a
nlambda_tanh = idOp tanh

nlambda_toEnum :: Enum a => Int -> WithMeta a
nlambda_toEnum = empty . toEnum

nlambda_toInteger :: Integral a => WithMeta a -> Integer
nlambda_toInteger = noMetaResOp toInteger

nlambda_toRational :: Real a => WithMeta a -> Rational
nlambda_toRational = noMetaResOp toRational

nlambda_traverse :: (Traversable t, MetaLevel t) => forall a (f :: * -> *) b. (Applicative f, MetaLevel f)
    => (WithMeta a -> WithMeta (f b)) -> WithMeta (t a) -> WithMeta (f (t b))
nlambda_traverse f (WithMeta x m) = liftMeta $ fmap liftMeta $ traverse (dropMeta . metaFun m f) x

nlambda_truncate :: RealFrac a => forall b. Integral b => WithMeta a -> WithMeta b
nlambda_truncate = idOp truncate

nlambda_uncurry :: (WithMeta a -> WithMeta b -> WithMeta c) -> WithMeta (a, b) -> WithMeta c
nlambda_uncurry f x = f (idOp fst $ x) (idOp snd $ x)

nlambda_undefined :: WithMeta a
nlambda_undefined = empty undefined

nlambda_until :: (WithMeta a -> Bool) -> (WithMeta a -> WithMeta a) -> WithMeta a -> WithMeta a
nlambda_until = until

nlambda_unzip :: WithMeta [(a, b)] -> WithMeta ([a], [b])
nlambda_unzip = idOp unzip

nlambda_unzip3 :: WithMeta [(a, b, c)] -> WithMeta ([a], [b], [c])
nlambda_unzip3 = idOp unzip3

nlambda_zip :: WithMeta [a] -> WithMeta [b] -> WithMeta [(a, b)]
nlambda_zip = unionOp zip

nlambda_zip3 :: WithMeta [a] -> WithMeta [b] -> WithMeta [c] -> WithMeta [(a, b, c)]
nlambda_zip3 = union3Op zip3

nlambda_zipWith :: (WithMeta a -> WithMeta b -> WithMeta c) -> WithMeta [a] -> WithMeta [b] -> WithMeta [c]
nlambda_zipWith f x y = liftMeta $ zipWith f (dropMeta x) (dropMeta y)

nlambda_zipWith3 :: (WithMeta a -> WithMeta b -> WithMeta c -> WithMeta d) -> WithMeta [a] -> WithMeta [b] -> WithMeta [c] -> WithMeta [d]
nlambda_zipWith3 f x y z = liftMeta $ zipWith3 f (dropMeta x) (dropMeta y) (dropMeta z)
