{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Test where

import Data.Either (isLeft, isRight)
import Data.Foldable (toList)
import Data.List (nub, sort)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Semigroup (Semigroup, (<>))
import GHC.Generics
import Meta (MetaLevel)
import Var (Var, Variable)

data Wrapper a = Wrapper a deriving (Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable, Traversable)
data Optional a = Optional a | Null deriving (Show, Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable, Traversable)
data Pair a b = Pair a b deriving (Show, Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable)
data List a = Element a (List a) | Empty deriving (Show, Generic, Var, Eq, Ord, Generic1, MetaLevel, Functor, Foldable)

fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = Element x $ fromList xs

----------------------------------------------------------------------------
-- Test Show
----------------------------------------------------------------------------

instance Show a => Show (Wrapper a) where
    show (Wrapper x) = "W " ++ show x

test1 :: Variable -> Variable -> Variable -> [String]
test1 x y z = [show (), show x, show y, show (x,y), show (x,y,z), show ([]::[Int]), show [x], show [x,y], show [x,y,z],
               show $ Wrapper y, show $ Wrapper $ Wrapper y,
               show (Optional x), show (Null::Optional Char), show (Optional x, Null::Optional String),
               show (Optional x, Optional y, Null::Optional (), Optional z),
               show (Pair x y), show (Pair x 1), show (Pair x [y]), show (Pair x (Pair y z)),
               show $ fromList ([]::[Variable]), show $ fromList [x], show $ fromList [x,y], show $ fromList [x,y,z]]

----------------------------------------------------------------------------
-- Test Eq
----------------------------------------------------------------------------

test2 :: Variable -> Variable -> Variable -> [Bool]
test2 x y z = [x == x, x == y, x == z, [x] == [y], [x,y] == [x,x], [x,y,z] /= [x,y,z],
               Wrapper x == Wrapper x, Wrapper x == Wrapper y, Wrapper x /= Wrapper z,
               Optional x == Null, Optional x == Optional x, Optional x == Optional y, Optional [x] /= Optional [z],
               Pair x y == Pair y x, Pair x 1 == Pair x 1, Pair x (y,z,1) /= Pair x (x,z,1),
               fromList [] == fromList [x], fromList [x] == fromList [x], fromList [x,x,z] /= fromList [x,y,z]]

----------------------------------------------------------------------------
-- Test Ord
----------------------------------------------------------------------------

test3 :: Variable -> Variable -> Variable -> [Ordering]
test3 x y z = fmap (uncurry compare) [(x,x), (x,y), (x,z)]
              ++ fmap (uncurry compare) [([x],[y]), ([x,y],[x,x]), ([x,y,z],[x,y,z])]
              ++ fmap (uncurry compare) [(Wrapper x,Wrapper x), (Wrapper x,Wrapper y), (Wrapper x,Wrapper z)]
              ++ fmap (uncurry compare) [(Optional x,Null), (Optional x,Optional x), (Optional x,Optional y), (Optional z,Optional x)]

test4 :: Variable -> Variable -> Variable -> [Bool]
test4 x y z = [Pair x y < Pair y x, Pair x 1 > Pair x 1, Pair x (y,z,1) <= Pair x (x,z,1)]

test5 :: Variable -> Variable -> Variable -> [List Variable]
test5 x y z = sort [fromList [], fromList [x], fromList [x], fromList [y], fromList [z], fromList [x,x,z], fromList [x,y,z]]

----------------------------------------------------------------------------
-- Test Functor
----------------------------------------------------------------------------

test6 :: Variable -> Variable -> Variable -> [[Variable]]
test6 x y z = [fmap id [x,y,z], fmap (const z) [x,y], fmap id [], fmap (const y) [1,2,3]]

test7 :: Variable -> Variable -> Variable -> [Maybe Variable]
test7 x y z = [fmap id Nothing, fmap id (Just x), fmap (const x) (Just y), fmap (const y) (Just z)]

test8 :: Variable -> Variable -> Variable -> [Wrapper Variable]
test8 x y z = [fmap id (Wrapper x), fmap id (Wrapper y), fmap id (Wrapper z), fmap (const z) (Wrapper x), fmap (const y) (Wrapper 1)]

test9 :: Variable -> Variable -> Variable -> [Optional Variable]
test9 x y z = [fmap id (Optional x), fmap id (Optional y), fmap id (Optional z), fmap (const z) Null, fmap (const y) (Optional 1)]

test10 :: Variable -> Variable -> Variable -> [Pair Variable Variable]
test10 x y z = [fmap id (Pair x y), fmap (const z) (Pair x y), fmap (const z) (Pair x 1)]

test11 :: Variable -> Variable -> Variable -> [List Variable]
test11 x y z = [fmap id (fromList [x,y,z]), fmap (const x) (fromList [x,y,z]), y <$ (fromList [x,y,z])]

----------------------------------------------------------------------------
-- Test Foldable
----------------------------------------------------------------------------

test12 :: Variable -> Variable -> Variable -> [[Variable]]
test12 x y z = [f [x,y,z], f (Just x), f (Left x), f (Right x), f (Wrapper x), f (Pair x y), f (Optional x), f Null, f $ fromList [x,y,z]]
    where f :: Foldable t => t a -> [a]
          f = foldr (:) []

test13 :: Variable -> Variable -> Variable -> [Int]
test13 x y z = [length [x,y,z], length (Just x), length (Left x), length (Right x), length (Wrapper x),
                length (Pair x y), length (Optional x), length Null, length $ fromList [x,y,z]]


test14 :: Variable -> Variable -> Variable -> [Bool]
test14 x y z = [null [x,y,z], null (Just x), null (Left x), null (Right x), null (Wrapper x),
                null (Pair x y), null (Optional x), null Null, null $ fromList [x,y,z],
                elem y [x,y,z], elem y (Just x), elem y (Left x), elem y (Right x :: Either Variable Variable),
                elem y (Wrapper x), elem y (Pair x y), elem y (Optional x), elem y Null, elem y $ fromList [x,y,z]]

test15 :: Variable -> Variable -> Variable -> [Variable]
test15 x y z = [maximum [x,y,z], maximum (Just x), maximum (Right x), maximum (Wrapper x),
                maximum (Pair x y), maximum (Optional x), maximum $ fromList [x,y,z],
                minimum [x,y,z], minimum (Just x), minimum (Right x :: Either Variable Variable),
                minimum (Wrapper x), minimum (Pair x y), minimum (Optional x), minimum $ fromList [x,y,z]]

----------------------------------------------------------------------------
-- Test Semigroup
----------------------------------------------------------------------------

test16 :: Variable -> Variable -> Variable -> [Maybe [Variable]]
test16 x y z = [Nothing <> Nothing, Nothing <> Just [x], Just [x] <> Nothing, Just [x] <> Just [y]]

instance Semigroup a => Semigroup (Wrapper a) where
    (Wrapper x) <> (Wrapper y) = Wrapper (x <> y)

test17 :: Variable -> Variable -> Variable -> Wrapper [Variable]
test17 x y z = Wrapper [x,x] <> Wrapper [y,y]

instance Semigroup a => Semigroup (Optional a) where
    Null <> b = b
    a <> Null = a
    Optional a <> Optional b = Optional (a <> b)

test18 :: Variable -> Variable -> Variable -> [Optional [Variable]]
test18 x y z = [Null <> Null, Null <> Optional [x], Optional [x] <> Null, Optional [x] <> Optional [y]]

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
    (Pair x1 y1) <> (Pair x2 y2) = Pair (x1 <> x2) (y1 <> y2)

test19 :: Variable -> Variable -> Variable -> Pair () [Variable]
test19 x y z = Pair () [x] <> Pair () [y,z]

instance Semigroup (List a) where
    l1 <> l2 = fromList (toList l1 <> toList l2)

test20 :: Variable -> Variable -> Variable -> List Variable
test20 x y z = fromList [x] <> fromList [y,z]

----------------------------------------------------------------------------
-- Test Monoid
----------------------------------------------------------------------------

test21 :: Variable -> Variable -> Variable -> [[Variable]]
test21 x y z = [mempty, mappend [x] [y,z], mconcat [[x], [y,z]]]

instance (Semigroup a, Monoid a) => Monoid (Wrapper a) where
    mempty = Wrapper mempty
    mappend = (<>)

test22 :: Variable -> Variable -> Variable -> [Wrapper [Variable]]
test22 x y z = [mempty, mappend (Wrapper [x]) (Wrapper [y,z]), mconcat [Wrapper [x], Wrapper [y,z]]]

instance (Semigroup a) => Monoid (Optional a) where
    mempty = Null
    mappend = (<>)

test23 :: Variable -> Variable -> Variable -> [Optional [Variable]]
test23 x y z = [mempty, mappend (Optional [x]) (Optional [y,z]), mappend (Optional [x]) Null, mconcat [Optional [x], Optional [y,z]], mconcat [Null]]

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = Pair mempty mempty
    mappend = (<>)

test24 :: Variable -> Variable -> Variable -> [Pair (Optional [Variable]) [Variable]]
test24 x y z = [mempty, mappend (Pair Null [x,y]) (Pair Null [z]), mconcat [Pair Null [], Pair (Optional [x]) [y,z]]]

instance Monoid (List a) where
    mempty = Empty
    mappend = (<>)

test25 :: Variable -> Variable -> Variable -> [List Variable]
test25 x y z = [mempty, mappend (fromList [x]) (fromList [y,z]), mconcat [fromList [x], fromList [y,z]]]

----------------------------------------------------------------------------
-- Test Applicative
----------------------------------------------------------------------------

test26 :: Variable -> Variable -> Variable -> [[Variable]]
test26 x y z = [pure x, [] <*> [x], [const x] <*> [y, z], [x, x] *> [y, z], [x] <* [y, z]]

instance Applicative Wrapper where
    pure = Wrapper
    Wrapper f <*> Wrapper x = Wrapper (f x)

test27 :: Variable -> Variable -> Variable -> [Wrapper Variable]
test27 x y z = [pure x, Wrapper id <*> Wrapper x, Wrapper (const y) <*> Wrapper z, Wrapper x *> Wrapper y, Wrapper x <* Wrapper y]

instance Applicative Optional where
    pure = Optional
    Optional f <*> a = fmap f a
    Null <*> _ = Null

test28 :: Variable -> Variable -> Variable -> [Optional Variable]
test28 x y z = [pure x, Optional id <*> Optional x, Null <*> Optional z, Optional id <*> Null, Optional x *> Optional y, Optional x <* (Null::Optional Variable)]

-- FIXME
--instance (Semigroup a, Monoid a) => Applicative (Pair a) where
--    pure x = Pair mempty x
--    Pair u f <*> Pair v x = Pair (u <> v) (f x)

instance Applicative List where
    pure x = Element x Empty
    fs <*> xs = fromList (toList fs <*> toList xs)

test29 :: Variable -> Variable -> Variable -> [List Variable]
test29 x y z = [pure x, fromList [] <*> fromList [x], fromList [const x] <*> fromList [y, z],
                fromList [x, x] *> fromList [y, z], fromList [x] <* fromList [y, z]]

----------------------------------------------------------------------------
-- Test Monad
----------------------------------------------------------------------------

instance Monad Wrapper where
    Wrapper x >>= f = f x

test30 :: Variable -> Variable -> Variable -> [Wrapper Variable]
test30 x y z = [Wrapper x >>= Wrapper, Wrapper x >> Wrapper y, return z]

instance Monad Optional where
    (Optional x) >>= k = k x
    Null  >>= _ = Null
    (>>) = (*>)
    fail _ = Null

test31 :: Variable -> Variable -> Variable -> [Optional Variable]
test31 x y z = [Optional x >>= Optional, Null >>= Optional, Optional x >> Optional y, Optional z >> Null,
                (Null :: Optional Variable) >> Optional x, (Null :: Optional Variable) >> Null, return x]

-- FIXME
--instance (Semigroup a, Monoid a) => Monad (Pair a) where
--    Pair u a >>= k = case k a of Pair v b -> Pair (u <> v) b

instance Monad List where
    xs >>= f = fromList (toList xs >>= toList . f)

test32 :: Variable -> Variable -> Variable -> [List Variable]
test32 x y z = [do {x' <- Empty; return x'}, do {x' <- fromList [x,y,z]; return x'}, do {x' <- fromList [x,y,z]; return z}]

----------------------------------------------------------------------------
-- Test Traversable
----------------------------------------------------------------------------

test33 :: Variable -> Variable -> Variable -> [Wrapper (Optional Variable)]
test33 x y z = [traverse Wrapper (Optional x), mapM Wrapper (Optional x), sequenceA (Optional (Wrapper y)), sequence (Optional (Wrapper y)), sequence Null]

----------------------------------------------------------------------------
-- Test Prelude functions
----------------------------------------------------------------------------

test34 :: Variable -> Variable -> Variable -> [Variable]
test34 x y z = [id $ x, const x $! y, [z]!!0, (id . const x) y, seq x x, head [x,z], last [y], either id id (Left x), fst (flip (,) x y), curry snd x y,
                fromMaybe x $ lookup x [(y,z)], fromJust $ lookup x [(x,z)], fromMaybe x Nothing]

test35 :: Variable -> Variable -> Variable -> [[Variable]]
test35 x y z = [tail $ concat [[x]], concatMap (:[]) [x], take 4 $ drop 10 $ cycle [y], dropWhile (const True) [z], filter (const True) [x], init [y],
                reverse $ replicate 5 z, fst $ splitAt 4 $ repeat x, scanl const x [y], scanl1 const [x], scanr const x [y], scanr1 const [x],
                fst $ span (const True) [x,y,z], takeWhile (const False) $ repeat x, map id [x,y], snd $ unzip $ zip [x,y][z],
                (\(x,_,_) -> x) $ unzip3 $ zip3 [x][y][z], catMaybes $ filter isJust [Just x, Nothing]]

test36 :: Variable -> Variable -> Variable -> [Bool]
test36 x y z = [x == x && x /= x, x == y && x /= y, x == x || x /= x, x == y || x /= y, and [x == x, x /= y], or [x == y, x /= y], all (== x) [y],
                any (== x) [], elem x [x], notElem x [x], isLeft (if x == x then Left x else Right y), isRight (if x == y then Left x else Right y)]

test37 :: Variable -> Variable -> Variable -> Wrapper Variable
test37 x y z = id id (Wrapper x)

----------------------------------------------------------------------------
-- Test user classes
----------------------------------------------------------------------------

class Vars a where
    vars :: a -> [Variable]

instance Vars Variable where
    vars v = [v]

instance Vars a => Vars (Wrapper a) where
    vars (Wrapper x) = vars x

instance Vars a => Vars (Optional a) where
    vars (Optional x) = vars x
    vars Null = []

instance (Vars a, Vars b) => Vars (Pair a b) where
    vars (Pair x y) = nub $ vars x ++ vars y

instance Vars a => Vars (List a) where
    vars (Element x xs) = nub $ vars x ++ vars xs
    vars Empty = []

test38 :: Variable -> Variable -> Variable -> [[Variable]]
test38 x y z = [vars x, vars y, vars z, vars (Wrapper x), vars (Optional x), vars (Null::Optional Variable), vars (Pair x y), vars $ fromList [x,y,z]]

class Fun f where
    fun :: (a -> b) -> f a -> f b

instance Fun Wrapper where
    fun f (Wrapper x) = Wrapper (f x)

instance Fun Optional where
    fun f (Optional x) = Optional (f x)
    fun f Null = Null

-- FIXME
--instance Fun List where
--    fun f (Element x xs) = Element (f x) (fun f xs)
--    fun f Empty = Empty

--test :: Variable -> Variable -> Variable -> [[Variable]]
--test x y z = [vars $ fun id $ Wrapper x, vars $ fun (const y) $ Optional x, vars $ fun id (Null::Optional Variable),
--              vars $ fun id $ fromList [x,y,z], vars $ fun (const x) $ fromList [x,y,z]]
