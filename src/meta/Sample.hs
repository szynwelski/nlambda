{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sample where

import Prelude (Bool(..), String)


infixr 9  .
infixr 0  $

{-# INLINE (.) #-}
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

{-# INLINE ($) #-}
($) :: (a -> b) -> a -> b
f $ x = f x

id :: a -> a
id x = x

const :: a -> b -> a
const x _ =  x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

----------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------

class Show a where
    show :: a -> String

----------------------------------------------------------------------------
-- Bool
----------------------------------------------------------------------------

instance Show Bool where
    show True = "True"
    show False = "False"

otherwise :: Bool
otherwise = True

not :: Bool -> Bool
not True = False
not False = True

(&&) :: Bool -> Bool -> Bool
False && _ = False
_ && False = False
_ && _ = True

(||) :: Bool -> Bool -> Bool
True || _ = True
_ || True = True
_ || _ = False

----------------------------------------------------------------------------
-- Maybe
----------------------------------------------------------------------------

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

----------------------------------------------------------------------------
-- Pair
----------------------------------------------------------------------------

instance (Show a, Show b) => Show (a, b) where
    show (x, y) = "(" ++ show x ++ "," ++ show y ++ ")"

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, x) = x

----------------------------------------------------------------------------
-- List
----------------------------------------------------------------------------

instance Show a => Show [a] where
    show [] = "[]"
    show (x:l) = "[" ++ show x ++ showTail l
        where showTail [] = "]"
              showTail (x:l) = "," ++ show x ++ showTail l

null :: [a] -> Bool
null [] = True
null _ = False

reverse :: [a] -> [a]
reverse l = go l []
    where go (x:l1) l2 = go l1 (x:l2)
          go [] l = l


(++) :: [a] -> [a] -> [a]
{-# NOINLINE [1] (++) #-}
(++) [] l = l
(++) (x:l1) l2 = x:(l1 ++ l2)

map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [0] map #-}
map _ [] = []
map f (x:l) = (f x) : (map f l)

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:l) = let fl = filter f l in if f x then x:fl else fl

foldr :: (a -> b -> b) -> b -> [a] -> b
{-# INLINE [0] foldr #-}
foldr k z = go
    where go []     = z
          go (y:ys) = y `k` go ys

head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (_:l) = Just l

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

test :: Bool
test = not $ (id const) True False
