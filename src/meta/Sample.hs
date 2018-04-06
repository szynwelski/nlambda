{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sample where

--import Prelude (Bool(True, False), Int, String, (+), (-), Show, show)
--import Prelude (Bool(..), Int, String, Show, show, (+), (-))
import Prelude (Show, show)


--infixr 9  .
--infixr 0  $

--{-# INLINE (.) #-}
--(.)    :: (b -> c) -> (a -> b) -> a -> c
--(.) f g = \x -> f (g x)
--
--{-# INLINE ($) #-}
--($) :: (a -> b) -> a -> b
--f $ x = f x
--
--id :: a -> a
--id x = x
--
--const :: a -> b -> a
--const x _ =  x
--
--flip :: (a -> b -> c) -> b -> a -> c
--flip f x y = f y x
--
------------------------------------------------------------------------------
---- Bool
------------------------------------------------------------------------------
--
--otherwise :: Bool
--otherwise = True
--
--not :: Bool -> Bool
--not True = False
--not False = True
--
--(&&) :: Bool -> Bool -> Bool
--False && _ = False
--_ && False = False
--_ && _ = True
--
--(||) :: Bool -> Bool -> Bool
--True || _ = True
--_ || True = True
--_ || _ = False
--
----------------------------------------------------------------------------
-- Maybe
----------------------------------------------------------------------------

--data Maybe a = Nothing | Just a
--
--instance Show a => Show (Maybe a) where
--    show Nothing = "Nothing"
--    show (Just x) = "Just " ++ show x

------------------------------------------------------------------------------
---- Pair
------------------------------------------------------------------------------
--
--fst :: (a, b) -> a
--fst (x, _) = x
--
--snd :: (a, b) -> b
--snd (_, x) = x
--
------------------------------------------------------------------------------
---- List
------------------------------------------------------------------------------

--null :: [a] -> Bool
--null [] = True
--null _ = False
--
--reverse :: [a] -> [a]
--reverse l = go l []
--    where go (x:l1) l2 = go l1 (x:l2)
--          go [] l = l
--
--
--(++) :: [a] -> [a] -> [a]
--{-# NOINLINE [1] (++) #-}
--(++) [] l = l
--(++) (x:l1) l2 = x:(l1 ++ l2)
--
--map :: (a -> b) -> [a] -> [b]
--{-# NOINLINE [0] map #-}
--map _ [] = []
--map f (x:l) = (f x) : (map f l)
--
--filter :: (a -> Bool) -> [a] -> [a]
--filter f [] = []
--filter f (x:l) = let fl = filter f l in if f x then x:fl else fl
--
--foldr :: (a -> b -> b) -> b -> [a] -> b
--{-# INLINE [0] foldr #-}
--foldr k z = go
--    where go []     = z
--          go (y:ys) = y `k` go ys

--head :: [a] -> Maybe a
--head [] = Nothing
--head (x:_) = Just x
--
--tail :: [a] -> Maybe [a]
--tail [] = Nothing
--tail (_:l) = Just l

----------------------------------------------------------------------------
-- Recursive
----------------------------------------------------------------------------

--fib1 :: Int -> Int
--fib1 0 = 0
--fib1 1 = 1
--fib1 n = fib1 (n-1) + fib1 (n-2)

--fib2 :: Int -> Int
--fib2 n = go n 0 1
--  where
--    go :: Int -> Int -> Int -> Int
--    go 0 a b = a
--    go n a b = go (n-1) b (a+b)

----------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------

data Atom = A

--instance Show Atom where
--    show A = "A"
--
data Bool = True | False
--
--instance Show Bool where
--    show True = "True"
--    show False = "False"

data Maybe a = Nothing | Just a
--
----instance Show a => Show (Maybe a) where
----    show Nothing = "Nothing"
----    show (Just x) = "Just"
--
class Class a where
    method1 :: a -> Atom
    method1 _ = A
    method2 :: a -> Atom
    method2 _ = A

instance Class Atom where
    method1 x = x

instance Class Bool

instance Class (Maybe a)

test :: Atom
test = A
