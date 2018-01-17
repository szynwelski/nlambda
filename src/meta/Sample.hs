{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sample where

import Prelude (String, Int, Char)

--data Data = X | Y Data
--
--x = X
--y = Y X
--
----showData :: Data -> String
--showData X = "X"
--showData (Y _) = "Y"


--newtype NewType a = NewType a
--newtyp = NewType X

--one :: Int
--one = 1
--one' = id one
--
--id x = x

--data W a = W a
--wx = W X
--showW (W a) = "W"

--data Pair a b = Pair a b
--fst (Pair x _) = x
--snd (Pair _ x) = x
--
--one'' = fst (Pair 1 2)
--two'' = snd (Pair 1 2)
--letx = let x = Pair 1 2 in fst x

-- Show
class Show a where
    show :: a -> String
    show _ = ""

---- Eq
--class Eq a where
--    eq :: a -> a -> Bool
--
---- Bool
data Bool = False | True

instance Show Bool where
--    show True = "True"
--    show False = "False"

---- Maybe
--data  Maybe a = Nothing | Just a
--
--instance Show a => Show (Maybe a) where
--    show Nothing = "Nothing"
--    show (Just x) = show x

-- List
--data List a = Empty | List a (List a)
--
--instance Show a => Show (List a) where
--    show Empty = "[]"
--    show (List x l) = "[x]"
--
--isEmpty :: List a -> Bool
--isEmpty Empty = True
--isEmpty (List _ _) = False
--
--empty :: List a
--empty = Empty
--
--singleton :: a -> List a
--singleton x = List x Empty
--
--reverse :: List a -> List a
--reverse l = go l Empty
--    where go (List x l1) l2 = go l1 (List x l2)
--          go Empty l = l
--
--(++) :: List a -> List a -> List a
--(++) l1 l2 = go (reverse l1) l2
--    where go Empty l = l
--          go (List x l1) l2 = go l1 (List x l2)


test :: String
test = show True
