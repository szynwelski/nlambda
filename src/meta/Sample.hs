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


-- Bool

data Bool = False | True

--showBool True = "True"
--showBool False = "False"
--
--data  Maybe a = Nothing | Just a
----
----showMaybe Nothing = "Nothing"
----showMaybe (Just _) = "Just ?"
--
--data List a = Empty | List a (List a)
--
--showList Empty = "Empty"
--showList (List _ _) = "List"
--
--isEmpty Empty = True
--isEmpty (List _ _) = False
--
--test1 = showList Empty
--test2 = showList (List 1 Empty)
--test3 = showList (List 1 (List 2 Empty))
--test4 = showBool (isEmpty Empty)
--test5 = showBool (isEmpty (List 1 Empty))

class Show a where
    show :: a -> String
    show _ = "!"
--    number :: a -> Int
--    mark :: a -> Char

--class Eq a where
--    eq :: a -> a -> Bool

instance Show Bool where
    show True = "True"
    show False = "False"
--    number True = 1
--    number False = 0
--    mark True = 'y'
--    mark False = 'n'

--instance Show (Maybe a) where
--    show Nothing = "Nothing"
--    show (Just _) = "Just"
--
test :: String
test = show True
--
--test1 :: Int
--test1 = number True
--
--test2 :: Char
--test2 = mark True
--
--test3 = show Nothing
