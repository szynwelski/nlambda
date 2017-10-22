{-# OPTIONS_GHC -fplugin MetaPlugin #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sample where

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

--data Bool = False | True
data  Maybe a  =  Nothing | Just a

showMaybe Nothing = "Nothing"
showMaybe (Just _) = "Just ?"
