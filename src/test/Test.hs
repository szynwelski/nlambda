module Test where

import Prelude hiding (sum, map, filter)

emptySet :: [[a]]
emptySet = [[]]

add :: [a] -> [[a]] -> [[a]]
add e s = sum $ fmap (\es -> (fmap (:es) e)) s

empty :: [[a]] -> [Bool]
empty = fmap null

map :: ([a] -> [b]) -> [[a]] -> [[b]]
map = fmap

sum :: [[[a]]] -> [[a]]
sum = foldl (++) []

----------------------------------------------------------------------------------------------------

just :: [a] -> [[a]]
just = flip add emptySet

--filter :: (a -> Bool) -> [[a]] -> [[a]]
--filter f s = sum (map (\x -> if (f x) then (just x) else emptySet) s)
--
--exists :: (a -> Bool) -> [a] -> Bool
--exists f s = not $ empty $ filter f s
--
--forall :: (a -> Bool) -> [a] -> Bool
--forall f s = empty $ filter (\x -> not (f x)) s
--
--union :: [a] -> [a] -> [a]
--union s1 s2 = sum $ add s1 $ add s2 emptySet
