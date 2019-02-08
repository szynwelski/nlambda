{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Nominal.Util.UnionFind where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Nominal.Variable (Var)

----------------------------------------------------------------------------------------------------
-- Disjoint-set data structure
----------------------------------------------------------------------------------------------------

data UnionFind a = UnionFind {parents :: Map a a, ranks :: Map a Int} deriving (Show, Read, Generic, Var)

empty :: UnionFind a
empty = UnionFind Map.empty Map.empty

find :: Ord a => a -> UnionFind a -> (a, UnionFind a)
find e uf@(UnionFind ps rs) = case Map.lookup e ps of
                               Nothing -> (e, uf)
                               Just parent -> let (repr, UnionFind ps' rs') = find parent uf
                                              in (repr, UnionFind (Map.insert e repr ps') rs')

union :: Ord a => a -> a -> UnionFind a -> UnionFind a
union e1 e2 uf@(UnionFind ps rs)
    | repr1 == repr2 = uf
    | r1 > r2 = UnionFind (Map.insert repr2 repr1 ps') rs'
    | r1 < r2 = UnionFind (Map.insert repr1 repr2 ps') rs'
    | otherwise = UnionFind (Map.insert repr2 repr1 ps') (Map.insert repr1 (succ r1) rs')
    where (repr1, uf') = find e1 uf
          (repr2, UnionFind ps' rs') = find e2 uf'
          r1 = Map.findWithDefault 0 repr1 rs'
          r2 = Map.findWithDefault 0 repr2 rs'

assocs :: Ord a => UnionFind a -> [(a, a)]
assocs uf@(UnionFind ps rs) = snd $ foldl (\(uf', res) e -> let (repr, uf'') = find e uf'
                                                            in (uf'', (e, repr) : res)) (uf, []) (Map.keys ps)

representatives :: Ord a => [(a,a)] -> [(a,a)]
representatives = assocs . foldr (\(e1,e2) uf -> union e1 e2 uf) empty
