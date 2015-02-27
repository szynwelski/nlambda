module Nominal.Util.InsertionSet where

import Data.Set (Set)
import qualified Data.Set as Set

----------------------------------------------------------------------------------------------------
-- Insertion Set
----------------------------------------------------------------------------------------------------

data InsertionSet a = InsertionSet (Set a) [a]

empty :: InsertionSet a
empty = InsertionSet Set.empty []

null :: InsertionSet a -> Bool
null (InsertionSet s _) = Set.null s

insert :: Ord a => a -> InsertionSet a -> InsertionSet a
insert e is@(InsertionSet s l) = if Set.member e s then is else (InsertionSet (Set.insert e s) (e : l))

toSet :: InsertionSet a -> Set a
toSet (InsertionSet s _) = s

toList :: InsertionSet a -> [a]
toList (InsertionSet _ l) = reverse l
