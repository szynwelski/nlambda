module Nominal.Maybe where

import qualified Data.Maybe as Maybe
import Nominal.Conditional
import Nominal.Formula
import Nominal.Variants
import Prelude hiding (filter, map, or)

----------------------------------------------------------------------------------------------------
-- NominalMaybe
----------------------------------------------------------------------------------------------------

type NominalMaybe a = Variants (Maybe a)

nothing :: NominalMaybe a
nothing = variant Nothing

just :: a -> NominalMaybe a
just = variant . Just

fromJust :: Ord a => NominalMaybe a -> Variants a
fromJust = map Maybe.fromJust

fromMaybe :: Ord a => a -> NominalMaybe a -> Variants a
fromMaybe dv = map (Maybe.fromMaybe dv)

maybe :: Ord b => b -> (a -> b) -> NominalMaybe a -> Variants b
maybe dv f = map (Maybe.maybe dv f)

isJust :: NominalMaybe a -> Formula
isJust = satisfying Maybe.isJust

isNothing :: NominalMaybe a -> Formula
isNothing = satisfying Maybe.isNothing

when :: Ord a => Formula -> a -> NominalMaybe a
when c v = ite c (just v) nothing

when' :: Ord a => Formula -> a -> NominalMaybe a
when' c v = ite' c (just v) nothing
