{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
module Nominal.Maybe where

import qualified Data.Maybe as Maybe
import Nominal.Conditional
import Nominal.Formula
import Nominal.Variants
import Prelude hiding (filter, map, or)

----------------------------------------------------------------------------------------------------
-- NominalMaybe
----------------------------------------------------------------------------------------------------

-- | Variants of 'Maybe' values.
type NominalMaybe a = Variants (Maybe a)

-- | Creates a single variant with 'Nothing'.
nothing :: NominalMaybe a
nothing = variant Nothing

-- | Creates a single variant with 'Just' value.
just :: a -> NominalMaybe a
just = variant . Just

-- | Transforms 'Just' variants to values variants, crashes if variants contain a 'Nothing' value.
fromJust :: Ord a => NominalMaybe a -> Variants a
fromJust = map Maybe.fromJust

-- | Takes a default value and 'Maybe' variants and returns a variants of values from 'Just' and default value instead of 'Nothing'.
fromMaybe :: Ord a => a -> NominalMaybe a -> Variants a
fromMaybe dv = map (Maybe.fromMaybe dv)

-- | Takes a default value, function and 'Maybe' variants and applies function to the values inside the 'Just' or returns the default value instead of 'Nothing'.
maybe :: Ord b => b -> (a -> b) -> NominalMaybe a -> Variants b
maybe dv f = map (Maybe.maybe dv f)

-- | Returns a condition under which the variants have a 'Just' value.
isJust :: NominalMaybe a -> Formula
isJust = satisfying Maybe.isJust

-- | Returns a condition under which the variants have a 'Nothing' value.
isNothing :: NominalMaybe a -> Formula
isNothing = satisfying Maybe.isNothing

-- | If a given condition is satisfied returns 'Just' value otherwise returns 'Nothing'.
maybeIf :: Ord a => Formula -> a -> NominalMaybe a
maybeIf c v = ite c (just v) nothing
