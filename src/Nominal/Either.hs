{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
module Nominal.Either where

import qualified Data.Either.Utils as Utils
import Nominal.Formula
import Nominal.Variants
import Prelude hiding (filter, map, or)

----------------------------------------------------------------------------------------------------
-- NominalEither
----------------------------------------------------------------------------------------------------

-- | Variants of 'Either' values.
type NominalEither a b = Variants (Either a b)

-- | Creates a single variant with a 'Left' value.
left :: a -> NominalEither a b
left v = variant (Left v)

-- | Creates a single variant with a 'Right' value.
right :: b -> NominalEither a b
right v = variant (Right v)

-- | Transforms 'Left' variants to values variants, crashes if variants contain a 'Right' value.
fromLeft :: Ord a => NominalEither a b -> Variants a
fromLeft = map Utils.fromLeft

-- | Transforms 'Right' variants to values variants, crashes if variants contain a 'Left' value.
fromRight :: Ord b => NominalEither a b -> Variants b
fromRight = map Utils.fromRight

-- | Transforms 'Either' variants to values variants.
fromEither :: Ord a => NominalEither a a -> Variants a
fromEither = map Utils.fromEither

-- | Returns a formula describing the condition of occurrence of 'Left' values in variants.
isLeft :: NominalEither a b -> Formula
isLeft = satisfying isLeft
    where isLeft (Left _) = True
          isLeft (Right _) = False

-- | Returns a formula describing the condition of occurrence of 'Right' values in variants.
isRight :: NominalEither a b -> Formula
isRight = satisfying isRight
    where isRight (Left _) = False
          isRight (Right _) = True
