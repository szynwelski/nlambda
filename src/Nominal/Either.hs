module Nominal.Either where

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
fromLeft = map f
    where f (Left a) = a
          f _ = error "Nominal.Either.fromLeft: Right"

-- | Transforms 'Right' variants to values variants, crashes if variants contain a 'Left' value.
fromRight :: Ord b => NominalEither a b -> Variants b
fromRight = map f
    where f (Right a) = a
          f _ = error "Nominal.Either.fromRight: Left"

-- | Transforms 'Either' variants to values variants.
fromEither :: Ord a => NominalEither a a -> Variants a
fromEither = map f
    where f (Left a)  = a
          f (Right a) = a

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
