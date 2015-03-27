module Nominal.Either where

import qualified Data.Either.Utils as Utils
import Nominal.Formula
import Nominal.Variants
import Prelude hiding (filter, map, or)

----------------------------------------------------------------------------------------------------
-- NominalEither
----------------------------------------------------------------------------------------------------

type NominalEither a b = Variants (Either a b)

left :: a -> NominalEither a b
left v = variant (Left v)

right :: b -> NominalEither a b
right v = variant (Right v)

fromLeft :: Ord a => NominalEither a b -> Variants a
fromLeft = map Utils.fromLeft . filter isLeft
    where isLeft (Left _) = True
          isLeft (Right _) = False

fromRight :: Ord b => NominalEither a b -> Variants b
fromRight = map Utils.fromRight . filter isRight
    where isRight (Left _) = False
          isRight (Right _) = True

fromEither :: Ord a => NominalEither a a -> Variants a
fromEither = map Utils.fromEither

isLeft :: NominalEither a b -> Formula
isLeft = satisfying isLeft
    where isLeft (Left _) = True
          isLeft (Right _) = False

isRight :: NominalEither a b -> Formula
isRight = satisfying isRight
    where isRight (Left _) = False
          isRight (Right _) = True
