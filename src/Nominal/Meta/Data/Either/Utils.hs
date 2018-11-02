module Nominal.Meta.Data.Either.Utils where

import Data.Either.Utils
import Nominal.Meta

nlambda_fromEither :: WithMeta (Either a a) -> WithMeta a
nlambda_fromEither = idOp fromEither

nlambda_fromLeft :: WithMeta (Either a b) -> WithMeta a
nlambda_fromLeft = idOp fromLeft

nlambda_fromRight :: WithMeta (Either a b) -> WithMeta b
nlambda_fromRight = idOp fromRight
