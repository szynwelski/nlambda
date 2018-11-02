module Nominal.Meta.Data.Maybe where

import Data.Maybe
import Nominal.Meta
import Nominal.Variable

nlambda_catMaybes :: WithMeta [Maybe a] -> WithMeta [a]
nlambda_catMaybes = idOp catMaybes

nlambda_fromJust :: WithMeta (Maybe a) -> WithMeta a
nlambda_fromJust = idOp fromJust

nlambda_fromMaybe :: Var a => WithMeta a -> WithMeta (Maybe a) -> WithMeta a
nlambda_fromMaybe = renameAndApply2 fromMaybe

nlambda_isJust :: WithMeta (Maybe a) -> Bool
nlambda_isJust = isJust . value

nlambda_isNothing :: WithMeta (Maybe a) -> Bool
nlambda_isNothing = isNothing . value

nlambda_listToMaybe :: WithMeta [a] -> WithMeta (Maybe a)
nlambda_listToMaybe = idOp listToMaybe

nlambda_maybe :: WithMeta b -> (WithMeta a -> WithMeta b) -> WithMeta (Maybe a) -> WithMeta b
nlambda_maybe n _ (WithMeta Nothing _) = n
nlambda_maybe _ f (WithMeta (Just x) m) = f (WithMeta x m)

nlambda_maybeToList :: WithMeta (Maybe a) -> WithMeta [a]
nlambda_maybeToList = idOp maybeToList
