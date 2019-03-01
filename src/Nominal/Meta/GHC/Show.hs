module Nominal.Meta.GHC.Show where

import GHC.Show (showList__)
import Nominal.Meta
import Nominal.Variable

class Show a => NLambda_Show a where
    nlambda_showsPrec :: Int -> WithMeta a -> ShowS
    nlambda_showsPrec n = noMetaResOp (showsPrec n)
    nlambda_show :: WithMeta a -> String
    nlambda_show = noMetaResOp show
    nlambda_showList :: WithMeta [a] -> ShowS
    nlambda_showList = noMetaResOp showList

instance NLambda_Show a => NLambda_Show [a]
instance NLambda_Show Word
instance NLambda_Show Ordering
instance NLambda_Show a => NLambda_Show (Maybe a)
instance NLambda_Show Integer
instance NLambda_Show Int
instance NLambda_Show Char
instance NLambda_Show Bool
instance NLambda_Show ()
instance (NLambda_Show a, NLambda_Show b) => NLambda_Show (a, b)
instance (NLambda_Show a, NLambda_Show b, NLambda_Show c) => NLambda_Show (a, b, c)
instance (NLambda_Show a, NLambda_Show b, NLambda_Show c, NLambda_Show d) => NLambda_Show (a, b, c, d)
instance (NLambda_Show a, NLambda_Show b, NLambda_Show c, NLambda_Show d, NLambda_Show e) => NLambda_Show (a, b, c, d, e)
instance (NLambda_Show a, NLambda_Show b, NLambda_Show c, NLambda_Show d, NLambda_Show e, NLambda_Show f) => NLambda_Show (a, b, c, d, e, f)
instance (NLambda_Show a, NLambda_Show b, NLambda_Show c, NLambda_Show d, NLambda_Show e, NLambda_Show f, NLambda_Show g) => NLambda_Show (a, b, c, d, e, f, g)

nlambda_showList__ :: (WithMeta a -> ShowS) ->  WithMeta [a] -> ShowS
nlambda_showList__ f (WithMeta xs m) = showList__ (metaFun m f) xs
