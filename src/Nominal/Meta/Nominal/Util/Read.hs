module Nominal.Meta.Nominal.Util.Read where

import Nominal.Util.Read
import Nominal.Meta
import Text.ParserCombinators.ReadPrec (ReadPrec)

nlambda_optional :: WithMeta (ReadPrec a) -> WithMeta (ReadPrec ())
nlambda_optional = idOp optional

nlambda_skipSpaces :: WithMeta (ReadPrec ())
nlambda_skipSpaces = noMeta skipSpaces

nlambda_spaces :: String -> String
nlambda_spaces = spaces

nlambda_string :: String -> WithMeta (ReadPrec String)
nlambda_string = noMetaArgOp string

nlambda_readSepBy :: Bool -> String -> WithMeta (ReadPrec a) -> WithMeta (ReadPrec [a])
nlambda_readSepBy b s = noMeta . readSepBy b s . value
