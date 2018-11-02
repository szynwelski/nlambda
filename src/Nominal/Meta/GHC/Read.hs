module Nominal.Meta.GHC.Read where

import GHC.Read
import Nominal.Meta
import Nominal.Variable
import Text.Read.Lex (Lexeme)
import Text.ParserCombinators.ReadPrec (ReadPrec)

class Read a => NLambda_Read a where
    nlambda_readsPrec :: Int -> String -> WithMeta [(a, String)]
    nlambda_readsPrec n = noMeta . readsPrec n
    nlambda_readList :: String -> WithMeta [([a], String)]
    nlambda_readList = noMeta . readList
    nlambda_readPrec :: WithMeta (ReadPrec a)
    nlambda_readPrec = noMeta readPrec
    nlambda_readListPrec :: WithMeta (ReadPrec [a])
    nlambda_readListPrec = noMeta readListPrec

instance NLambda_Read a => NLambda_Read [a]
instance NLambda_Read Word
instance NLambda_Read Ordering
instance NLambda_Read a => NLambda_Read (Maybe a)
instance NLambda_Read Integer
instance NLambda_Read Int
instance NLambda_Read Float
instance NLambda_Read Double
instance NLambda_Read Char
instance NLambda_Read Bool
instance NLambda_Read ()
instance (NLambda_Read a, NLambda_Read b) => NLambda_Read (a, b)
instance (NLambda_Read a, NLambda_Read b, NLambda_Read c) => NLambda_Read (a, b, c)

nlambda_expectP :: Lexeme -> WithMeta (ReadPrec ())
nlambda_expectP = noMeta . expectP

nlambda_lexP :: WithMeta (ReadPrec Lexeme)
nlambda_lexP = noMeta lexP

nlambda_parens :: WithMeta (ReadPrec a) -> WithMeta (ReadPrec a)
nlambda_parens = idOp parens

nlambda_readListDefault :: Read a => WithMeta (ReadS [a])
nlambda_readListDefault = noMeta readListDefault

nlambda_readListPrecDefault :: Read a => WithMeta (ReadPrec [a])
nlambda_readListPrecDefault = noMeta readListPrecDefault
