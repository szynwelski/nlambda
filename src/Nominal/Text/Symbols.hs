{-# LANGUAGE CPP #-}
module Nominal.Text.Symbols where

import Numeric (showIntAtBase)
import Text.Read (ReadPrec, lift, readPrec)
import Text.Read.Lex (readIntP)
import Text.ParserCombinators.ReadP (char)

-- I have put the symbols into a single file
-- This way, we only need CPP here, and not
-- sprinkled thoughout the library

for, inSet, atoms, lt, leq, gt, geq, eq, neq, not, or, and, valueCondSep, variantsSep :: String
subscriptIndex :: Int -> String
readSubscriptIndex :: ReadPrec Int

for = "for"
valueCondSep = ":"
variantsSep = "|"

#ifdef DISABLE_UNICODE

inSet = "in"
atoms = "A"

lt  = "<"
leq = "<="
eq  = "="
neq = "/="
gt  = ">"
geq = ">="

not = "~"
or = "\\/"
and = "/\\"

subscriptIndex n = "_" ++ show n
readSubscriptIndex =
    do lift (char '_')
       readPrec

#else

inSet = "∊"
atoms = "𝔸"

lt  = "<"
leq = "≤"
eq  = "="
neq = "≠"
gt  = ">"
geq = "≥"

not = "¬"
or = "∨"
and = "∧"

fromSubscript :: Char -> Int
fromSubscript c = fromEnum c - 8320

isSubscript :: Char -> Bool
isSubscript c = let i = fromSubscript c in i >= 0 && i < 10

subscriptIndex n = showIntAtBase 10 (toEnum . (+8320)) n ""
readSubscriptIndex = lift $ readIntP 10 isSubscript fromSubscript

#endif
