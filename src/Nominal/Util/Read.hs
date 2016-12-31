module Nominal.Util.Read where

import Text.Read (ReadPrec, (+++), lift, step)
import qualified Text.ParserCombinators.ReadP as ReadP

optional :: ReadPrec a -> ReadPrec ()
optional p = (p >> return ()) +++ return ()

skipSpaces :: ReadPrec ()
skipSpaces = lift ReadP.skipSpaces

spaces :: String -> String
spaces str = " " ++ str ++ " "

string :: String -> ReadPrec String
string = lift . ReadP.string

readSepBy :: Bool -> String -> ReadPrec a -> ReadPrec [a]
readSepBy allowSingleton sep p = list allowSingleton
  where list allowSingleton = do x  <- step p
                                 xs <- rest allowSingleton
                                 return (x:xs)
        rest allowSingleton = if allowSingleton then sepAndRest +++ return [] else sepAndRest
        sepAndRest = do string sep
                        list True
