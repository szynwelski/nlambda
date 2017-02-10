{-# LANGUAGE CPP, MultiParamTypeClasses #-}
module Nominal.Atoms.Signature where

import Nominal.Text.Symbols
import Text.ParserCombinators.ReadP (choice, string)
import Text.Read (ReadPrec, (<++), lift, parens, readPrec, step)

#if TOTAL_ORDER
import GHC.Read (expectP)
import Data.Ratio ((%), denominator, numerator)
import Data.String.Utils (replace)
import Text.Read.Lex (Lexeme(Symbol))
#endif



----------------------------------------------------------------------------------------------------
-- Relation
----------------------------------------------------------------------------------------------------
data Relation = LessThan | LessEquals | Equals | NotEquals | GreaterEquals | GreaterThan deriving (Eq, Ord, Enum)

relations :: [Relation]
relations = [LessThan ..]

symmetricRelation :: Relation -> Relation
symmetricRelation LessThan = GreaterThan
symmetricRelation LessEquals = GreaterEquals
symmetricRelation GreaterThan = LessThan
symmetricRelation GreaterEquals = LessEquals
symmetricRelation Equals = Equals
symmetricRelation NotEquals = NotEquals

relationFunction :: Relation -> (Constant -> Constant -> Bool)
relationFunction LessThan = (<)
relationFunction LessEquals = (<=)
relationFunction GreaterThan = (>)
relationFunction GreaterEquals = (>=)
relationFunction Equals = (==)
relationFunction NotEquals = (/=)

----------------------------------------------------------------------------------------------------
-- Show and Read
----------------------------------------------------------------------------------------------------

instance Show Relation where
    show LessThan      = lt
    show LessEquals    = leq
    show Equals        = eq
    show NotEquals     = neq
    show GreaterThan   = gt
    show GreaterEquals = geq

relationAscii :: Relation -> String
relationAscii LessThan = "<"
relationAscii LessEquals = "<="
relationAscii Equals = "="
relationAscii NotEquals = "/="
relationAscii GreaterThan = ">"
relationAscii GreaterEquals = ">="

instance Read Relation where
    readPrec = parens $ lift $ choice [string lt  >> return LessThan,
                                       string leq >> return LessEquals,
                                       string eq  >> return Equals,
                                       string neq >> return NotEquals,
                                       string gt  >> return GreaterThan,
                                       string geq >> return GreaterEquals]

----------------------------------------------------------------------------------------------------
-- Atoms signature
----------------------------------------------------------------------------------------------------

class AtomsSignature where

    -- | Minimum list of relations from signature of atoms type
    minRelations :: [Relation]

    -- | Returns text representation of given constant
    showConstant :: Constant -> String

    -- | Returns constant for text representation
    readConstant :: ReadPrec Constant

    -- | Default constant value
    defaultConstant :: Constant

----------------------------------------------------------------------------------------------------
-- Current atoms type
----------------------------------------------------------------------------------------------------

#if TOTAL_ORDER

type Constant = Rational
instance AtomsSignature where
    minRelations = [Equals, LessThan]
    showConstant x = let (n,d) = (numerator x, denominator x)
                                          in if d == 1 then show n else show n ++ "/" ++ show d
    readConstant = parens
        (do x <- step readPrec
            expectP (Symbol "/")
            y <- step readPrec
            return (x % y)
         <++
         do x <- step readPrec
            return (x % 1)
        )
    defaultConstant = 0

#else

type Constant = Integer
instance AtomsSignature where
    minRelations = [Equals]
    showConstant = show
    readConstant = readPrec
    defaultConstant = 0

#endif
