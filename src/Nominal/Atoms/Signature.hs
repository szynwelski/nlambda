{-# LANGUAGE CPP, NullaryTypeClasses #-}
module Nominal.Atoms.Signature where

import Data.Ratio (denominator, numerator)
import Data.String.Utils (replace)

----------------------------------------------------------------------------------------------------
-- Relation
----------------------------------------------------------------------------------------------------
data Relation = LessThan | LessEquals | Equals | NotEquals | GreaterEquals | GreaterThan deriving (Eq, Ord, Enum)

instance Show Relation where
    show LessThan = "<"
    show LessEquals = "≤"
    show Equals = "="
    show NotEquals = "≠"
    show GreaterThan = ">"
    show GreaterEquals = "≥"

relationAscii :: Relation -> String
relationAscii LessThan = "<"
relationAscii LessEquals = "<="
relationAscii Equals = "="
relationAscii NotEquals = "/="
relationAscii GreaterThan = ">"
relationAscii GreaterEquals = ">="

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

checkConstants :: Relation -> String -> String -> Bool
checkConstants r x y = (relationFunction r) (readConstant x) (readConstant y)

----------------------------------------------------------------------------------------------------
-- Atoms signature
----------------------------------------------------------------------------------------------------

class AtomsSignature where

    -- | Minimum list of relations from signature of atoms type
    minRelations :: [Relation]

    -- | Returns text representation of given constant
    showConstant :: Constant -> String

    -- | Returns constant for text representation
    readConstant :: String -> Constant

----------------------------------------------------------------------------------------------------
-- Current atoms type
----------------------------------------------------------------------------------------------------

#if TOTAL_ORDER

type Constant = Rational
instance AtomsSignature where
    minRelations = [Equals, LessThan]
    showConstant x = let (n,d) = (numerator x, denominator x)
                                          in if d == 1 then show n else show n ++ "/" ++ show d
    readConstant x = if elem '/' x then read $ replace "/" "%" x else read $ x ++ "%1"

#else

type Constant = Integer
instance AtomsSignature where
    minRelations = [Equals]
    showConstant = show
    readConstant = read

#endif
