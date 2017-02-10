module Nominal.Formula.Definition where

import Data.List.Utils (join)
import Data.Set (Set, elems, fromList)
import Nominal.Atoms.Signature (Relation)
import qualified Nominal.Text.Symbols as Symbols
import Nominal.Util.Read (readSepBy, skipSpaces, spaces, string)
import Nominal.Variable (Variable)
import Text.Read (Lexeme(Ident), Prec, ReadPrec, (+++), lexP, parens, pfail, prec, readPrec, step)


----------------------------------------------------------------------------------------------------
-- Formula structure
----------------------------------------------------------------------------------------------------

data FormulaStructure
    = F
    | T
    | Constraint Relation Variable Variable
    | And (Set Formula)
    | Or (Set Formula)
    | Not Formula deriving (Eq, Ord)

----------------------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------------------

showSubformula :: FormulaStructure -> String
showSubformula f@(And fs) = "(" ++ show f ++ ")"
showSubformula f@(Or fs) = "(" ++ show f ++ ")"
showSubformula f = show f

instance Show FormulaStructure where
    show F = "false"
    show T = "true"
    show (Constraint r x1 x2) = show x1 ++ spaces (show r) ++ show x2
    show (And fs) = join (spaces Symbols.and) ((showSubformula . formula) <$> elems fs)
    show (Or fs) = join (spaces Symbols.or) ((showSubformula . formula) <$> elems fs)
    show (Not f) = Symbols.not ++ "(" ++ show f ++ ")"

----------------------------------------------------------------------------------------------------
-- Read
----------------------------------------------------------------------------------------------------

readAndOr :: Prec -> (Set Formula -> FormulaStructure) -> String -> ReadPrec FormulaStructure
readAndOr pr constr symbol = do fs <- prec pr (readSepBy False symbol readFormula)
                                return $ constr $ fromList $ fmap (Formula False) fs

readAnd :: ReadPrec FormulaStructure
readAnd = readAndOr 3 And Symbols.and

readOr :: ReadPrec FormulaStructure
readOr = readAndOr 4 Or Symbols.or

readLit :: ReadPrec FormulaStructure
readLit = do Ident s <- lexP
             case s of
               "false" -> return F
               "true"  -> return T
               _       -> pfail

readNot :: ReadPrec FormulaStructure
readNot = prec 5 (do skipSpaces
                     string Symbols.not
                     f <- step readFormula
                     return $ Not $ Formula False f)

readConstraint :: ReadPrec FormulaStructure
readConstraint = do x <- readPrec
                    skipSpaces
                    r <- readPrec
                    skipSpaces
                    y <- readPrec
                    return $ Constraint r x y

readFormula :: ReadPrec FormulaStructure
readFormula = do f <- parens $ readLit +++ readConstraint +++ readNot +++ readAnd +++ readOr
                 skipSpaces
                 return f

instance Read FormulaStructure where
    readPrec = readFormula
----------------------------------------------------------------------------------------------------
-- Formula
----------------------------------------------------------------------------------------------------

-- | First order formula with free variables and relations between variables.
data Formula = Formula {isSimplified :: Bool, formula :: FormulaStructure}

instance Eq Formula where
    (Formula _ f1) == (Formula _ f2) = f1 == f2

instance Ord Formula where
    compare (Formula _ f1) (Formula _ f2) = compare f1 f2

instance Show Formula where
    show (Formula _ f) = show f

instance Read Formula where
    readPrec = do f <- step readPrec
                  return (Formula False f)
