{-# LANGUAGE CPP #-}

module Nominal.Formula.Solver (isTrue, isFalse, lia, lra, model, simplifyFormula) where

import Control.Applicative ((<|>), (*>), (<*))
import Data.Attoparsec.ByteString.Char8 (Parser, char, digit, isDigit, many1, satisfy, sepBy, sepBy1, skipWhile, string, takeWhile, takeWhile1)
import Data.Attoparsec.ByteString.Lazy (Result(Done, Fail), parse)
import Data.ByteString.Builder (Builder, char8, intDec, string8, toLazyByteString, wordDec)
import qualified Data.ByteString.Char8 as S -- strict
import qualified Data.ByteString.Lazy.Char8 as L -- lazy
import Data.Char (isSpace, isLetter)
import Data.List (find)
import Data.List.Utils (split)
import Data.Map (Map, empty, fromList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Set (elems, null)
import Data.Word (Word)
import Nominal.Atoms.Signature (Constant, Relation(..), readConstant, relationAscii, relations)
import Nominal.Formula.Constructors
import Nominal.Formula.Definition
import Nominal.Formula.Operators
import Nominal.Variable (Variable, constantValue, constantVar, fromParts, isConstant, isVariableChar, toParts)
import Prelude hiding (null, takeWhile)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.IO.Unsafe (unsafePerformIO)
import System.Process.ByteString.Lazy (readProcessWithExitCode)

----------------------------------------------------------------------------------------------------
-- SmtLogic
----------------------------------------------------------------------------------------------------

data SmtLogic = SmtLogic {sort :: String, logic :: String, constantToSmt :: Constant -> SmtScript,
                          parseConstant :: Parser Variable}

signed :: Parser String -> Parser Variable
signed parser = do
    n <- (('-' :) <$> (text "(-" *> spaces *> parser <* spaces <* char ')')) <|> parser
    return $ constantVar $ read n

lia :: SmtLogic
lia = SmtLogic "Int" "LIA" (string8 . show) (signed $ many1 digit)

ratioToSmt :: Constant -> SmtScript
ratioToSmt c = let r = show c
                   rs = split "%" r
               in if length rs == 1
                  then string8 r
                  else string8 "(/ " <> string8 (head rs) <> char8 ' ' <> string8 (rs !! 1) <> char8 ')'

parseRatio :: Parser String
parseRatio = do
    text "(/"
    spaces
    x <- many1 digit
    text ".0"
    spaces
    y <- many1 digit
    text ".0"
    char ')'
    return $ x ++ "/" ++ y


#if TOTAL_ORDER

signed2 :: Parser String -> Parser Variable
signed2 parser = do
    n <- (('-' :) <$> (text "(-" *> spaces *> parser <* spaces <* char ')')) <|> parser
    return $ constantVar $ toRational (read n :: Double)

#else

signed2 :: Parser String -> Parser Variable
signed2 = signed

#endif

lra :: SmtLogic
lra = SmtLogic "Real" "LRA" ratioToSmt (signed2 ((many1 digit <* text ".0") <|> parseRatio))

----------------------------------------------------------------------------------------------------
-- SMT Solver
----------------------------------------------------------------------------------------------------

data SmtSolver = SmtSolver {executable :: FilePath, options :: [String], smtOptions :: [String]}

getExecutable :: String -> FilePath
getExecutable command = unsafePerformIO $ do
    path <- findExecutable command
    return $ fromMaybe (error ("SMT Solver \"" ++ command ++ "\" is not installed or is not added to PATH.")) path

z3Solver :: SmtSolver
z3Solver = SmtSolver {executable = getExecutable "z3", options = ["-smt2", "-in", "-nw"],
                      smtOptions = ["(set-option :smt.auto-config false)",
                                    "(set-option :smt.mbqi false)",
                                    "(set-option :pp.min-alias-size 1000000)",
                                    "(set-option :pp.max-depth 1000000)"]}
type SmtScript = Builder
type SmtResult = L.ByteString

runSolver :: SmtSolver -> SmtScript -> SmtResult
runSolver solver script = unsafePerformIO $ do
    (exit, out, err) <- readProcessWithExitCode (executable solver) (options solver)
                          (toLazyByteString $ mconcat (fmap string8 (smtOptions solver)) <> script)
    return $ case exit of
               ExitSuccess      -> out
               ExitFailure code -> error $ unlines ["SMT Solver " ++ show (executable solver) ++ " exits with code: " ++ show code,
                                                    "input: " ++ show (toLazyByteString script),
                                                    "output: " ++ show out,
                                                    "error: " ++ show err]

----------------------------------------------------------------------------------------------------
-- SMT-LIB script
----------------------------------------------------------------------------------------------------

variableToSmt :: Variable -> SmtScript
variableToSmt v =
    case toParts v of
      Left name -> string8 name
      Right (level, index, id) -> char8 'v' <> intDec level <> char8 '_' <> intDec index <> char8 '_' <> maybe mempty wordDec id

toSmt :: SmtLogic -> Variable -> SmtScript
toSmt l x = if isConstant x
            then constantToSmt l $ constantValue x
            else variableToSmt x

getSmtAssertOp :: SmtLogic -> String -> [Formula] -> SmtScript
getSmtAssertOp l op fs =
    char8 '('
    <> string8 op
    <> char8 ' '
    <> mconcat (fmap (getSmtAssert l) fs)
    <> char8 ')'

getSmtAssert :: SmtLogic -> Formula -> SmtScript
getSmtAssert l (Formula _ f) = getAssert l f
    where getAssert _ T = string8 " true "
          getAssert _ F = string8 " false "
          getAssert _ (Constraint NotEquals x1 x2) =
            string8 "(not (= "
            <> toSmt l x1
            <> char8 ' '
            <> toSmt l x2
            <> string8 "))"
          getAssert _ (Constraint r x1 x2) =
            char8 '('
            <> string8 (relationAscii r)
            <> char8 ' '
            <> toSmt l x1
            <> char8 ' '
            <> toSmt l x2
            <> char8 ')'
          getAssert l (And fs) = getSmtAssertOp l "and" $ elems fs
          getAssert l (Or fs) = getSmtAssertOp l "or" $ elems fs
          getAssert l (Not f) = getSmtAssertOp l "not" [f]

getSmtAssertForAllFree :: SmtLogic -> Formula -> SmtScript
getSmtAssertForAllFree l f =
  let fvs = freeVariables f
  in (if null fvs
      then mempty
      else mconcat (fmap (\x -> string8 "(declare-const "
                               <> variableToSmt x
                               <> char8 ' '
                               <> string8 (sort l)
                               <> char8 ')') (elems fvs)))
     <> string8 "(assert "
     <> getSmtAssert l f
     <> char8 ')'

getSmtScript :: String -> SmtLogic -> Formula -> SmtScript
getSmtScript check l f =
    string8 "(set-logic "
    <> string8 (logic l)
    <> char8 ')'
    <> getSmtAssertForAllFree l f
    <> string8 check

checkSatScript :: SmtLogic -> Formula -> SmtScript
checkSatScript = getSmtScript "(check-sat)"

isNotSatisfiable :: SmtResult -> Bool
isNotSatisfiable = (== "unsat") . filter (Prelude.not . isSpace) . L.unpack

simplifyScript :: SmtLogic -> Formula -> SmtScript
simplifyScript = getSmtScript "(apply ctx-solver-simplify)"

getModelScript :: SmtLogic -> Formula -> SmtScript
getModelScript = getSmtScript "(check-sat)(get-model)"

----------------------------------------------------------------------------------------------------
-- Formula solving
----------------------------------------------------------------------------------------------------

isTrue :: SmtLogic -> Formula -> Bool
isTrue _ (Formula _ T) = True
isTrue _ (Formula _ F) = False
isTrue _ (Formula True _ ) = False
isTrue l f = isNotSatisfiable $ runSolver z3Solver $ checkSatScript l (Formula False $ Not f)

isFalse :: SmtLogic -> Formula -> Bool
isFalse _ (Formula _ T) = False
isFalse _ (Formula _ F) = True
isFalse _ (Formula True _ ) = False
isFalse l f = isNotSatisfiable $ runSolver z3Solver $ checkSatScript l f

simplifyFormula :: SmtLogic -> Formula -> Formula
simplifyFormula _ (Formula _ T) = true
simplifyFormula _ (Formula _ F) = false
simplifyFormula _ f@(Formula True _) = f
simplifyFormula l f = parseSimplifiedFormula l $ runSolver z3Solver $ simplifyScript l f

model :: SmtLogic -> Formula -> Map Variable Variable
model _ (Formula _ T) = empty
model l f = if isFalse l f
            then error "No model for unsatisfied formula."
            else parseModelResult l $ runSolver z3Solver $ getModelScript l f

----------------------------------------------------------------------------------------------------
-- Parser of the result of simplification
----------------------------------------------------------------------------------------------------

toInt :: S.ByteString -> Int
toInt s = fst $ fromMaybe (error $ "input is not a number: " ++ show s) $ S.readInt s

toWord :: S.ByteString -> Word
toWord = fromIntegral . toInt

text :: String -> Parser S.ByteString
text = string . S.pack

spaces :: Parser ()
spaces = skipWhile isSpace

parseError :: SmtResult -> [String] -> String -> a
parseError rest ctx e = error $ unlines ["Fail to parse SMT Solver output:",
                                        "- not parsed output: " ++ show rest,
                                        "- list of contexts in which the error occurred: " ++ show ctx,
                                        "- error message: " ++ show e]

parseSimplifiedFormula :: SmtLogic -> SmtResult -> Formula
parseSimplifiedFormula l output =
  case parse (parseGoals l) output of
    Fail rest ctx e -> parseError rest ctx e
    Done _ f -> f

parseGoals :: SmtLogic -> Parser Formula
parseGoals l = do
    text "(goals"
    spaces
    f <- parseGoal l
    spaces
    char ')'
    return f

parseGoal :: SmtLogic -> Parser Formula
parseGoal l = do
    text "(goal"
    spaces
    fs <- parseFormula l `sepBy` spaces
    spaces
    parseOptions
    char ')'
    return $ case fs of
               []  -> true
               [f] -> f
               _   -> simplifiedAnd fs

parseOptions :: Parser [(S.ByteString, S.ByteString)]
parseOptions = parseOption `sepBy` spaces

parseOption :: Parser (S.ByteString, S.ByteString)
parseOption = parsePrecision <|> parseDepth

parsePrecision :: Parser (S.ByteString, S.ByteString)
parsePrecision = do
    k <- text ":precision"
    spaces
    v <- text "precise"
    return (k,v)

parseDepth :: Parser (S.ByteString, S.ByteString)
parseDepth = do
    k <- text ":depth"
    spaces
    v <- many1 digit
    return (k, S.pack v)

parseFormula :: SmtLogic -> Parser Formula
parseFormula l = parseTrue <|> parseFalse <|> parseConstraint l <|> parseNot l <|> parseAnd l <|> parseOr l

parseTrue :: Parser Formula
parseTrue = text "true" *> return true

parseFalse :: Parser Formula
parseFalse = text "false" *> return false

parseConstraint :: SmtLogic -> Parser Formula
parseConstraint l = do
    char '('
    r <- parseRelation
    spaces
    x <- parseIterationVariable <|> parseVariable <|> parseConstant l
    spaces
    y <- parseIterationVariable <|> parseVariable <|> parseConstant l
    char ')'
    return $ Formula True (Constraint r x y)

parseRelation :: Parser Relation
parseRelation = do
    r <- takeWhile1 (\c -> c == '=' || c == '<' || c == '>')
    return $ fromMaybe (error $ "unknown relation: " ++ show r) $ find (\rel -> S.unpack r == relationAscii rel) relations

parseVariable :: Parser Variable
parseVariable = do
    x <- satisfy isLetter
    y <- takeWhile isVariableChar
    return $ fromParts $ Left $ x : S.unpack y

parseIterationVariable :: Parser Variable
parseIterationVariable = do
    char 'v'
    x <- takeWhile1 isDigit
    char '_'
    y <- takeWhile1 isDigit
    char '_'
    z <- takeWhile isDigit
    return $ fromParts $ Right (toInt x, toInt y, if S.null z then Nothing else Just $ toWord z)

parseNot :: SmtLogic -> Parser Formula
parseNot l = do
    text  "(not"
    spaces
    f <- parseFormula l
    char ')'
    return $ Nominal.Formula.Operators.not f

parseAnd :: SmtLogic -> Parser Formula
parseAnd l = do
    text "(and"
    spaces
    fs <- parseFormula l `sepBy1` spaces
    char ')'
    return $ simplifiedAnd fs

parseOr :: SmtLogic -> Parser Formula
parseOr l = do
    text "(or"
    spaces
    fs <- parseFormula l `sepBy1` spaces
    char ')'
    return $ simplifiedOr fs

----------------------------------------------------------------------------------------------------
-- Parser of the model for formula
----------------------------------------------------------------------------------------------------

parseModelResult :: SmtLogic -> SmtResult -> Map Variable Variable
parseModelResult  l output =
  case parse (parseModelOutput l) output of
    Fail rest ctx e -> parseError rest ctx e
    Done _ m -> m

parseModelOutput :: SmtLogic -> Parser (Map Variable Variable)
parseModelOutput l = do
    text "sat"
    spaces
    parseModel l

parseModel :: SmtLogic -> Parser (Map Variable Variable)
parseModel l = do
    text "(model"
    spaces
    vs <- parseModelVariable l `sepBy1` spaces
    spaces
    char ')'
    return $ fromList vs

parseModelVariable l = do
    text "(define-fun"
    spaces
    v <- parseIterationVariable <|> parseVariable
    spaces
    text "()"
    spaces
    text (sort l)
    spaces
    c <- parseConstant l
    spaces
    char ')'
    return (v,c)
