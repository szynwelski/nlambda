module Nominal.Formula.Solver (isTrue, isFalse, lia, lra, simplifyFormula) where

import Data.Char (isSpace)
import Data.Functor.Identity (Identity)
import Data.List (find, takeWhile)
import Data.List.Utils (split)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Set (Set, elems, fromList, null)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder
import Nominal.Atoms.Signature (Relation(..), relationAscii, relations)
import Nominal.Formula.Constructors
import Nominal.Formula.Definition
import Nominal.Formula.Operators
import Nominal.Variable (Variable, constantValue, constantVar, fromVariableNameAscii, isConstant, variableName, variableNameAscii)
import Prelude hiding (and, map, null, or)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Text.Parsec

----------------------------------------------------------------------------------------------------
-- SmtLogic
----------------------------------------------------------------------------------------------------

data SmtLogic = SmtLogic {sort :: String, logic :: String, constantToSmt :: String -> String,
                          parseConstant :: ParsecT String () Identity Variable}

parseInt :: ParsecT String () Identity Variable
parseInt = do
    n <- many digit
    _ <- string ".0"
    return $ constantVar n

lia :: SmtLogic
lia = SmtLogic "Int" "LIA" id parseInt

ratioToSmt :: String -> String
ratioToSmt r = let rs = split "/" r in if length rs == 1 then r else "(/ " ++ (rs !! 0) ++ " " ++ (rs !! 1) ++ ")"

parseRatio :: ParsecT String () Identity Variable
parseRatio = do
    _ <- string "(/ "
    x <- many digit
    _ <- string ".0 "
    y <- many digit
    _ <- string ".0)"
    return $ constantVar $ x ++ "/" ++ y

lra :: SmtLogic
lra = SmtLogic "Real" "LRA" ratioToSmt parseRatio

----------------------------------------------------------------------------------------------------
-- SMT Solver
----------------------------------------------------------------------------------------------------

data SmtSolver = SmtSolver {command :: String, options :: [String], smtOptions :: [String]}

z3Solver :: SmtSolver
z3Solver = SmtSolver {command = "z3", options = ["-smt2", "-in", "-nw"],
                      smtOptions = ["(set-option :smt.auto-config false)",
                                    "(set-option :smt.mbqi false)",
                                    "(set-option :pp.min-alias-size 1000000)",
                                    "(set-option :pp.max-depth 1000000)"]}

isNotSatisfiable :: String -> Bool
isNotSatisfiable = (== "unsat") . filter (Prelude.not . isSpace)

type SmtScript = String

runSolver :: SmtSolver -> SmtScript -> String
runSolver solver script = unsafePerformIO $ do
    exec <- findExecutable (command solver)
    (case exec of
      Nothing       -> return $ error ("SMT Solver \""
                              ++ (command solver)
                              ++ "\" is not installed or is not added to PATH.")
      Just execPath -> do (exit, out, err) <- readProcessWithExitCode execPath (options solver)
                                                (concat (smtOptions solver) ++ script)
                          return (case exit of
                            ExitSuccess -> out
                            ExitFailure code -> error $ unlines ["SMT Solver " ++ (command solver) ++ " exits with code: "
                               ++ show code, "input: " ++ script, "output: " ++ out, "error: " ++ err]))

----------------------------------------------------------------------------------------------------
-- SMT-LIB script
----------------------------------------------------------------------------------------------------

variableToSmt :: SmtLogic -> Variable -> String
variableToSmt l x = if isConstant x then constantToSmt l $ constantValue x else variableNameAscii x

getSmtAssertOp :: SmtLogic -> String -> [Formula] -> Builder
getSmtAssertOp l op fs =
    singleton '('
    <> fromString op
    <> singleton ' '
    <> (foldl1 (<>) $ fmap (getSmtAssert l) fs)
    <> singleton ')'

getSmtAssert :: SmtLogic -> Formula -> Builder
getSmtAssert l (Formula _ f) = getAssert l f
    where getAssert _ T = fromString " true "
          getAssert _ F = fromString " false "
          getAssert _ (Constraint NotEquals x1 x2) =
            fromString "(not (= "
            <> fromString (variableToSmt l x1)
            <> singleton ' '
            <> fromString (variableToSmt l x2)
            <> fromString "))"
          getAssert _ (Constraint r x1 x2) =
            singleton '('
            <> fromString (relationAscii r)
            <> singleton ' '
            <> fromString (variableToSmt l x1)
            <> singleton ' '
            <> fromString (variableToSmt l x2)
            <> singleton ')'
          getAssert l (And fs) = getSmtAssertOp l "and" $ elems fs
          getAssert l (Or fs) = getSmtAssertOp l "or" $ elems fs
          getAssert l (Not f) = getSmtAssertOp l "not" [f]

getSmtAssertForAllFree :: SmtLogic -> Formula -> SmtScript
getSmtAssertForAllFree l f =
  unpack $ toLazyText $
  let fvs = freeVariables f
  in (if null fvs
      then fromString ""
      else foldl1 (<>) (fmap (\x -> fromString "(declare-const "
                                    <> fromString (variableNameAscii x)
                                    <> singleton ' '
                                    <> fromString (sort l)
                                    <> singleton ')') (elems fvs)))
     <> fromString "(assert "
     <> (getSmtAssert l f)
     <> singleton ')'

getSmtScript :: String -> SmtLogic -> Formula -> SmtScript
getSmtScript check l f = "(set-logic " ++ logic l ++ ")" ++ (getSmtAssertForAllFree l f) ++ check

checkSatScript :: SmtLogic -> Formula -> SmtScript
checkSatScript = getSmtScript "(check-sat)"

simplifyScript :: SmtLogic -> Formula -> SmtScript
simplifyScript = getSmtScript "(apply ctx-solver-simplify)"

----------------------------------------------------------------------------------------------------
-- Formula solving
----------------------------------------------------------------------------------------------------

isTrue :: SmtLogic -> Formula -> Bool
isTrue l (Formula _ T) = True
isTrue l (Formula _ F) = False
isTrue l f = isNotSatisfiable $ runSolver z3Solver $ checkSatScript l (Formula False $ Not f)

isFalse :: SmtLogic -> Formula -> Bool
isFalse _ (Formula _ T) = False
isFalse _ (Formula _ F) = True
isFalse l f = isTrue l (Formula False $ Not f)

simplifyFormula :: SmtLogic -> Formula -> Formula
simplifyFormula _ (Formula _ T) = true
simplifyFormula _ (Formula _ F) = false
simplifyFormula _ f@(Formula True _) = f
simplifyFormula l f = parseSimplifiedFormula l $ runSolver z3Solver $ simplifyScript l f

----------------------------------------------------------------------------------------------------
-- Parser of the result of simplification
----------------------------------------------------------------------------------------------------

parseSimplifiedFormula :: SmtLogic -> String -> Formula
parseSimplifiedFormula l output =
  case parse (parseGoals l) "" output of
    Left e -> error (show e)
    Right f -> f

parseGoals :: SmtLogic -> ParsecT String () Identity Formula
parseGoals l = do
    _ <- string "(goals"
    _ <- spaces
    f <- parseGoal l
    _ <- char ')'
    return f

parseGoal :: SmtLogic -> ParsecT String () Identity Formula
parseGoal l = do
    _ <- string "(goal"
    _ <- spaces
    fs <- many $ parseFormula l
    _ <- spaces
    _ <- parseOptions
    _ <- char ')'
    _ <- spaces
    return $ case fs of
               []        -> true
               otherwise -> Formula True (And $ fromList fs)

parseOptions :: ParsecT String () Identity [(String, String)]
parseOptions = many parseOption

parseOption :: ParsecT String () Identity (String, String)
parseOption = try parsePrecision <|> try parseDepth

parsePrecision :: ParsecT String () Identity (String, String)
parsePrecision = do
    k <- string ":precision"
    _ <- spaces
    v <- string "precise"
    _ <- spaces
    return (k,v)

parseDepth :: ParsecT String () Identity (String, String)
parseDepth = do
    k <- string ":depth"
    _ <- spaces
    v <- many digit
    _ <- spaces
    return (k,v)

parseFormula :: SmtLogic -> ParsecT String () Identity Formula
parseFormula l = try parseTrue <|> try parseFalse <|> try (parseConstraint l)
                 <|> try (parseNot l) <|> try (parseAnd l) <|> try (parseOr l)

parseTrue :: ParsecT String () Identity Formula
parseTrue = do
    _ <- string "true"
    _ <- spaces
    return true

parseFalse :: ParsecT String () Identity Formula
parseFalse = do
    _ <- string "false"
    _ <- spaces
    return false

parseConstraint :: SmtLogic -> ParsecT String () Identity Formula
parseConstraint l = do
    _ <- char '('
    r <- parseRelation
    _ <- spaces
    x <- parseVariable <|> parseConstant l
    _ <- spaces
    y <- parseVariable <|> parseConstant l
    _ <- char ')'
    _ <- spaces
    return $ Formula True (Constraint r x y)

parseRelation :: ParsecT String () Identity Relation
parseRelation = do
    r <- many (oneOf "=<>")
    return $ fromJust $ find (\rel -> r == relationAscii rel) relations

parseVariable :: ParsecT String () Identity Variable
parseVariable = do
    x <- many $ alphaNum <|> char '_' <|> char '-'
    return $ fromVariableNameAscii x

parseNot :: SmtLogic -> ParsecT String () Identity Formula
parseNot l = do
    _ <- string "(not"
    _ <- spaces
    f <- parseFormula l
    _ <- char ')'
    _ <- spaces
    return $ Nominal.Formula.Operators.not f

parseAnd :: SmtLogic -> ParsecT String () Identity Formula
parseAnd l = do
    _ <- string "(and"
    _ <- spaces
    fs <- many $ try $ parseFormula l
    _ <- char ')'
    _ <- spaces
    return $ Formula True (And $ fromList fs)

parseOr :: SmtLogic -> ParsecT String () Identity Formula
parseOr l = do
    _ <- string "(or"
    _ <- spaces
    fs <- many $ parseFormula l
    _ <- char ')'
    _ <- spaces
    return $ Formula True (Or $ fromList fs)
