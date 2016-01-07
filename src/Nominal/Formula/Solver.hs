module Nominal.Formula.Solver (isTrue, isFalse, lia, lra) where

import Data.Set (Set, elems, empty, map, member, null, singleton, unions)
import Nominal.Formula.Constructors
import Nominal.Formula.Definition
import Nominal.Variable (Variable, variableNameAscii)
import Prelude hiding (map, null)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

----------------------------------------------------------------------------------------------------
-- SmtLogic
----------------------------------------------------------------------------------------------------

data SmtLogic = SmtLogic {sort :: String, logic :: String} deriving (Eq, Ord)

lia :: SmtLogic
lia = SmtLogic "Int" "LIA"

lra :: SmtLogic
lra = SmtLogic "Real" "LRA"

----------------------------------------------------------------------------------------------------
-- SMT Solver
----------------------------------------------------------------------------------------------------

data SmtSolver = SmtSolver {command :: String, options :: [String], smtOptions :: [String]}

z3Solver :: SmtSolver
z3Solver = SmtSolver {command = "z3", options = ["-smt2", "-in", "-nw"],
                      smtOptions = ["(set-option :smt.auto-config false)",
                                    "(set-option :smt.mbqi false)"]}

type SmtScript = String
data SmtResult = Sat | Unsat | Unknown deriving (Show, Eq)

smtResult "sat" = Sat
smtResult "unsat" = Unsat
smtResult "unknown" = Unknown

isNotSatisfiable :: SmtResult -> Bool
isNotSatisfiable Unsat = True
isNotSatisfiable _ = False

runSolver :: SmtSolver -> SmtScript -> IO SmtResult
runSolver solver script = do
    exec <- findExecutable (command solver)
    (case exec of
      Nothing       -> return $ error ("SMT Solver \""
                              ++ (command solver)
                              ++ "\" is not installed or is not added to PATH.")
      Just execPath -> do (exit, out, err) <- readProcessWithExitCode execPath (options solver)
                                                (concat (smtOptions solver) ++ script)
                          return (case exit of
                            ExitSuccess -> smtResult $ head $ lines out
                            ExitFailure code -> error $ unlines ["SMT Solver " ++ (command solver) ++ " exits with code: "
                               ++ show code, "input: " ++ script, "output: " ++ out, "error: " ++ err]))

----------------------------------------------------------------------------------------------------
-- SMT-LIB script
----------------------------------------------------------------------------------------------------

getSmtAssertOp :: SmtLogic -> String -> [Formula] -> String
getSmtAssertOp l op fs = "(" ++ op ++ " " ++ (concat $ fmap (getSmtAssert l) fs) ++ ")"

getSmtAssert :: SmtLogic -> Formula -> String
getSmtAssert l (Formula _ f) = getAssert l f
    where getAssert _ T = "true"
          getAssert _ F = "false"
          getAssert _ (Constraint NotEquals x1 x2) = "(not (= " ++ (variableNameAscii x1) ++ " " ++ (variableNameAscii x2) ++ "))"
          getAssert _ (Constraint r x1 x2) = "(" ++ relationAscii r ++ " " ++ (variableNameAscii x1) ++ " " ++ (variableNameAscii x2) ++ ")"
          getAssert l (And fs) = getSmtAssertOp l "and" $ elems fs
          getAssert l (Or fs) = getSmtAssertOp l "or" $ elems fs
          getAssert l (Not f) = getSmtAssertOp l "not" [f]

getSmtAssertForAllFree :: SmtLogic -> Formula -> String
getSmtAssertForAllFree l f@(Formula fvs _) =
    if null fvs
    then (getSmtAssert l f)
    else (concat $ fmap (\x -> "(declare-const " ++ (variableNameAscii x) ++ " " ++ sort l ++ ")") $ elems fvs)
         ++ "(assert "
         ++ (getSmtAssert l f)
         ++ ")"

getSmtScript :: SmtLogic -> Formula -> SmtScript
getSmtScript l f = "(set-logic " ++ logic l ++ ")" ++ (getSmtAssertForAllFree l f) ++ "(check-sat)"

----------------------------------------------------------------------------------------------------
-- Formula solving
----------------------------------------------------------------------------------------------------

isTrueIO :: SmtLogic -> Formula -> IO Bool
isTrueIO l (Formula _ T) = return True
isTrueIO l (Formula _ F) = return False
isTrueIO l f@(Formula fvs _) = do
                                 result <- runSolver z3Solver $ getSmtScript l (Formula fvs $ Not f)
                                 return $ isNotSatisfiable result

isFalseIO :: SmtLogic -> Formula -> IO Bool
isFalseIO _ (Formula _ T) = return False
isFalseIO _ (Formula _ F) = return True
isFalseIO l f@(Formula fvs _) = isTrueIO l (Formula fvs $ Not f)

----------------------------------------------------------------------------------------------------
-- Formula unsafe solving
----------------------------------------------------------------------------------------------------

isTrue :: SmtLogic -> Formula -> Bool
isTrue l = unsafePerformIO . isTrueIO l

isFalse :: SmtLogic -> Formula -> Bool
isFalse l = unsafePerformIO . isFalseIO l
