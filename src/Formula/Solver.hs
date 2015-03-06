module Formula.Solver (isTrue, isFalse, solve, unsafeIsTrue, unsafeIsFalse, unsafeSolve) where

import Data.Set (toList)
import Formula
import Nominal.Type
import Nominal.Variable (variableNameAscii)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

----------------------------------------------------------------------------------------------------
-- SMT Solver
----------------------------------------------------------------------------------------------------

data SmtSolver = SmtSolver {command :: String, options :: [String], smtOptions :: [String]}

z3Solver :: SmtSolver
z3Solver = SmtSolver {command = "z3", options = ["-smt2", "-in"],
                      smtOptions = ["(set-option :smt.auto-config false)",
                                    "(set-option :smt.mbqi false)"]}

type SmtScript = String
data SmtResult = Sat | Unsat | Unknown deriving (Show, Eq)

smtResult "sat" = Sat
smtResult "unsat" = Unsat
smtResult "unknown" = Unknown

isSatisfiable :: SmtResult -> Bool
isSatisfiable Sat = True
isSatisfiable _ = False

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
                            ExitFailure code -> error $ unlines ["SMT Solver " ++ (command solver)
                                ++ " exits with code: " ++ show code, out, err]))

----------------------------------------------------------------------------------------------------
-- SMT-LIB script
----------------------------------------------------------------------------------------------------

getSmtAssertOp :: String -> [Formula] -> String
getSmtAssertOp op fs = "(" ++ op ++ " " ++ (concat $ fmap getSmtAssert fs) ++ ")"

getSmtAssert :: Formula -> String
getSmtAssert T = "true"
getSmtAssert F = "false"
getSmtAssert (Constraint r x1 x2) = "(" ++ relationAscii r ++ " " ++ (variableNameAscii x1) ++ " " ++ (variableNameAscii x2) ++ ")"
getSmtAssert (And f1 f2) = getSmtAssertOp "and" [f1, f2]
getSmtAssert (Or f1 f2) = getSmtAssertOp "or" [f1, f2]
getSmtAssert (Not f) = getSmtAssertOp "not" [f]
getSmtAssert (Imply f1 f2) = getSmtAssertOp "=>" [f1, f2]
getSmtAssert (Equivalent f1 f2) = getSmtAssertOp "=" [f1, f2]
getSmtAssert (ForAll x f) = "(forall ((" ++ (variableNameAscii x) ++ " Int)) " ++ (getSmtAssert f) ++ ")"
getSmtAssert (Exists x f) = "(exists ((" ++ (variableNameAscii x) ++ " Int)) " ++ (getSmtAssert f) ++ ")"

getSmtAssertForAllFree :: Formula -> String
getSmtAssertForAllFree f =
    if null fvs
        then (getSmtAssert f)
        else "(forall ("
             ++ (concat $ fmap (\x -> "(" ++ (variableNameAscii x) ++ " Int)") fvs)
             ++ ")"
             ++ (getSmtAssert f)
             ++ ")"
    where fvs = toList $ freeVariables f

getSmtScript :: Formula -> SmtScript
getSmtScript f = "(set-logic LIA)(assert " ++ (getSmtAssertForAllFree f) ++ ")(check-sat)"

----------------------------------------------------------------------------------------------------
-- Formula solving
----------------------------------------------------------------------------------------------------

isTrue :: Formula -> IO Bool
isTrue T = return True
isTrue F = return False
isTrue f = do
        result <- runSolver z3Solver (getSmtScript f)
        return $ isSatisfiable result

isFalse :: Formula -> IO Bool
isFalse f = isTrue (Formula.not f)

solve :: Formula -> IO (Maybe Bool)
solve f = do
        true <- isTrue f
        if true
            then return (Just True)
            else do
                 false <- isFalse f
                 if false
                    then return (Just False)
                    else return Nothing

----------------------------------------------------------------------------------------------------
-- Formula unsafe solving
----------------------------------------------------------------------------------------------------

unsafeIsTrue :: Formula -> Bool
unsafeIsTrue = unsafePerformIO . isTrue

unsafeIsFalse :: Formula -> Bool
unsafeIsFalse = unsafePerformIO . isFalse

unsafeSolve :: Formula -> Maybe Bool
unsafeSolve f
         | unsafeIsTrue f  = Just True
         | unsafeIsFalse f = Just False
         | otherwise = Nothing
