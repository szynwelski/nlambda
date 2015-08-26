module Nominal.Formula.Solver (isTrueIO, isFalseIO, solveIO, isTrue, isFalse, solve) where

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

getSmtLogicForRelation :: Relation -> SmtLogic
getSmtLogicForRelation LessThan = lra
getSmtLogicForRelation LessEquals = lra
getSmtLogicForRelation Equals = lia
getSmtLogicForRelation NotEquals = lia
getSmtLogicForRelation GreaterThan = lra
getSmtLogicForRelation GreaterEquals = lra

getFormulaRelations :: Formula -> Set Relation
getFormulaRelations (Formula _ f) = getRelations f
    where getRelations T = empty
          getRelations F = empty
          getRelations (Constraint r _ _) = singleton r
          getRelations (And fs) = unions $ fmap getFormulaRelations $ elems fs
          getRelations (Or fs) = unions $ fmap getFormulaRelations $ elems fs
          getRelations (Not f) = getFormulaRelations f
          getRelations (ForAll _ f) = getFormulaRelations f
          getRelations (Exists _ f) = getFormulaRelations f

getSmtLogic :: Formula -> SmtLogic
getSmtLogic f = if member lra $ map getSmtLogicForRelation $ getFormulaRelations f then lra else lia

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
          getAssert l (ForAll x f) = "(forall ((" ++ (variableNameAscii x) ++ " " ++ sort l ++ ")) " ++ (getSmtAssert l f) ++ ")"
          getAssert l (Exists x f) = "(exists ((" ++ (variableNameAscii x) ++ " " ++ sort l ++ ")) " ++ (getSmtAssert l f) ++ ")"

getSmtAssertForAllFree :: SmtLogic -> Formula -> String
getSmtAssertForAllFree l f@(Formula fvs _) =
    if null fvs
    then (getSmtAssert l f)
    else "(forall ("
         ++ (concat $ fmap (\x -> "(" ++ (variableNameAscii x) ++ " " ++ sort l ++ ")") $ elems fvs)
         ++")"
         ++ (getSmtAssert l f)
         ++ ")"

getSmtScript :: Formula -> SmtScript
getSmtScript f = let l = getSmtLogic f
                 in "(set-logic " ++ logic l ++ ")(assert " ++ (getSmtAssertForAllFree l f) ++ ")(check-sat)"

----------------------------------------------------------------------------------------------------
-- Formula solving
----------------------------------------------------------------------------------------------------

isTrueIO :: Formula -> IO Bool
isTrueIO (Formula _ T) = return True
isTrueIO (Formula _ F) = return False
isTrueIO f = do
               result <- runSolver z3Solver $ getSmtScript f
               return $ isSatisfiable result

isFalseIO :: Formula -> IO Bool
isFalseIO f@(Formula fvs _) = isTrueIO (Formula fvs $ Not f)

solveIO :: Formula -> IO (Maybe Bool)
solveIO f = do
        true <- isTrueIO f
        if true
            then return (Just True)
            else do
                 false <- isFalseIO f
                 if false
                    then return (Just False)
                    else return Nothing

----------------------------------------------------------------------------------------------------
-- Formula unsafe solving
----------------------------------------------------------------------------------------------------

isTrue :: Formula -> Bool
isTrue = unsafePerformIO . isTrueIO

isFalse :: Formula -> Bool
isFalse = unsafePerformIO . isFalseIO

solve :: Formula -> Maybe Bool
solve f
    | isTrue f  = Just True
    | isFalse f = Just False
    | otherwise = Nothing
