import Data.Logic.Propositional hiding (interpret)
import Data.SBV
import Text.Parsec.Error (ParseError)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Foldable (foldMap)
import Control.Monad ((<=<))

testParse :: Either ParseError Expr
testParse = parseExpr "test source" "((X | ~Z) & (Y | ~Z))"

type Env = M.Map String SBool

envLookup :: Var -> Env -> SBool
envLookup (Var v) e = maybe (error $ "Var not found: " ++ show v) id
                            (M.lookup [v] e)

solveExpr :: Expr -> IO AllSatResult
solveExpr e0 = allSat go
 where
  vs :: [String]
  vs = map (\(Var c) -> [c]) (variables e0)

  go :: Predicate
  go = do
      syms <- mapM exists vs
      let env = M.fromList (zip vs syms)
      interpret env e0
  interpret :: Env -> Expr -> Predicate
  interpret env expr = do
   let interp = interpret env
   case expr of
    Variable v -> return (envLookup v env)
    Negation e -> bnot `fmap` interp e
    Conjunction e1 e2   ->
     do r1 <- interp e1
        r2 <- interp e2
        return (r1 &&& r2)
    Disjunction e1 e2   ->
     do r1 <- interp e1
        r2 <- interp e2
        return (r1 ||| r2)
    Conditional e1 e2   -> error "And so on"
    Biconditional e1 e2 -> error "And so on"

main :: IO ()
main = do
       let expr = testParse
       putStrLn $ "Solving expr: " ++ show expr
       either (error . show) (print <=< solveExpr) expr
