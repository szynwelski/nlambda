import Sample
import Meta
import Prelude (print)
import System.Environment (getArgs)

main = do args <- getArgs
          output args

output [] = print test
output _ = print test_nlambda
