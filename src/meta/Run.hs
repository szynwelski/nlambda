import Sample
import Meta
import Prelude (print, null)
import System.Environment (getArgs)

main = do args <- getArgs
          output args

output [] = print test
output _ = print nlambda_test
