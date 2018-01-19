import Sample
import Meta
import Prelude (print, null)
import System.Environment (getArgs)

main = do args <- getArgs
          if null args
          then print test
          else print nlambda_test
