import Test
import Meta
import Var
import Data.Map (fromList)
import System.Environment (getArgs)

main = do args <- getArgs
          output args

withMeta :: a -> [(Identifier, Identifier)] -> WithMeta a
withMeta x = create x . metaFromMap . fromList

x = Variable 1
mx = withMeta x [(1,2)]
my = withMeta x [(1,3)]

output [] = print $ test x x
output _ = print $ test_nlambda mx my
