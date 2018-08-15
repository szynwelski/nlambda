import Test
import Meta
import Var
import qualified Data.Map as Map
import System.Environment (getArgs)

main = do args <- getArgs
          output args

withMeta :: a -> [(Identifier, Identifier)] -> WithMeta a
withMeta x = create x . metaFromMap . Map.fromList

x = Variable 1
z = Variable 4
mx = withMeta x [(1,2)]
my = withMeta x [(1,3)]
mz = withMeta x [(4,5)]

output [] = print $ test x x
output _ = print $ test_nlambda mx my
