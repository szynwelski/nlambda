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
y = Variable 4
m1 = withMeta x [(1,2)]
m2 = withMeta x [(1,3)]
m3 = withMeta y [(4,5)]

output [] = print $ test x x y
output _ = print $ test_nlambda m1 m2 m3
