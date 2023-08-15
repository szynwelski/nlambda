import NLambda
import Test.Tasty (localOption)
import Test.Tasty.Bench

{- NOTE: we measure using WallTime, because the time z3 is running
   is also relevant for us.

   Currently we only benchmark computing all orbits.
-}

main :: IO ()
main = defaultMain . pure . localOption WallTime $ bgroup "benchmarks"
  [ bgroup "setOrbitsNumber A^n"
    [ bench ("n = " <> show n) $ nf (show . setOrbitsNumber . replicateAtoms) n | n <- [1..4] ]
  , bgroup "setOrbitsNumber filter"
    [ bench ("n = " <> show n) $ nf (show . setOrbitsNumber . NLambda.filter constr . replicateAtoms) n | n <- [1..4] ]
  ]
  where
    constr ls = NLambda.and . fmap (uncurry neq) $ zip ls (tail ls)
