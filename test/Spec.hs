{-# language CPP #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-partial-type-signatures -Wno-unused-top-binds #-}

import NLambda
import Prelude hiding (filter, sum)
import Test.Tasty
import Test.Tasty.HUnit

{- Currently we only have some high level tests, checking simple boolean
   properties of sets and graphs. Note: most of these properties could
   be satisfied, even if the implementation is wrong (for example if all
   sets happen to be empty, or if `isTrue` always returns True).
-}

main :: IO ()
main = defaultMain $ testGroup "All" [sets, graphs]

assertFormula :: Formula -> Assertion
assertFormula = assertBool "" . isTrue

sets :: TestTree
sets = testGroup "Set tests"
  [ testGroup "size of A^n"
    [ testCase ("n = " <> show n) $ (fromVariant . setOrbitsNumber . replicateAtoms $ n) @?= (combinatoric1 !! n) | n <- [1..3] ]
  , testGroup "size of filter"
    [ testCase ("n = " <> show n) $ (fromVariant . setOrbitsNumber . filter constr . replicateAtoms $ n) @?= (combinatoric2 !! n) | n <- [1..4] ]
  , testGroup "sum . orbits = id" -- ideally this would be tested with e.g. QuickCheck
    [ testCase name $ assertFormula ((sum . setOrbits $ x) `eq` x) | (name, x) <- someSets]
  , testCase "A^1 != A^2" $ assertFormula (replicateAtoms 1 `neq` replicateAtoms 2)
  , testCase "filtered A^3 < A^3" $ assertFormula (filter constr (replicateAtoms 3) `isProperSubsetOf` replicateAtoms 3)
  , testCase "AxA = A^2" $ assertFormula (pairsWith (\x y -> [x, y]) atoms atoms `eq` replicateAtoms 2)
  , testCase "pairsWithFilter = differentAtomPairs" $ assertFormula (pairsWithFilter (\x y -> maybeIf (x `neq` y) (x, y)) atoms atoms `eq` differentAtomsPairs)
  ]
  where
    constr ls = NLambda.and . fmap (uncurry neq) $ zip ls (tail ls)
    someSets =
      [ ("A", replicateAtoms 1)
      , ("A^3", replicateAtoms 3)
      , ("filtered set", filter constr (replicateAtoms 3)) ]

graphs :: TestTree
graphs = testGroup "Graph tests"
  [ testCase "clique A = reverse (clique A)" $ assertFormula (atomsClique `eq` reverseEdges atomsClique)
  , testCase "clique A = (clique A)^2" $ assertFormula (atomsClique `eq` compose atomsClique atomsClique)
  , testCase "isStronglyConnected atomsClique" $ assertFormula (isStronglyConnected atomsClique)
  , testCase "hasEvenLengthCycle atomsClique" $ assertFormula (hasEvenLengthCycle atomsClique)
  , testCase "hasOddLengthCycle atomsClique" $ assertFormula (hasOddLengthCycle atomsClique)
  , testCase "no equiv. colouring" $ assertBool "" $ isFalse (hasEquivariantColoring atomsClique 3)
  , testCase "atomsClique is not simple" $ assertBool "" $ isFalse (isSimple atomsClique)
  ]

-- A000110: The Bell numbers
a000110 :: [Int]
a000110 = [1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597, 27644437]

-- A000670: Ordered Bell numbers or Fubini numbers
a000670 :: [Int]
a000670 = [1, 1, 3, 13, 75, 541, 4683, 47293, 545835, 7087261, 102247563, 1622632573, 28091567595]

-- A005649: The number of ordered set partitions of {1,...,n + 1} with no
-- two successive vertices in the same block.
a005649 :: [Int]
a005649 = [1, 2, 8, 44, 308, 2612, 25988, 296564, 3816548, 54667412, 862440068]

#if TOTAL_ORDER
combinatoric1 = a000670
combinatoric2 = a005649
#else
-- The Bell numbers are also the number of nonisomorphic colorings of
-- a map consisting of a row of n+1. We use that for combinatoric2.
-- (See https://oeis.org/A000110)
combinatoric1 = a000110
combinatoric2 = 1:a000110
#endif
