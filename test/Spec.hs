{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-partial-type-signatures -Wno-unused-top-binds -Wno-incomplete-uni-patterns -Wno-x-partial #-}

import NLambda
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (filter, map, sum)

{-
NOTE: It is not so easy to make test cases for NLambda, as a lot of things are
symbolic. So we often resort to either 'isTrue' or 'isFalse'. Because of that,
there are many simple test-cases for sets with 'eq' and 'neq'.

USAGE: run `cabal test` to run all test-cases. Options can be passed as:

  cabal test nlambda --test-options=--hide-successes
  cabal test nlambda --test-options="-p Set -q"

-}

------------------------------------------------------------------------------
-- Main setup
------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "All" [sets, graphs]

assertTrue :: Formula -> Assertion
assertTrue = assertBool "" . isTrue

assertFalse :: Formula -> Assertion
assertFalse = assertBool "" . isFalse

------------------------------------------------------------------------------
-- Test cases for 'NLambda.Set'
------------------------------------------------------------------------------
sets :: TestTree
sets = testGroup "Sets"
  [ testGroup "Size"
    [ testGroup "size of A^n"
      [ testCase ("n = " <> show n) $ (fromVariant . setOrbitsNumber . replicateAtoms $ n) @?= (combinatoric1 !! n) | n <- [1..3] ]
    , testGroup "size of filter"
      [ testCase ("n = " <> show n) $ (fromVariant . setOrbitsNumber . filter constr . replicateAtoms $ n) @?= (combinatoric2 !! n) | n <- [1..4] ]
    ]
  , testGroup "Equality"
    [ testGroup "proper subsets"
      [ testCase ("filtered A^n < A^n, n = " <> show n) $ assertTrue (filter constr (replicateAtoms n) `isProperSubsetOf` replicateAtoms n) | n <- [2, 3, 4]]
    , testGroup "sum . orbits = id"
      [ testCase name $ assertTrue ((sum . setOrbits $ x) `eq` x) | (name, x) <- someSets]
    , testGroup "set equality and inequality"
      [ testCase ("eq/neq: " <> n1 <> " vs " <> n2) $ assertTrue (s1 `op` s2) | (n1, s1) <- someSets, (n2, s2) <- someSets, let op = if n1 == n2 then eq else neq]
    ]
  , testGroup "Algebra"
    [ testGroup "absorption1"
      [ testCase ("(" <> n1 <> " ∪ " <> n2 <> ") ∩ " <> n1) $ assertTrue (((s1 `union` s2) `intersection` s1) `eq` s1) | (n1, s1) <- someSets, (n2, s2) <- someSets]
    , testGroup "absorption2"
      [ testCase ("(" <> n1 <> " ∩ " <> n2 <> ") ∪ " <> n1) $ assertTrue (((s1 `intersection` s2) `union` s1) `eq` s1) | (n1, s1) <- someSets, (n2, s2) <- someSets]
    , testGroup "distributivity-symm"
      [ testCase (n1 <> ", " <> n2 <> ", " <> n3) $ assertTrue (unions [s1 `intersection` s2, s2 `intersection` s3, s3 `intersection` s1] `eq` intersections1 [s1 `union` s2, s2 `union` s3, s3 `union` s1]) | (n1, s1) <- someSets, (n2, s2) <- someSets, (n3, s3) <- someSets]
    ]
  ]
  where
    -- constrain the list [a1, a2, a3, ...] so that a1 ≠ a2, a2 ≠ a3, etc
    constr ls = NLambda.and . fmap (uncurry neq) $ zip ls (tail ls)
    -- collection of test-sets, we include the same set multiple times, with
    -- a different construction, so that we test equality of different
    -- constructions as well. (All based on lists of atoms.)
    -- TODO: Maybe this can be replaced with QuickCheck style testing?
    someSets =
      [ ("A^1", replicateAtoms 1)
      , ("A^2", replicateAtoms 2)
      , ("A^2", map (\(a, b) -> [a,b]) differentAtomsPairs `union` map (\a -> [a, a]) atoms)
      , ("A^2", pairsWith (\x y -> [x, y]) atoms atoms)
      , ("distinct A^2", map (\(a, b) -> [a,b]) differentAtomsPairs)
      , ("distinct A^2", filter (\[a, b] -> a `neq` b) (replicateAtoms 2))
      , ("A^3", replicateAtoms 3)
      , ("filtered A^3", filter constr (replicateAtoms 3)) ]
    -- dual to unions, but with at least one term
    intersections1 = foldl1 intersection

------------------------------------------------------------------------------
-- Test cases for 'NLambda.Graph'
------------------------------------------------------------------------------
graphs :: TestTree
graphs = testGroup "Graphs"
  [ testCase "clique A = reverse (clique A)" $ assertTrue (atomsClique `eq` reverseEdges atomsClique)
  , testCase "clique A = (clique A)^2" $ assertTrue (atomsClique `eq` compose atomsClique atomsClique)
  , testCase "isStronglyConnected atomsClique" $ assertTrue (isStronglyConnected atomsClique)
  , testCase "hasEvenLengthCycle atomsClique" $ assertTrue (hasEvenLengthCycle atomsClique)
  , testCase "hasOddLengthCycle atomsClique" $ assertTrue (hasOddLengthCycle atomsClique)
  , testCase "no equiv. colouring" $ assertFalse (hasEquivariantColoring atomsClique 3)
  , testCase "atomsClique is not simple" $ assertFalse (isSimple atomsClique)
  ]

------------------------------------------------------------------------------
-- Combinatorics/Numbers used in test cases, depends on the structure of atoms
------------------------------------------------------------------------------

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
