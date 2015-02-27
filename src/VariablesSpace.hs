module Nominal.VariablesSpace where

import Control.Monad (replicateM)
import Control.Monad.State (State, evalState, state)
import Nominal.Variants (Atom, atom)

type VariablesSpace = State [String]

allVariableNames :: [String]
allVariableNames = [1..] >>= (`replicateM` ['a'..'z'])

variableSpace :: (String -> a) -> VariablesSpace a
variableSpace f = state (\s -> (f $ head s, tail s))

runNLambda :: VariablesSpace a -> a
runNLambda nl = evalState nl allVariableNames

newAtom :: VariablesSpace Atom
newAtom  = variableSpace atom
