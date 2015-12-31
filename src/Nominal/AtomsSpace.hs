module Nominal.AtomsSpace where

import Control.Monad (replicateM)
import Control.Monad.State (State, evalState, state)
import Nominal.Atom (Atom, atom)

----------------------------------------------------------------------------------------------------
-- Atoms space
----------------------------------------------------------------------------------------------------

-- | Space with free atoms names.
type AtomsSpace = State [String]

allAtomsNames :: [String]
allAtomsNames = [1..] >>= (`replicateM` ['a'..'z'])

atomsSpace :: (String -> a) -> AtomsSpace a
atomsSpace f = state (\s -> (f $ head s, tail s))

-- | Creates a new atom with a previously unused name.
newAtom :: AtomsSpace Atom
newAtom  = atomsSpace atom

-- | Evaluates given value having 'newAtom' commands.
runNLambda :: AtomsSpace a -> a
runNLambda nl = evalState nl allAtomsNames
