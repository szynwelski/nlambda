module Nominal.Automaton.Deterministic where

import Nominal.Formula
import Nominal.Set
import Nominal.Type

---------------------------------------------------------------------------------------------------
-- Deterministic automaton
----------------------------------------------------------------------------------------------------

data DAutomaton a b = DAutomaton {delta :: a -> b -> a, initialState :: a, finalStates :: Set a}

instance Show a => Show (DAutomaton a b) where
    show a = "DAutomaton {initialState = " ++ show (initialState a) ++ ", "
                      ++ "finalStates = " ++ show (finalStates a) ++ "}"

da :: (a -> b -> a) -> a -> Set a -> DAutomaton a b
da = DAutomaton

dAccepts :: NominalType a => DAutomaton a b -> [b] -> Formula
dAccepts a = contains (finalStates a) . foldl (delta a) (initialState a)

dMinimize :: DAutomaton a b -> DAutomaton (Set a) b
dMinimize a = undefined
