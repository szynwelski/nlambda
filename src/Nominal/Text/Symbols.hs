{-# LANGUAGE CPP #-}

module Nominal.Text.Symbols where

-- I have put the symbols into a single file
-- This way, we only need CPP here, and not
-- sprinkled thoughout the library

inSet, atoms, lt, leq, gt, geq, eq, neq, not, or, and :: String
subscriptIndex :: Int -> String

#ifdef DISABLE_UNICODE

inSet = "in"
atoms = "A"

lt  = "<"
leq = "<="
eq  = "="
neq = "/="
gt  = ">"
geq = ">="

not = "~"
or = " \\/ "
and = " /\\ "

subscriptIndex n = "_" ++ show n

#else

inSet = "∊"
atoms = "𝔸"

lt  = "<"
leq = "≤"
eq  = "="
neq = "≠"
gt  = ">"
geq = "≥"

not = "¬"
or = " ∨ "
and = " ∧ "

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

subscriptIndex n = fmap (toEnum . (+ 8320)) (digits n)

#endif