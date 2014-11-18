module Nominal where

import Prelude hiding (or, and, not, sum, map, filter)
import Data.List.Utils (join)
import Data.IORef


----------------------------------------------------------------------------------------------------

variables = [ c : s | s <- "": variables, c <- ['a'..'z']]

----------------------------------------------------------------------------------------------------

class NominalEq a where
    eq :: a -> a -> Formula

----------------------------------------------------------------------------------------------------

newtype Atom = Atom {atomName :: String}

atom :: String -> Nominal Atom
atom a = nominal (Atom a)

instance Show Atom where
    show = atomName

instance Eq Atom where
    a1 == a2 = (atomName a1) == (atomName a2)

instance NominalEq Atom where
    eq a1 a2 = if a1 == a2 then T else Equals a1 a2

----------------------------------------------------------------------------------------------------

data Formula = T | F | And Formula Formula | Or Formula Formula | Not Formula
    |Imply Formula Formula | Equivalent Formula Formula | Equals Atom Atom

showFormula :: Formula -> String
showFormula f@(And f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Or f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Imply f1 f2) = "(" ++ show f ++ ")"
showFormula f@(Equivalent f1 f2) = "(" ++ show f ++ ")"
showFormula f = show f

instance Show Formula where
    show T = "True"
    show F = "False"
    show (And f1 f2) = showFormula f1 ++ " /\\ " ++ showFormula f2
    show (Or f1 f2) = showFormula f1 ++ " \\/ " ++ showFormula f2
    show (Not (Equals x1 x2)) = show x1 ++ " != " ++ show x2
    show (Not f) = "!(" ++ show f ++ ")"
    show (Equals x1 x2) = show x1 ++ " = " ++ show x2
    show (Imply x1 x2) = showFormula x1 ++ " ==> " ++ showFormula x2
    show (Equivalent x1 x2) = showFormula x1 ++ " <==> " ++ showFormula x2

instance Eq Formula where
    T == T = True
    F == F = True
    (And f11 f12) == (And f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (Or f11 f12) == (Or f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (Not f1) == (Not f2) = f1 == f2
    (Imply f11 f12) == (Imply f21 f22) = (f11 == f21) && (f12 == f22)
    (Equivalent f11 f12) == (Equivalent f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    (Equals f11 f12) == (Equals f21 f22) = ((f11 == f21) && (f12 == f22)) || ((f12 == f21) && (f11 == f22))
    _ == _ = False

instance NominalEq Formula where
    eq = iff


(/\) :: Formula -> Formula -> Formula
T /\ f = f
F /\ _ = F
f /\ T = f
_ /\ F = F
(Not f1) /\ (Not f2) = (not (f1 \/ f2))
f1 /\ f2
    | f1 == f2       = f1
    | (not f1) == f2 = F
    | otherwise      = And f1 f2

and :: [Formula] -> Formula
and [] = T
and fs = foldr1 (/\) fs

(\/) :: Formula -> Formula -> Formula
F \/ f = f
T \/ _ = T
f \/ F = f
_ \/ T = T
(Not f1) \/ (Not f2) = (not (f1 /\ f2))
f1 \/ f2
    | f1 == f2       = f1
    | (not f1) == f2 = T
    | otherwise      = Or f1 f2

or :: [Formula] -> Formula
or [] = F
or fs = foldr1 (\/) fs

not :: Formula -> Formula
not F = T
not T = F
not (Not f) = f
not f = Not f

(==>) :: Formula -> Formula -> Formula
T ==> f = f
F ==> _ = T
_ ==> T = T
f ==> F = f
(Not f1) ==> (Not f2) = f2 ==> f1
f1 ==> f2
    | f1 == f2       = T
    | (not f1) == f2 = Not f1
    | otherwise      = Imply f1 f2

implies :: Formula -> Formula -> Formula
implies = (==>)

(<==>) :: Formula -> Formula -> Formula
T <==> T = T
F <==> F = T
F <==> T = F
T <==> F = F
(Not f1) <==> (Not f2) = f1 <==> f2
f1 <==> f2
    | f1 == f2       = T
    | (not f1) == f2 = F
    | otherwise      = Equivalent f1 f2

iff :: Formula -> Formula -> Formula
iff = (<==>)

isTrue :: Formula -> Bool
isTrue T = True
isTrue _ = False

isFalse :: Formula -> Bool
isFalse F = True
isFalse _ = False

----------------------------------------------------------------------------------------------------

data Branch a = Branch {value :: a, condition :: Formula}

instance Show a => Show (Branch a) where
--    show (Branch v c) = show v ++ " : " ++ show c --(if (isTrue c) then "" else " : " ++ show c)
    show (Branch v c) = show v ++ (if (isTrue c) then "" else " : " ++ show c)

instance Functor Branch where
    fmap f (Branch v c) = Branch (f v) c

instance Monad Branch where
    return = branch
    b >>= f = branchIf (condition b) (f $ value b)

instance NominalEq a => NominalEq (Branch a) where
    eq (Branch v1 c1) (Branch v2 c2) = (eq v1 v2) /\ c1 /\ c2

branch :: a -> Branch a
branch v = Branch v T

branchIf :: Formula -> Branch a -> Branch a
branchIf c b = Branch (value b) (condition b /\ c)

----------------------------------------------------------------------------------------------------

data Nominal a = Nominal {branches :: [Branch a]}

instance Show a => Show (Nominal a) where
    show e = join " | " (fmap show (branches e))

instance Functor Nominal where
    fmap f (Nominal bs) = Nominal $ fmap (fmap f) bs

instance Monad Nominal where
    return = nominal
    n >>= f = Nominal $ concat $ fmap branches $ fmap (\b -> nominalIf (condition b) (value b)) (branches $ fmap f n)

instance NominalEq a => NominalEq (Nominal a) where
    eq (Nominal bs1) (Nominal bs2) = or [(eq b1 b2) | b1 <- bs1, b2 <- bs2]

nominal :: a -> Nominal a
nominal v = Nominal [branch v]

nominalIf :: Formula -> Nominal a -> Nominal a
nominalIf c n = Nominal $ fmap (branchIf c) (branches n)

----------------------------------------------------------------------------------------------------

iF :: Formula -> Nominal a -> Nominal a -> Nominal a
iF c (Nominal bs1) (Nominal bs2)
    | (isTrue c) = Nominal bs1
    | (isFalse c) = Nominal bs2
    | otherwise = Nominal $ (fmap (branchIf c) bs1) ++ (fmap (branchIf $ not c) bs2)

----------------------------------------------------------------------------------------------------

data Set a = Set {elements :: [Branch a]}

instance Show a => Show (Set a) where
    show s = "{" ++ (join ", " (fmap show (elements s))) ++ "}"

instance Functor Set where
    fmap f (Set elems) = Set $ fmap (fmap f) elems

instance Monad Set where
    return e = Set [branch e]
    n >>= f = Set $ concat $ fmap elements $ fmap (\b -> setIf (condition b) (value b)) (elements $ fmap f n)

setIf :: Formula -> Set a -> Set a
setIf c n = Set $ fmap (branchIf c) (elements n)

reduceSet :: Nominal (Set a) -> [Set a]
reduceSet ns = fmap (\b -> setIf (condition b) (value b)) (branches ns)

emptySet :: Nominal (Set a)
emptySet = nominal (Set [])

empty :: Nominal (Set a) -> Formula
empty ns = or (fmap (\b -> (and (fmap (not . condition) (elements (value b)))) /\ (condition b)) (branches ns))

add :: Nominal a -> Nominal (Set a) -> Nominal (Set a)
add e = fmap (Set . (branches e ++) . elements)

just :: Nominal a -> Nominal (Set a)
just e = add e emptySet

map :: (Nominal a -> Nominal b) -> Nominal (Set a) -> Nominal (Set b)
map f ns = fmap (\s -> Set $ concat (fmap (\e -> (branches (f (Nominal [e])))) (elements s))) ns

sum :: Nominal (Set (Set a)) -> Nominal (Set a)
sum = fmap (\s -> Set $ concat (fmap (\b -> (fmap (branchIf (condition b)) (elements $ value b))) (elements s)))

----------------------------------------------------------------------------------------------------

filter :: (Nominal a -> Formula) -> Nominal (Set a) -> Nominal (Set a)
filter f s = sum (map (\x -> (iF (f x) (just x) emptySet)) s)

exists :: (Nominal a -> Formula) -> Nominal (Set a) -> Formula
exists f s = not (empty (filter f s))

forall :: (Nominal a -> Formula) -> Nominal (Set a) -> Formula
forall f s = empty (filter (\x -> not (f x)) s)

union :: Nominal (Set a) -> Nominal (Set a) -> Nominal (Set a)
union s1 s2 = sum (add s1 (add s2 emptySet))

----------------------------------------------------------------------------------------------------

contains :: NominalEq a => Nominal (Set a) -> Nominal a -> Formula
contains s e = exists (eq e) s

subset :: NominalEq a => Nominal (Set a) -> Nominal (Set a) -> Formula
subset s1 s2 = forall (contains s2) s1

instance NominalEq a => NominalEq (Set a) where
    eq s1 s2 = (subset ns1 ns2) /\ (subset ns2 ns1)
               where ns1 = (nominal s1)
                     ns2 = (nominal s2)

--instance NominalEq a => NominalEq (Nominal (Set a)) where
--    eq ns1 ns2 = (subset ns1 ns2) /\ (subset ns2 ns1)

----------------------------------------------------------------------------------------------------

x = atom "x"
y = atom "y"
z = atom "z"
cond = eq x y

el = iF cond x y
set = just el
el1 = iF cond y x
set1 = just el1

justset = value $ head $ branches set
condSet = iF cond (just x) emptySet
