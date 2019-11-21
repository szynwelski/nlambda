{-# OPTIONS_GHC -fplugin Nominal.Meta.Plugin #-}
module Nominal.Formula.Definition where

import Data.List.Utils (join)
import qualified Data.MultiMap as MM
import Data.Set (Set, delete, elems, empty, findMin, foldl, foldr, fromList, insert, map, member, null, partition, singleton, size, union, unions)
import Nominal.Atoms.Signature (Relation(..), relationFunction, symmetricRelation)
import Nominal.Meta (WithMeta, idOp, noMeta, renameAndApply3)
import qualified Nominal.Text.Symbols as Symbols
import Nominal.Util.Read (readSepBy, skipSpaces, spaces, string)
import Nominal.Variable (Var(..), Variable, constantValue, isConstant, renameWithFlatTree)
import Prelude hiding (and, foldl, foldr, map, not, null, or)
import Text.Read (Lexeme(Ident), Prec, ReadPrec, (+++), lexP, parens, pfail, prec, readPrec, step)

----------------------------------------------------------------------------------------------------
-- Formula structure
----------------------------------------------------------------------------------------------------

data FormulaStructure
    = F
    | T
    | Constraint Relation Variable Variable
    | And (Set Formula)
    | Or (Set Formula)
    | Not Formula deriving (Eq, Ord)

----------------------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------------------

showSubformula :: FormulaStructure -> String
showSubformula f@(And fs) = "(" ++ show f ++ ")"
showSubformula f@(Or fs) = "(" ++ show f ++ ")"
showSubformula f = show f

instance Show FormulaStructure where
    show F = "false"
    show T = "true"
    show (Constraint r x1 x2) = show x1 ++ spaces (show r) ++ show x2
    show (And fs) = join (spaces Symbols.and) ((showSubformula . formula) <$> elems fs)
    show (Or fs) = join (spaces Symbols.or) ((showSubformula . formula) <$> elems fs)
    show (Not f) = Symbols.not ++ "(" ++ show f ++ ")"

----------------------------------------------------------------------------------------------------
-- Read
----------------------------------------------------------------------------------------------------

readAndOr :: Prec -> (Set Formula -> FormulaStructure) -> String -> ReadPrec FormulaStructure
readAndOr pr constr symbol = do fs <- prec pr (readSepBy False symbol readFormula)
                                return $ constr $ fromList $ fmap (Formula False) fs

readAnd :: ReadPrec FormulaStructure
readAnd = readAndOr 3 And Symbols.and

readOr :: ReadPrec FormulaStructure
readOr = readAndOr 4 Or Symbols.or

readLit :: ReadPrec FormulaStructure
readLit = do Ident s <- lexP
             case s of
               "false" -> return F
               "true"  -> return T
               _       -> pfail

readNot :: ReadPrec FormulaStructure
readNot = prec 5 (do skipSpaces
                     string Symbols.not
                     f <- step readFormula
                     return $ Not $ Formula False f)

readConstraint :: ReadPrec FormulaStructure
readConstraint = do x <- readPrec
                    skipSpaces
                    r <- readPrec
                    skipSpaces
                    y <- readPrec
                    return $ Constraint r x y

readFormula :: ReadPrec FormulaStructure
readFormula = do f <- parens $ readLit +++ readConstraint +++ readNot +++ readAnd +++ readOr
                 skipSpaces
                 return f

instance Read FormulaStructure where
    readPrec = readFormula
----------------------------------------------------------------------------------------------------
-- Formula
----------------------------------------------------------------------------------------------------

-- | First order formula with free variables and relations between variables.
data Formula = Formula {isSimplified :: Bool, formula :: FormulaStructure}

instance Eq Formula where
    (Formula _ f1) == (Formula _ f2) = f1 == f2

instance Ord Formula where
    compare (Formula _ f1) (Formula _ f2) = compare f1 f2

instance Show Formula where
    show (Formula _ f) = show f

instance Read Formula where
    readPrec = do f <- step readPrec
                  return (Formula False f)

----------------------------------------------------------------------------------------------------
-- Formula constructors
----------------------------------------------------------------------------------------------------

-- | Creates the tautology formula.
true :: Formula
true = Formula True T

-- | Creates the contradiction formula.
false :: Formula
false = Formula True F

-- | Creates a formula based on a given 'Bool' value.
fromBool :: Bool -> Formula
fromBool True = true
fromBool False = false

----------------------------------------------------------------------------------------------------
-- Constraints
----------------------------------------------------------------------------------------------------

constraint :: Relation -> Variable -> Variable -> Formula
constraint r x1 x2
    | isConstant x1 && isConstant x2 = fromBool $ relationFunction r (constantValue x1) (constantValue x2)
    | x1 == x2 = if r == LessThan || r == GreaterThan || r == NotEquals then false else true
    | x1 > x2 = Formula True $ Constraint (symmetricRelation r) x2 x1
    | otherwise = Formula True $ Constraint r x1 x2

equals :: Variable -> Variable -> Formula
equals = constraint Equals

notEquals :: Variable -> Variable -> Formula
notEquals = constraint NotEquals

lessThan :: Variable -> Variable -> Formula
lessThan = constraint LessThan

lessEquals :: Variable -> Variable -> Formula
lessEquals = constraint LessEquals

greaterThan :: Variable -> Variable -> Formula
greaterThan = constraint GreaterThan

greaterEquals :: Variable -> Variable -> Formula
greaterEquals = constraint GreaterEquals

----------------------------------------------------------------------------------------------------
-- Simplification of constraints with the same variables
----------------------------------------------------------------------------------------------------

isConstraint :: Formula -> Bool
isConstraint (Formula _ Constraint{}) = True
isConstraint _ = False

sameVarsInConstraints :: FormulaStructure -> FormulaStructure -> Bool
sameVarsInConstraints (Constraint _ x1 x2) (Constraint _ y1 y2) = (x1, x2) == (y1, y2)
sameVarsInConstraints _ _ = False

checkSize :: (Set Formula -> FormulaStructure) -> Formula -> Set Formula -> Formula
checkSize creator defVal fs
    | null fs = defVal
    | size fs == 1 = findMin fs
    | size fs > 1 = Formula False (creator fs)

checkConstraints :: (Set Formula -> FormulaStructure) -> Formula
    -> (Maybe Relation -> Maybe Relation -> Maybe Relation) -> Set Formula -> Formula
checkConstraints creator defVal relFun fs =
    if member nDefVal cs
        then nDefVal
        else checkSize creator defVal (cs `union` fs2)
    where nDefVal = not defVal
          (fs1, fs2) = partition isConstraint fs
          relLists = MM.assocs $ foldr (\(Constraint r x1 x2) -> MM.insert (x1,x2) (Just r)) MM.empty (map formula fs1)
          rels = fmap (fmap $ foldr1 relFun) relLists
          cs = fromList $ fmap (\((x1,x2),rel) -> maybe nDefVal (\r -> constraint r x1 x2) rel) rels

----------------------------------------------------------------------------------------------------
-- And
----------------------------------------------------------------------------------------------------

-- merging constraints
andRelations :: Maybe Relation -> Maybe Relation -> Maybe Relation
andRelations r1 r2
    | r1 == r2 = r1
    | r1 > r2 = andRelations r2 r1
andRelations (Just LessThan)      (Just LessEquals)    = Just LessThan
andRelations (Just LessThan)      (Just NotEquals)     = Just LessThan
andRelations (Just LessEquals)    (Just NotEquals)     = Just LessThan
andRelations (Just LessEquals)    (Just Equals)        = Just Equals
andRelations (Just Equals)        (Just GreaterEquals) = Just Equals
andRelations (Just LessEquals)    (Just GreaterEquals) = Just Equals
andRelations (Just NotEquals)     (Just GreaterEquals) = Just GreaterThan
andRelations (Just NotEquals)     (Just GreaterThan)   = Just GreaterThan
andRelations (Just GreaterEquals) (Just GreaterThan)   = Just GreaterThan
andRelations _                    _                    = Nothing

-- create and formula
createAndSet :: Set Formula -> Set Formula
createAndSet = delete true . unions . elems . map fromAnd
    where fromAnd (Formula _ (And fs)) = fs
          fromAnd f = singleton f

andFromSet :: Set Formula -> Formula
andFromSet fs = if member false fs
                  then false
                  else checkConstraints And true andRelations (createAndSet fs)

-- | Creates a logical conjunction of two given formulas, e.g.
--
-- > f /\ false == false
-- > f /\ true  == f
-- > f /\ g     == g /\ f
infixr 3 /\
(/\) :: Formula -> Formula -> Formula
Formula _ F /\ _ = false
_ /\ Formula _ F = false
Formula _ T /\ f = f
f /\ Formula _ T = f
f1 /\ f2 = and [f1,f2]

-- | Creates a logical conjunction of a given list of formulas.
and :: [Formula] -> Formula
and = andFromSet . fromList

simplifiedAnd :: [Formula] -> Formula
simplifiedAnd = Formula True . And . fromList

----------------------------------------------------------------------------------------------------
-- Or
----------------------------------------------------------------------------------------------------

-- merging constraints
orRelations :: Maybe Relation -> Maybe Relation -> Maybe Relation
orRelations r1 r2
    | r1 == r2 = r1
    | r1 > r2 = orRelations r2 r1
orRelations (Just LessThan)      (Just LessEquals)    = Just LessEquals
orRelations (Just LessThan)      (Just Equals)        = Just LessEquals
orRelations (Just LessEquals)    (Just Equals)        = Just LessEquals
orRelations (Just LessThan)      (Just NotEquals)     = Just NotEquals
orRelations (Just LessThan)      (Just GreaterThan)   = Just NotEquals
orRelations (Just NotEquals)     (Just GreaterThan)   = Just NotEquals
orRelations (Just Equals)        (Just GreaterEquals) = Just GreaterEquals
orRelations (Just Equals)        (Just GreaterThan)   = Just GreaterEquals
orRelations (Just GreaterEquals) (Just GreaterThan)   = Just GreaterEquals
orRelations _                    _                    = Nothing

-- create or formula
createOrSet :: Set Formula -> Set Formula
createOrSet = delete false . unions . elems . map fromOr
    where fromOr (Formula _ (Or fs)) = fs
          fromOr f = singleton f

orFromSet :: Set Formula -> Formula
orFromSet fs = if member true fs
                  then true
                  else checkConstraints Or false orRelations (createOrSet fs)

-- | Creates a logical disjunction of two given formulas, e.g.
--
-- > f \/ true  == true
-- > f \/ false == f
-- > f \/ g     == g \/ f
infixr 4 \/
(\/) :: Formula -> Formula -> Formula
Formula _ T \/ _ = true
_ \/ Formula _ T = true
Formula _ F \/ f = f
f \/ Formula _ F = f
f1 \/ f2 = or [f1,f2]

-- | Creates a logical disjunction of a given list of formulas.
or :: [Formula] -> Formula
or = orFromSet . fromList

simplifiedOr :: [Formula] -> Formula
simplifiedOr = Formula True . Or . fromList

----------------------------------------------------------------------------------------------------
-- Not
----------------------------------------------------------------------------------------------------

createNot :: FormulaStructure -> FormulaStructure
createNot T = F
createNot F = T
createNot (Constraint LessThan x1 x2) = Constraint GreaterEquals x1 x2
createNot (Constraint LessEquals x1 x2) = Constraint GreaterThan x1 x2
createNot (Constraint Equals x1 x2) = Constraint NotEquals x1 x2
createNot (Constraint NotEquals x1 x2) = Constraint Equals x1 x2
createNot (Constraint GreaterThan x1 x2) = Constraint LessEquals x1 x2
createNot (Constraint GreaterEquals x1 x2) = Constraint LessThan x1 x2
createNot (And fs) = Or $ map not fs
createNot (Or fs) = And $ map not fs
createNot (Not (Formula _ f)) = f

-- | Creates a negation of a given formula, e.g.
--
-- > not true    == false
-- > not false   == true
-- > not (not f) == f
not :: Formula -> Formula
not (Formula s f) = Formula s (createNot f)

----------------------------------------------------------------------------------------------------
-- Imply
----------------------------------------------------------------------------------------------------

-- | Creates an implication of given formulas, e.g.
--
-- > false ==> f    == true
-- > f     ==> true == true
-- > f     ==> g    == g <== f
infix 5 ==>
(==>) :: Formula -> Formula -> Formula
f1 ==> f2 = not f1 \/ f2

-- | Creates an implication of given formulas, e.g.
--
-- > f    <== false == true
-- > true <== f     == true
-- > f    <== g     == g ==> f
infix 5 <==
(<==) :: Formula -> Formula -> Formula
f1 <== f2 = f1 \/ not f2

-- | Creates a implication of given formulas, equivalent to '==>'.
implies :: Formula -> Formula -> Formula
implies = (==>)

----------------------------------------------------------------------------------------------------
-- Equivalent
----------------------------------------------------------------------------------------------------

-- | Creates a formula representing "if and only if" relation between given formulas, e.g.
--
-- > f <==> f     == true
-- > f <==> not f == false
-- > f <==> g     == (f /\ g \/ not f /\ not g)
infix 6 <==>
(<==>) :: Formula -> Formula -> Formula
f1 <==> f2 = (f1 /\ f2) \/ (not f1 /\ not f2)

-- | Equivalent to '<==>'.
iff :: Formula -> Formula -> Formula
iff = (<==>)

----------------------------------------------------------------------------------------------------
-- Variables functions
----------------------------------------------------------------------------------------------------

foldFormulaStructureVariables :: (Variable -> a -> a) -> a -> FormulaStructure -> a
foldFormulaStructureVariables fun acc = doFold
    where doFold T = acc
          doFold F = acc
          doFold (Constraint _ x1 x2) = fun x2 $ fun x1 acc
          doFold (And fs) = foldl (foldFormulaVariables fun) acc fs
          doFold (Or fs) = foldl (foldFormulaVariables fun) acc fs
          doFold (Not f) = foldFormulaVariables fun acc f

foldFormulaVariables :: (Variable -> a -> a) -> a -> Formula -> a
foldFormulaVariables fun acc = foldFormulaStructureVariables fun acc . formula

freeVariables :: Formula -> Set Variable
freeVariables = foldFormulaVariables (\x -> if isConstant x then id else insert x) empty

mapFormulaStructureVariables :: (Variable -> Variable) -> FormulaStructure -> FormulaStructure
mapFormulaStructureVariables fun = formula . mapFormulaVariables fun . Formula False

mapFormulaVariables :: (Variable -> Variable) -> Formula -> Formula
mapFormulaVariables fun = doMap . formula
    where doMap T = true
          doMap F = false
          doMap (Constraint r x1 x2) = constraint r (fun x1) (fun x2)
          doMap (And fs) = andFromSet $ map (mapFormulaVariables fun) fs
          doMap (Or fs) = orFromSet $ map (mapFormulaVariables fun) fs
          doMap (Not f) = not $ mapFormulaVariables fun f

mapFormula :: (Formula -> Formula) -> Formula -> Formula
mapFormula fun f = doMap fun (formula f)
    where doMap _ T = f
          doMap _ F = f
          doMap fun (Constraint r x1 x2) = f
          doMap fun (And fs) = andFromSet $ map fun fs
          doMap fun (Or fs) = orFromSet $ map fun fs
          doMap fun (Not f) = not $ fun f

replaceFormulaVariable :: Variable -> Variable -> Formula -> Formula
replaceFormulaVariable oldVar newVar = mapFormulaVariables (\var -> if oldVar == var then newVar else var)

getEquationsFromFormula :: Formula -> Set (Variable, Variable)
getEquationsFromFormula f = go $ formula f
    where go (Constraint Equals x1 x2) = singleton (x1, x2)
          go (And fs) = unions $ elems $ map (go . formula) fs
          go _ = empty

getConstraintsFromFormula :: Formula -> [(Relation, Variable, Variable)]
getConstraintsFromFormula f = go $ formula f
    where go (Constraint r x1 x2) = [(r, x1, x2)]
          go (And fs) = concatMap (go . formula) $ elems fs
          go (Or fs) = concatMap (go . formula) $ elems fs
          go (Not f) = go $ formula f
          go _ = []

instance Var FormulaStructure where
    mapVariables (_, f) = mapFormulaStructureVariables f
    foldVariables (_, f) = foldFormulaStructureVariables f
    renameVariables = renameWithFlatTree

instance Var Formula where
    mapVariables (_, f) = mapFormulaVariables f
    foldVariables (_, f) = foldFormulaVariables f
    renameVariables = renameWithFlatTree

----------------------------------------------------------------------------------------------------
-- Meta equivalents
----------------------------------------------------------------------------------------------------

nlambda_true :: WithMeta Formula
nlambda_true = noMeta true

nlambda_false :: WithMeta Formula
nlambda_false = noMeta false

nlambda_fromBool :: WithMeta Bool -> WithMeta Formula
nlambda_fromBool = idOp fromBool

nlambda_constraint :: WithMeta Relation -> WithMeta Variable -> WithMeta Variable -> WithMeta Formula
nlambda_constraint = renameAndApply3 constraint

nlambda_andFromSet :: WithMeta (Set Formula) -> WithMeta Formula
nlambda_andFromSet = idOp andFromSet

nlambda_and :: WithMeta [Formula] -> WithMeta Formula
nlambda_and = idOp and

nlambda_orFromSet :: WithMeta (Set Formula) -> WithMeta Formula
nlambda_orFromSet = idOp orFromSet

nlambda_or :: WithMeta [Formula] -> WithMeta Formula
nlambda_or = idOp or

nlambda_not :: WithMeta Formula -> WithMeta Formula
nlambda_not = idOp not
