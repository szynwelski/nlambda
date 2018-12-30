{-# LANGUAGE CPP, DefaultSignatures, FlexibleContexts, TypeOperators #-}

module Nominal.Variable (
Identifier,
Variable,
constantVar,
variable,
iterationVariable,
iterationVariableWithId,
iterationVariablesList,
isConstant,
isVariableChar,
constantValue,
getIdentifier,
setIdentifier,
hasIdentifierEquals,
hasIdentifierNotEquals,
clearIdentifier,
getIterationLevel,
changeIterationLevel,
toParts,
fromParts,
FoldVarFun,
IdMap,
MapVarFun,
RenameTree(..),
Scope(..),
Var(..),
collectWith,
getAllVariables,
freeVariables,
mapVariablesIf,
replaceVariables,
renameFreeVariables,
addMapToTree,
createNode,
getChildrenOrNode,
isTreeEmpty,
renameWithFlatTree) where

import Data.Char (isAlphaNum, isLetter, isLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Word (Word)
import GHC.Generics
import Numeric (showIntAtBase)
import Nominal.Atoms.Signature (Constant, Relation, readConstant, showConstant)
import qualified Nominal.Text.Symbols as Symbols
import Nominal.Util.Read (skipSpaces)
import Text.ParserCombinators.ReadP (munch, satisfy)
import Text.Read (Lexeme(Ident), ReadPrec, (+++), (<++), lexP, lift, parens, readPrec)
import Text.Read.Lex (readIntP)

----------------------------------------------------------------------------------------------------
-- Variable
----------------------------------------------------------------------------------------------------

type Identifier = Word

-- | Free variable in a 'Nominal.Formula' or iteration variable in a 'Nominal.Set' or constant.
data Variable = Var String | IterationVariable Int Int (Maybe Identifier) | ConstantVar Constant deriving (Eq, Ord)

----------------------------------------------------------------------------------------------------
-- Constant
----------------------------------------------------------------------------------------------------

isConstant :: Variable -> Bool
isConstant (ConstantVar _) = True
isConstant _ = False

constantValue :: Variable -> Constant
constantValue (ConstantVar value) = value
constantValue _ = error "function constantValue can be applied only for constants"

----------------------------------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------------------------------

variableNameBeforeIndex :: Int -> String
variableNameBeforeIndex level = showIntAtBase 25 (toEnum . (+97)) level ""

instance Show Variable where
    show (Var name) = name
    show (IterationVariable level index id) = variableNameBeforeIndex level ++ Symbols.subscriptIndex index
--        ++ maybe "" (\i -> "(" ++ show i ++ ")") id -- for tests
    show (ConstantVar value) = showConstant value

----------------------------------------------------------------------------------------------------
-- Read
----------------------------------------------------------------------------------------------------

readLevelFromVariableNameBeforeIndex :: ReadPrec Int
readLevelFromVariableNameBeforeIndex = lift $ readIntP 25 isLower ((+ (-97)) . fromEnum)

instance Read Variable where
    readPrec = parens $ do value <- readConstant
                           return $ ConstantVar value
                        +++
                        do skipSpaces
                           level <- readLevelFromVariableNameBeforeIndex
                           index <- Symbols.readSubscriptIndex
                           return $ iterationVariable level index
                        <++
                        do x <- lift $ satisfy isLetter
                           y <- lift $ munch isVariableChar
                           return $ Var $ x:y

----------------------------------------------------------------------------------------------------
-- Variable parts
----------------------------------------------------------------------------------------------------

toParts :: Variable -> Either String (Int, Int, Maybe Identifier)
toParts (Var name) = Left name
toParts (IterationVariable level index id) = Right (level, index, id)

fromParts :: Either String (Int, Int, Maybe Identifier) -> Variable
fromParts (Left name) = Var name
fromParts (Right (level, index, id)) = IterationVariable level index id

----------------------------------------------------------------------------------------------------
-- Variable constructors
----------------------------------------------------------------------------------------------------

-- | Creates a constant with a given value

constantVar :: Constant -> Variable
constantVar = ConstantVar

isVariableChar :: Char -> Bool
isVariableChar c = isAlphaNum c || c == '_' || c == '-' || c == '.'

-- | Creates a variable with a given name.
variable :: String -> Variable
variable (x:y) = if isLetter x && all isVariableChar y
                 then Var (x:y)
                 else error "variable name must start with a letter and contain only alphanumeric characters, dot, underscore or hyphen"
variable _ = error "variable name is empty"

iterationVariable :: Int -> Int -> Variable
iterationVariable level index = IterationVariable level index Nothing

iterationVariableWithId :: Int -> Int -> Identifier -> Variable
iterationVariableWithId level index id = IterationVariable level index (Just id)

iterationVariablesList :: Int -> Int -> [Variable]
iterationVariablesList level size = fmap (iterationVariable level) [1..size]

----------------------------------------------------------------------------------------------------
-- Operations on iteratation variables
----------------------------------------------------------------------------------------------------

onlyForIteration :: a -> (Int -> Int -> Maybe Identifier -> a) -> Variable -> a
onlyForIteration result _ (ConstantVar _) = result
onlyForIteration result _ (Var _) = result
onlyForIteration _ f (IterationVariable level index id) = f level index id

getIdentifier :: Variable -> Maybe Identifier
getIdentifier = onlyForIteration Nothing (\_ _ id -> id)

setIdentifier :: Identifier -> Variable -> Variable
setIdentifier id v = onlyForIteration v (\l i _ -> IterationVariable l i (Just id)) v

hasIdentifierEquals :: Identifier -> Variable -> Bool
hasIdentifierEquals t = onlyForIteration False (\_ _ id -> maybe False (== t) id)

hasIdentifierNotEquals :: Identifier -> Variable -> Bool
hasIdentifierNotEquals t = onlyForIteration False (\_ _ id -> maybe True (/= t) id)

clearIdentifier :: Variable -> Variable
clearIdentifier v = onlyForIteration v (\l i _ -> iterationVariable l i) v

getIterationLevel :: Variable -> Maybe Int
getIterationLevel = onlyForIteration Nothing (\l _ _ -> Just l)

changeIterationLevel :: Map Int Int -> Variable -> Variable
changeIterationLevel m v = onlyForIteration v (\l i id -> IterationVariable (Map.findWithDefault l l m) i id) v

----------------------------------------------------------------------------------------------------
-- Rename tree
----------------------------------------------------------------------------------------------------

type IdMap = Map Identifier Identifier
data RenameTree = Empty | Node IdMap [RenameTree] deriving Show

isTreeEmpty :: RenameTree -> Bool
isTreeEmpty Empty = True
isTreeEmpty (Node map ts) = null map && all isTreeEmpty ts

createNode :: Bool -> [RenameTree] -> RenameTree
createNode checkEmpty ts
    | checkEmpty, all isTreeEmpty ts = Empty
    | otherwise = Node Map.empty ts

addMapToTree :: IdMap -> RenameTree -> RenameTree
addMapToTree map t | null map = t
addMapToTree map Empty = Node map []
addMapToTree map (Node m ts) = Node (Map.union m map) ts

getChildren :: RenameTree -> [RenameTree]
getChildren Empty = []
getChildren (Node map ts) = addMapToTree map <$> ts

getChildrenOrNode :: RenameTree -> [RenameTree]
getChildrenOrNode Empty = [Empty]
getChildrenOrNode t@(Node _ []) = [t]
getChildrenOrNode t = getChildren t

flatMap :: RenameTree -> IdMap
flatMap Empty = Map.empty
flatMap (Node map ts) = Map.unionsWith (\x y -> if x == y then x else error "flatMap - different values in rename tree")
                                       (map : fmap flatMap ts)

----------------------------------------------------------------------------------------------------
-- Class Var
----------------------------------------------------------------------------------------------------

data Scope = All | Free

type MapVarFun = (Scope, Variable -> Variable)
type FoldVarFun b = (Scope, Variable -> b -> b)

class Var a where
    mapVariables :: MapVarFun -> a -> a
    default mapVariables :: (Generic a, GVar (Rep a)) => MapVarFun -> a -> a
    mapVariables f x = to (gmapVariables f (from x))

    foldVariables :: FoldVarFun b -> b -> a -> b
    default foldVariables :: (Generic a, GVar (Rep a)) => FoldVarFun b -> b -> a -> b
    foldVariables f b x = gfoldVariables f b (from x)

    renameVariables :: RenameTree -> a -> a
    default renameVariables :: (Generic a, GVar (Rep a)) => RenameTree -> a -> a
    renameVariables Empty x = x
    renameVariables (Node map []) x = mapVariables (All, renameVarFun map) x
    renameVariables rt x = to (snd $ grenameVariables (getChildren rt) (from x))

renameVarFun :: IdMap -> Variable -> Variable
renameVarFun map v = maybe v (\id -> setIdentifier (Map.findWithDefault id id map) v) (getIdentifier v)

renameWithFlatTree :: Var a => RenameTree -> a -> a
renameWithFlatTree rt = mapVariables (All, renameVarFun $ flatMap rt)

renameFreeVariables :: Var a => IdMap -> a -> a
renameFreeVariables map = mapVariables (Free, renameVarFun map)

----------------------------------------------------------------------------------------------------
-- Operations on variables
----------------------------------------------------------------------------------------------------

collectWith :: (Var a, Ord b) => (Variable -> Maybe b) -> a -> Set b
collectWith cf = foldVariables (All, maybe id Set.insert . cf) Set.empty

getAllVariables :: Var a => a -> Set Variable
getAllVariables = foldVariables (All, Set.insert) Set.empty

freeVariables :: Var a => a -> Set Variable
freeVariables = foldVariables (Free, Set.insert) Set.empty

mapVariablesIf :: Var a => (Variable -> Bool) -> (Variable -> Variable) -> a -> a
mapVariablesIf cf mf = mapVariables (All, \v -> if cf v then mf v else v)

replaceVariables :: Var a => Map Variable Variable -> a -> a
replaceVariables varsMap = mapVariables (All, \var -> Map.findWithDefault var var varsMap)

----------------------------------------------------------------------------------------------------
-- Var instances
----------------------------------------------------------------------------------------------------

#define VAR_INSTANCE(type)        \
instance Var (type) where;        \
    mapVariables _ = id;          \
    foldVariables _ acc _ = acc;  \
    renameVariables _ = id;       \

VAR_INSTANCE(Bool)
VAR_INSTANCE(Char)
VAR_INSTANCE(Double)
VAR_INSTANCE(Float)
VAR_INSTANCE(Int)
VAR_INSTANCE(Integer)
VAR_INSTANCE(Ordering)
VAR_INSTANCE(Word)
VAR_INSTANCE((a -> b))
VAR_INSTANCE(Relation)
VAR_INSTANCE(ReadPrec a)
VAR_INSTANCE(C)
VAR_INSTANCE(D)
VAR_INSTANCE(R)
VAR_INSTANCE(S)
VAR_INSTANCE(NoSelector)

instance Var ()
instance Var a => Var [a]
instance Var a => Var (Maybe a)
instance (Var a, Var b) => Var (Either a b)
instance (Var a, Var b) => Var (a, b)
instance (Var a, Var b, Var c) => Var (a, b, c)
instance (Var a, Var b, Var c, Var d) => Var (a, b, c, d)
instance (Var a, Var b, Var c, Var d, Var e) => Var (a, b, c, d, e)
instance (Var a, Var b, Var c, Var d, Var e, Var f) => Var (a, b, c, d, e, f)
instance (Var a, Var b, Var c, Var d, Var e, Var f, Var g) => Var (a, b, c, d, e, f, g)
instance (Var (f p), Var (g p)) => Var ((f :*: g) p)
instance (Var (f p), Var (g p)) => Var ((f :+: g) p)
instance Var c => Var (K1 i c p)
instance Var (f p) => Var (M1 i c f p)
instance Var p => Var (Par1 p)
instance Var (f p) => Var (Rec1 f p)
instance Var (U1 p)

instance (Integral a, Var a) => Var (Ratio a) where
    mapVariables f r = mapVariables f (numerator r) % mapVariables f (denominator r)
    foldVariables f x r = foldVariables f x [numerator r, denominator r]
    renameVariables rt r = renameVariables t1 (numerator r) % renameVariables t2 (denominator r)
        where [t1,t2] = getChildren rt

instance (Ord a, Var a) => Var (Set.Set a) where
    mapVariables f = Set.map (mapVariables f)
    foldVariables f acc = foldVariables f acc . Set.elems
    renameVariables = renameWithFlatTree

instance (Ord k, Var k, Var a) => Var (Map k a) where
    mapVariables f = Map.fromList . mapVariables f . Map.assocs
    foldVariables f acc = foldVariables f acc . Map.assocs
    renameVariables = renameWithFlatTree

instance (Ord k, Var k, Var a) => Var (MultiMap k a) where
    mapVariables f = MM.fromList . mapVariables f . MM.toList
    foldVariables f acc = foldVariables f acc . MM.toList
    renameVariables = renameWithFlatTree

instance Var Variable where
    mapVariables (_, f) = f
    foldVariables (_, f) acc v = f v acc
    renameVariables Empty = id
    renameVariables (Node map _) = renameVarFun map

----------------------------------------------------------------------------------------------------
-- Class GVar
----------------------------------------------------------------------------------------------------

class GVar f where
    gmapVariables :: MapVarFun -> f a -> f a
    gfoldVariables :: FoldVarFun b -> b -> f a -> b
    grenameVariables :: [RenameTree] -> f a -> ([RenameTree], f a)

-- For the void type (no constructor)
instance GVar V1 where
    gmapVariables _ = id
    gfoldVariables _ acc _ = acc
    grenameVariables _ v = ([], v)

-- For the unit type (constructors without fields)
instance GVar U1 where
    gmapVariables _ = id
    gfoldVariables _ acc _ = acc
    grenameVariables _ u = ([], u)

-- For constants
instance Var c => GVar (K1 i c) where
    gmapVariables f (K1 a) = K1 $ mapVariables f a
    gfoldVariables f b (K1 a) = foldVariables f b a
    grenameVariables rts (K1 a) = (tail rts, K1 $ renameVariables (head rts) a)

-- For constructors with meta information (which we ignore)
instance GVar a => GVar (M1 i c a) where
    gmapVariables f (M1 a) = M1 $ gmapVariables f a
    gfoldVariables f b (M1 a) = gfoldVariables f b a
    grenameVariables rts (M1 a) = let (rts', a') = grenameVariables rts a in (rts', M1 a')

-- For sums
instance (GVar a, GVar b) => GVar (a :+: b) where
    gmapVariables f (L1 a) = L1 $ gmapVariables f a
    gmapVariables f (R1 a) = R1 $ gmapVariables f a
    gfoldVariables f b (L1 a) = gfoldVariables f b a
    gfoldVariables f b (R1 a) = gfoldVariables f b a
    grenameVariables rts (L1 a) = let (rts', a') = grenameVariables rts a in (rts', L1 a')
    grenameVariables rts (R1 a) = let (rts', a') = grenameVariables rts a in (rts', R1 a')

-- For products
instance (GVar a, GVar b) => GVar (a :*: b) where
    gmapVariables f (a :*: b) = gmapVariables f a :*: gmapVariables f b
    gfoldVariables f c (a :*: b) = gfoldVariables f (gfoldVariables f c a) b
    grenameVariables rts (a :*: b) = (rts'', a' :*: b')
        where (rts', a') = grenameVariables rts a
              (rts'', b') = grenameVariables rts' b
