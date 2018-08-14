{-# LANGUAGE DefaultSignatures, FlexibleContexts, CPP, TypeOperators #-}

module Var where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

----------------------------------------------------------------------------------------------------
-- Variable
----------------------------------------------------------------------------------------------------

type Identifier = Int
data Variable = Variable Identifier deriving (Eq, Ord)

instance Show Variable where
    show (Variable n) = "a" ++ show n

----------------------------------------------------------------------------------------------------
-- Rename tree
----------------------------------------------------------------------------------------------------

type IdMap = Map Identifier Identifier
data RenameTree = Empty | Node IdMap [RenameTree] deriving Show

isTreeEmpty :: RenameTree -> Bool
isTreeEmpty Empty = True
isTreeEmpty (Node map ts) = Map.null map && all isTreeEmpty ts

createNode :: Bool -> [RenameTree] -> RenameTree
createNode checkEmpty ts
    | checkEmpty, all isTreeEmpty ts = Empty
    | otherwise = Node Map.empty ts

addMapToTree :: IdMap -> RenameTree -> RenameTree
addMapToTree map t | Map.null map = t
addMapToTree map Empty = Node map []
addMapToTree map (Node m ts) = Node (Map.union m map) ts

getChildren :: RenameTree -> [RenameTree]
getChildren Empty = []
getChildren (Node map ts) = addMapToTree map <$> ts

getChildrenOrNode :: RenameTree -> [RenameTree]
getChildrenOrNode Empty = [Empty]
getChildrenOrNode t@(Node _ []) = [t]
getChildrenOrNode t = getChildren t

----------------------------------------------------------------------------------------------------
-- Var Type
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
    renameVariables (Node map []) x = mapVariables (All, \(Variable id) -> Variable $ Map.findWithDefault id id map) x
    renameVariables rt x = to (snd $ grenameVariables (getChildren rt) (from x))

----------------------------------------------------------------------------------------------------
-- Operations on all variables
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
-- Instances
----------------------------------------------------------------------------------------------------

#define VAR_INSTANCE(foo)         \
instance Var foo where;   \
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

instance (Integral a, Var a) => Var (Ratio a) where
    mapVariables f r = mapVariables f (numerator r) % mapVariables f (denominator r)
    foldVariables f x r = foldVariables f x [numerator r, denominator r]
    renameVariables rt r = renameVariables t1 (numerator r) % renameVariables t2 (denominator r)
        where [t1,t2] = getChildren rt

instance Var Variable where
    mapVariables (_, f) = f
    foldVariables (_, f) acc v = f v acc
    renameVariables Empty v = v
    renameVariables (Node map _) (Variable id) = Variable $ Map.findWithDefault id id map

----------------------------------------------------------------------------------------------------
-- GVar class
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
