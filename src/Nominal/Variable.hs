{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Nominal.Variable (
Identifier,
Variable,
constantVar,
variable,
iterationVariable,
iterationVariablesList,
isConstant,
constantValue,
setIdentifier,
hasIdentifierEquals,
hasIdentifierNotEquals,
clearIdentifier,
getIterationLevel,
changeIterationLevel,
toParts,
fromParts) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Map (Map, findWithDefault)
import Data.Word (Word)
import Numeric (showIntAtBase)
import Nominal.Atoms.Signature (Constant, showConstant)
import qualified Nominal.Text.Symbols as Symbols

----------------------------------------------------------------------------------------------------
-- Variable
----------------------------------------------------------------------------------------------------

type Identifier = Word

-- | Free variable in a 'Nominal.Formula' or iteration variable in a 'Nominal.Set' or constant.
data Variable = Var String | IterationVariable Int Int (Maybe Identifier) | ConstantVar Constant deriving (Eq, Ord, Generic, NFData)

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
    show (IterationVariable level index _) = variableNameBeforeIndex level ++ Symbols.subscriptIndex index
    show (ConstantVar value) = showConstant value

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

-- | Creates a variable with a given name.
variable :: String -> Variable
variable = Var

iterationVariable :: Int -> Int -> Variable
iterationVariable level index = IterationVariable level index Nothing

iterationVariablesList :: Int -> Int -> [Variable]
iterationVariablesList level size = fmap (iterationVariable level) [1..size]

----------------------------------------------------------------------------------------------------
-- Operations on iteratation variables
----------------------------------------------------------------------------------------------------

onlyForIteration :: a -> (Int -> Int -> Maybe Identifier -> a) -> Variable -> a
onlyForIteration result _ (ConstantVar _) = result
onlyForIteration result _ (Var _) = result
onlyForIteration _ f (IterationVariable level index id) = f level index id

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
changeIterationLevel m v = onlyForIteration v (\l i id -> IterationVariable (findWithDefault l l m) i id) v
