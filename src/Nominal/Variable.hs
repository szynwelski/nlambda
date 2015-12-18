module Nominal.Variable (
Identifier,
Variable,
variableName,
variableNameAscii,
variable,
iterationVariable,
iterationVariablesList,
setIdentifier,
hasIdentifierEquals,
hasIdentifierNotEquals,
clearIdentifier,
getIterationLevel,
changeIterationLevel) where

import Data.Map (Map, findWithDefault)
import Data.Maybe (isNothing)
import Numeric (showIntAtBase)

----------------------------------------------------------------------------------------------------
-- Variable
----------------------------------------------------------------------------------------------------

type Identifier = Int
data Variable = Var String | IterationVariable Int Int (Maybe Identifier) deriving (Eq, Ord)

---------------------------------------------------------------------------------------------------
-- Variable name
----------------------------------------------------------------------------------------------------

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

variableNameBeforeIndex :: Int -> String
variableNameBeforeIndex charIndex = showIntAtBase 25 (toEnum . (+97)) charIndex ""

variableNameWithIndex :: Int -> Int -> Maybe Identifier -> String
variableNameWithIndex charIndex varIndex _ = variableNameBeforeIndex charIndex ++ fmap (toEnum . (+ 8320)) (digits varIndex)

variableNameAsciiWithIndex :: Int -> Int -> Maybe Identifier -> String
variableNameAsciiWithIndex charIndex varIndex id = variableNameBeforeIndex charIndex ++ '_' : show varIndex ++ maybe "" (("_" ++) . show) id

createVariableName :: (Int -> Int -> Maybe Identifier -> String) -> Variable -> String
createVariableName _ (Var name) = name
createVariableName indexName (IterationVariable level index id) = indexName level index id

variableName :: Variable -> String
variableName = createVariableName variableNameWithIndex

variableNameAscii :: Variable -> String
variableNameAscii = createVariableName variableNameAsciiWithIndex

instance Show Variable where
    show = variableName

----------------------------------------------------------------------------------------------------
-- Variable constructors
----------------------------------------------------------------------------------------------------

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
