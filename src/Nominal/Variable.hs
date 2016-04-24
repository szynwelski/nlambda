module Nominal.Variable (
Identifier,
Variable,
constantVar,
variable,
variableName,
variableNameAscii,
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
fromVariableNameAscii) where

import Data.List.Utils (split)
import Data.Map (Map, findWithDefault)
import Data.Maybe (isNothing)
import Numeric (readInt, showIntAtBase)

----------------------------------------------------------------------------------------------------
-- Variable
----------------------------------------------------------------------------------------------------

type Identifier = Int

-- | Free variable in a 'Nominal.Formula' or iteration variable in a 'Nominal.Set'.
data Variable = Var String | IterationVariable Int Int (Maybe Identifier) | ConstantVar String deriving (Eq, Ord)

isConstant :: Variable -> Bool
isConstant (ConstantVar _) = True
isConstant _ = False

constantValue :: Variable -> String
constantValue (ConstantVar value) = value
constantValue _ = error "function constantValue can be applied only for constants"

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
createVariableName _ _ = error "constant has no name"

-- | Returns the name of the variable.
variableName :: Variable -> String
variableName = createVariableName variableNameWithIndex

variableNameAscii :: Variable -> String
variableNameAscii = createVariableName variableNameAsciiWithIndex

instance Show Variable where
    show v = if isConstant v then constantValue v else variableName v

----------------------------------------------------------------------------------------------------
-- Variable constructors
----------------------------------------------------------------------------------------------------

-- | Creates a constant with a given value

constantVar :: String -> Variable
constantVar = ConstantVar

-- | Creates a variable with a given name.
variable :: String -> Variable
variable = Var

iterationVariable :: Int -> Int -> Variable
iterationVariable level index = IterationVariable level index Nothing

iterationVariablesList :: Int -> Int -> [Variable]
iterationVariablesList level size = fmap (iterationVariable level) [1..size]

fromVariableNameBeforeIndex :: String -> Int
fromVariableNameBeforeIndex = fst . head . readInt 25 ((`elem` [97..122]) . fromEnum) ((subtract 97) . fromEnum)

fromVariableNameAscii :: String -> Variable
fromVariableNameAscii name =
    let parts = split "_" name
    in case length parts of
         1 -> Var $ head parts
         2 -> IterationVariable (fromVariableNameBeforeIndex $ head parts) (read $ parts !! 1) Nothing
         3 -> IterationVariable (fromVariableNameBeforeIndex $ head parts) (read $ parts !! 1) (Just $ read $ parts !! 2)

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
