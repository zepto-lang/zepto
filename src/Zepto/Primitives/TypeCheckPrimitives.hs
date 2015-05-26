module Zepto.Primitives.TypeCheckPrimitives where
import Control.Monad.Except (throwError)
import Data.Complex (imagPart)

import Zepto.Types

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber ([Number _]) = return $ Bool True
isNumber _ = return $ Bool False

isReal :: [LispVal] -> ThrowsError LispVal
isReal ([Number (NumC x)]) = return $ Bool $ imagPart x == 0
isReal ([Number _]) = return $ Bool True
isReal _ = return $ Bool False

isInteger :: [LispVal] -> ThrowsError LispVal
isInteger ([Number (NumI _)]) = return $ Bool True
isInteger ([Number (NumS _)]) = return $ Bool True
isInteger _ = return $ Bool False

isSmall :: [LispVal] -> ThrowsError LispVal
isSmall ([Number (NumS _)]) = return $ Bool True
isSmall _ = return $ Bool False

isFloat :: [LispVal] -> ThrowsError LispVal
isFloat ([Number (NumF _)]) = return $ Bool True
isFloat _ = return $ Bool False

isRational :: [LispVal] -> ThrowsError LispVal
isRational ([Number (NumR _)]) = return $ Bool True
isRational ([Number (NumS _)]) = return $ Bool True
isRational ([Number (NumI _)]) = return $ Bool True
isRational ([Number (NumF x)]) =
        if x == fromInteger (round x)
            then return $ Bool True
            else return $ Bool False
isRational _ = return $ Bool False

isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList ([DottedList _ _]) = return $ Bool True
isDottedList _ = return $ Bool False

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure ([PrimitiveFunc _]) = return $ Bool True
isProcedure ([EvalFunc _]) = return $ Bool True
isProcedure ([Func _]) = return $ Bool True
isProcedure ([IOFunc _]) = return $ Bool True
isProcedure ([Cont _]) = return $ Bool True
isProcedure _ = return $ Bool False

isVector, isList :: LispVal -> ThrowsError LispVal
isVector (Vector _) = return $ Bool True
isVector _ = return $ Bool False
isList (List _) = return $ Bool True
isList _ = return $ Bool False

isNull :: [LispVal] -> ThrowsError LispVal
isNull ([List []]) = return $ Bool True
isNull ([Nil _]) = return $ Bool True
isNull _ = return $ Bool False

isNil :: [LispVal] ->ThrowsError LispVal
isNil ([Nil _]) = return $ Bool True
isNil _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol ([Atom (':' : _)]) = return $ Bool False
isSymbol ([Atom _]) = return $ Bool True
isSymbol _ = return $ Bool False

isAtom :: [LispVal] -> ThrowsError LispVal
isAtom ([Atom (':' : _)]) = return $ Bool True
isAtom _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString ([String _]) = return $ Bool True
isString _ = return $ Bool False

isPort :: [LispVal] -> ThrowsError LispVal
isPort ([Port _]) = return $ Bool True
isPort _ = return $ Bool False

-- Implement
isInputPort :: [LispVal] -> ThrowsError LispVal
isInputPort ([Port _]) = return $ Bool True
isInputPort _ = return $ Bool False

-- Implement
isOutputPort :: [LispVal] -> ThrowsError LispVal
isOutputPort ([Port _]) = return $ Bool True
isOutputPort _ = return $ Bool False

isChar :: [LispVal] -> ThrowsError LispVal
isChar ([Character _]) = return $ Bool True
isChar _ = return $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean ([Bool _]) = return $ Bool True
isBoolean _ = return $ Bool False

checkType :: [LispVal] -> ThrowsError LispVal
checkType [x] = return $ String $ typeString x
checkType badArgList = throwError $ NumArgs 1 badArgList

