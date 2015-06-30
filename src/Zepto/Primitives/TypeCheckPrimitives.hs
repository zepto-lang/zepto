module Zepto.Primitives.TypeCheckPrimitives where
import Control.Monad.Except (throwError)
import Data.Complex (imagPart)

import Zepto.Types

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber ([SimpleVal (Number _)]) = return $ fromSimple $ Bool True
isNumber _ = return $ fromSimple $ Bool False

isReal :: [LispVal] -> ThrowsError LispVal
isReal ([SimpleVal (Number (NumC x))]) = return $ fromSimple $ Bool $ imagPart x == 0
isReal ([SimpleVal (Number _)]) = return $ fromSimple $ Bool True
isReal _ = return $ fromSimple $ Bool False

isInteger :: [LispVal] -> ThrowsError LispVal
isInteger ([SimpleVal (Number (NumI _))]) = return $ fromSimple $ Bool True
isInteger ([SimpleVal (Number (NumS _))]) = return $ fromSimple $ Bool True
isInteger _ = return $ fromSimple $ Bool False

isSmall :: [LispVal] -> ThrowsError LispVal
isSmall ([SimpleVal (Number (NumS _))]) = return $ fromSimple $ Bool True
isSmall _ = return $ fromSimple $ Bool False

isFloat :: [LispVal] -> ThrowsError LispVal
isFloat ([SimpleVal (Number (NumF _))]) = return $ fromSimple $ Bool True
isFloat _ = return $ fromSimple $ Bool False

isRational :: [LispVal] -> ThrowsError LispVal
isRational ([SimpleVal (Number (NumR _))]) = return $ fromSimple $ Bool True
isRational ([SimpleVal (Number (NumS _))]) = return $ fromSimple $ Bool True
isRational ([SimpleVal (Number (NumI _))]) = return $ fromSimple $ Bool True
isRational ([SimpleVal (Number (NumF x))]) =
        if x == fromInteger (round x)
            then return $ fromSimple $ Bool True
            else return $ fromSimple $ Bool False
isRational _ = return $ fromSimple $ Bool False

isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList ([DottedList _ _]) = return $ fromSimple $ Bool True
isDottedList _ = return $ fromSimple $ Bool False

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure ([PrimitiveFunc _]) = return $ fromSimple $ Bool True
isProcedure ([EvalFunc _]) = return $ fromSimple $ Bool True
isProcedure ([Func _]) = return $ fromSimple $ Bool True
isProcedure ([IOFunc _]) = return $ fromSimple $ Bool True
isProcedure ([Cont _]) = return $ fromSimple $ Bool True
isProcedure _ = return $ fromSimple $ Bool False

isSimple :: [LispVal] -> ThrowsError LispVal
isSimple [SimpleVal _] = return $ fromSimple $ Bool True
isSimple _ = return $ fromSimple $ Bool False

isVector, isList :: LispVal -> ThrowsError LispVal
isVector (Vector _) = return $ fromSimple $ Bool True
isVector _ = return $ fromSimple $ Bool False
isList (List _) = return $ fromSimple $ Bool True
isList _ = return $ fromSimple $ Bool False

isNull :: [LispVal] -> ThrowsError LispVal
isNull ([List []]) = return $ fromSimple $ Bool True
isNull ([SimpleVal (Nil _)]) = return $ fromSimple $ Bool True
isNull _ = return $ fromSimple $ Bool False

isNil :: [LispVal] ->ThrowsError LispVal
isNil ([SimpleVal (Nil _)]) = return $ fromSimple $ Bool True
isNil _ = return $ fromSimple $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol ([SimpleVal (Atom (':' : _))]) = return $ fromSimple $ Bool False
isSymbol ([SimpleVal (Atom _)]) = return $ fromSimple $ Bool True
isSymbol _ = return $ fromSimple $ Bool False

isAtom :: [LispVal] -> ThrowsError LispVal
isAtom ([SimpleVal (Atom (':' : _))]) = return $ fromSimple $ Bool True
isAtom _ = return $ fromSimple $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString ([SimpleVal (String _)]) = return $ fromSimple $ Bool True
isString _ = return $ fromSimple $ Bool False

isPort :: [LispVal] -> ThrowsError LispVal
isPort ([Port _]) = return $ fromSimple $ Bool True
isPort _ = return $ fromSimple $ Bool False

-- Implement
isInputPort :: [LispVal] -> ThrowsError LispVal
isInputPort ([Port _]) = return $ fromSimple $ Bool True
isInputPort _ = return $ fromSimple $ Bool False

-- Implement
isOutputPort :: [LispVal] -> ThrowsError LispVal
isOutputPort ([Port _]) = return $ fromSimple $ Bool True
isOutputPort _ = return $ fromSimple $ Bool False

isChar :: [LispVal] -> ThrowsError LispVal
isChar ([SimpleVal (Character _)]) = return $ fromSimple $ Bool True
isChar _ = return $ fromSimple $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean ([SimpleVal (Bool _)]) = return $ fromSimple $ Bool True
isBoolean _ = return $ fromSimple $ Bool False

checkType :: [LispVal] -> ThrowsError LispVal
checkType [x] = return $ fromSimple $ String $ typeString x
checkType badArgList = throwError $ NumArgs 1 badArgList

