module Zepto.Primitives.TypeCheckPrimitives where
import Control.Monad.Except (liftIO)
import Data.Complex (imagPart)
import System.IO (hIsReadable, hIsWritable)

import Zepto.Types

typecheckDoc :: String -> String
typecheckDoc t = "checks whether <par>arg</par> is a " ++ t ++ ".\n\
\n\
  params:\n\
    - arg: the object to check\n\
  complexity: O(1)\n\
  returns: a boolean"

isError :: LispVal -> ThrowsError LispVal
isError (Error _) = return $ fromSimple $ Bool True
isError _ = return $ fromSimple $ Bool False

isNumber :: LispVal -> ThrowsError LispVal
isNumber (SimpleVal (Number _)) = return $ fromSimple $ Bool True
isNumber _ = return $ fromSimple $ Bool False

isReal :: LispVal -> ThrowsError LispVal
isReal (SimpleVal (Number (NumC x))) = return $ fromSimple $ Bool $ imagPart x == 0
isReal (SimpleVal (Number _)) = return $ fromSimple $ Bool True
isReal _ = return $ fromSimple $ Bool False

isInteger :: LispVal -> ThrowsError LispVal
isInteger (SimpleVal (Number (NumI _))) = return $ fromSimple $ Bool True
isInteger (SimpleVal (Number (NumS _))) = return $ fromSimple $ Bool True
isInteger _ = return $ fromSimple $ Bool False

isSmall :: LispVal -> ThrowsError LispVal
isSmall (SimpleVal (Number (NumS _))) = return $ fromSimple $ Bool True
isSmall _ = return $ fromSimple $ Bool False

isFloat :: LispVal -> ThrowsError LispVal
isFloat (SimpleVal (Number (NumF _))) = return $ fromSimple $ Bool True
isFloat _ = return $ fromSimple $ Bool False

isRational :: LispVal -> ThrowsError LispVal
isRational (SimpleVal (Number (NumR _))) = return $ fromSimple $ Bool True
isRational (SimpleVal (Number (NumS _))) = return $ fromSimple $ Bool True
isRational (SimpleVal (Number (NumI _))) = return $ fromSimple $ Bool True
isRational (SimpleVal (Number (NumF x))) =
        if x == fromInteger (round x)
            then return $ fromSimple $ Bool True
            else return $ fromSimple $ Bool False
isRational _ = return $ fromSimple $ Bool False

isDottedList :: LispVal -> ThrowsError LispVal
isDottedList (DottedList _ _) = return $ fromSimple $ Bool True
isDottedList _ = return $ fromSimple $ Bool False

isProcedure :: LispVal -> ThrowsError LispVal
isProcedure (PrimitiveFunc _ _) = return $ fromSimple $ Bool True
isProcedure (EvalFunc _ _) = return $ fromSimple $ Bool True
isProcedure (Func _ _) = return $ fromSimple $ Bool True
isProcedure (IOFunc _ _) = return $ fromSimple $ Bool True
isProcedure (Cont _) = return $ fromSimple $ Bool True
isProcedure _ = return $ fromSimple $ Bool False

isPrim :: LispVal -> ThrowsError LispVal
isPrim (PrimitiveFunc _ _) = return $ fromSimple $ Bool True
isPrim (EvalFunc _ _) = return $ fromSimple $ Bool True
isPrim (IOFunc _ _) = return $ fromSimple $ Bool True
isPrim _ = return $ fromSimple $ Bool False

isFun :: LispVal -> ThrowsError LispVal
isFun (Func _ _) = return $ fromSimple $ Bool True
isFun _ = return $ fromSimple $ Bool False

isEnv :: LispVal -> ThrowsError LispVal
isEnv (Environ _) = return $ fromSimple $ Bool True
isEnv _ = return $ fromSimple $ Bool False

isSimple :: LispVal -> ThrowsError LispVal
isSimple (SimpleVal _) = return $ fromSimple $ Bool True
isSimple _ = return $ fromSimple $ Bool False

isVector, isList, isByteVector, isSimpleList :: LispVal -> ThrowsError LispVal
isVector (Vector _) = return $ fromSimple $ Bool True
isVector _ = return $ fromSimple $ Bool False
isList (List _) = return $ fromSimple $ Bool True
isList _ = return $ fromSimple $ Bool False
isByteVector (ByteVector _) = return $ fromSimple $ Bool True
isByteVector _ = return $ fromSimple $ Bool False
isSimpleList (SimpleVal (SimpleList _)) = return $ fromSimple $ Bool True
isSimpleList _ = return $ fromSimple $ Bool False

isNull :: LispVal -> ThrowsError LispVal
isNull (List []) = return $ fromSimple $ Bool True
isNull (SimpleVal (Nil _)) = return $ fromSimple $ Bool True
isNull _ = return $ fromSimple $ Bool False

isNil :: LispVal ->ThrowsError LispVal
isNil (SimpleVal (Nil _)) = return $ fromSimple $ Bool True
isNil _ = return $ fromSimple $ Bool False

isSymbol :: LispVal -> ThrowsError LispVal
isSymbol (SimpleVal (Atom (':' : _))) = return $ fromSimple $ Bool False
isSymbol (SimpleVal (Atom _)) = return $ fromSimple $ Bool True
isSymbol _ = return $ fromSimple $ Bool False

isAtom :: LispVal -> ThrowsError LispVal
isAtom (SimpleVal (Atom (':' : _))) = return $ fromSimple $ Bool True
isAtom _ = return $ fromSimple $ Bool False

isRegex :: LispVal -> ThrowsError LispVal
isRegex (SimpleVal (Regex _)) = return $ fromSimple $ Bool True
isRegex _ = return $ fromSimple $ Bool False

isOpaque :: LispVal -> ThrowsError LispVal
isOpaque (Opaque _) = return $ fromSimple $ Bool True
isOpaque _ = return $ fromSimple $ Bool False

isString :: LispVal -> ThrowsError LispVal
isString (SimpleVal (String _)) = return $ fromSimple $ Bool True
isString _ = return $ fromSimple $ Bool False

isPort :: LispVal -> ThrowsError LispVal
isPort (Port _) = return $ fromSimple $ Bool True
isPort _ = return $ fromSimple $ Bool False

isInputPort :: LispVal -> IOThrowsError LispVal
isInputPort (Port p) = do
    x <- liftIO $ hIsReadable p
    return $ fromSimple $ Bool x
isInputPort _ = return $ fromSimple $ Bool False

isOutputPort :: LispVal -> IOThrowsError LispVal
isOutputPort (Port p) = do
    x <- liftIO $ hIsWritable p
    return $ fromSimple $ Bool x
isOutputPort _ = return $ fromSimple $ Bool False

isChar :: LispVal -> ThrowsError LispVal
isChar (SimpleVal (Character _)) = return $ fromSimple $ Bool True
isChar _ = return $ fromSimple $ Bool False

isBoolean :: LispVal -> ThrowsError LispVal
isBoolean (SimpleVal (Bool _)) = return $ fromSimple $ Bool True
isBoolean _ = return $ fromSimple $ Bool False

checkType :: LispVal -> ThrowsError LispVal
checkType x = return $ fromSimple $ String $ typeString x

isHash :: LispVal -> ThrowsError LispVal
isHash (HashMap _) = return $ fromSimple $ Bool True
isHash _ = return $ fromSimple $ Bool False
