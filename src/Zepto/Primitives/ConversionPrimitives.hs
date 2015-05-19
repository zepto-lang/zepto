module Zepto.Primitives.ConversionPrimitives where
import Data.Char (ord)
import Control.Monad.Except (throwError)

import Zepto.Types

symbol2String :: [LispVal] -> ThrowsError LispVal
symbol2String ([Atom a]) = return $ String a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbol2String [] = return $ Bool False
symbol2String _ = return $ Bool False

string2Symbol :: [LispVal] -> ThrowsError LispVal
string2Symbol [] = return $ Bool False
string2Symbol ([String s]) = return $ Atom s
string2Symbol [notString] = throwError $ TypeMismatch "string" notString
string2Symbol args@(_ : _) = throwError $ NumArgs 1 args

charToInteger :: [LispVal] -> ThrowsError LispVal
charToInteger [] = return $ Bool False
charToInteger ([Character c]) = return $ Number $ NumI $ toInteger $ ord c
charToInteger [notChar] = throwError $ TypeMismatch "character" notChar
charToInteger args@(_ : _) = throwError $ NumArgs 1 args

