module Zepto.Primitives.ConversionPrimitives where
import Data.Char (ord)
import Control.Monad.Except (throwError)

import Zepto.Types

symbol2String :: [LispVal] -> ThrowsError LispVal
symbol2String ([Atom a]) = return $ String a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbol2String [] = throwError $ NumArgs 1 []
symbol2String args@(_ : _) = throwError $ NumArgs 1 args

string2Symbol :: [LispVal] -> ThrowsError LispVal
string2Symbol ([String s]) = return $ Atom s
string2Symbol [notString] = throwError $ TypeMismatch "string" notString
string2Symbol [] = throwError $ NumArgs 1 []
string2Symbol args@(_ : _) = throwError $ NumArgs 1 args

charToInteger :: [LispVal] -> ThrowsError LispVal
charToInteger [] = return $ Bool False
charToInteger ([Character c]) = return $ Number $ NumI $ toInteger $ ord c
charToInteger [notChar] = throwError $ TypeMismatch "character" notChar
charToInteger args@(_ : _) = throwError $ NumArgs 1 args

number2String :: [LispVal] -> ThrowsError LispVal
number2String ([Number x]) = return $ String $ show x
number2String [notNumber] = throwError $ TypeMismatch "number" notNumber
number2String [] = throwError $ NumArgs 1 []
number2String args@(_ : _) = throwError $ NumArgs 1 args
