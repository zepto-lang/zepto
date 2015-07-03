module Zepto.Primitives.ConversionPrimitives where
import Data.Char (ord)
import Control.Monad.Except (throwError)

import Zepto.Types

symbol2String :: [LispVal] -> ThrowsError LispVal
symbol2String [SimpleVal (Atom a)] = return $ fromSimple $ String a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbol2String [] = throwError $ NumArgs 1 []
symbol2String args@(_ : _) = throwError $ NumArgs 1 args

string2Symbol :: [LispVal] -> ThrowsError LispVal
string2Symbol [SimpleVal (String s)] = return $ fromSimple $ Atom s
string2Symbol [notString] = throwError $ TypeMismatch "string" notString
string2Symbol [] = throwError $ NumArgs 1 []
string2Symbol args@(_ : _) = throwError $ NumArgs 1 args

charToInteger :: [LispVal] -> ThrowsError LispVal
charToInteger [] = return $ fromSimple $ Bool False
charToInteger [SimpleVal (Character c)] = return $ fromSimple $ Number $ NumI $ toInteger $ ord c
charToInteger [notChar] = throwError $ TypeMismatch "character" notChar
charToInteger args@(_ : _) = throwError $ NumArgs 1 args

number2String :: [LispVal] -> ThrowsError LispVal
number2String [SimpleVal (Number x)] = return $ fromSimple $ String $ show x
number2String [notNumber] = throwError $ TypeMismatch "number" notNumber
number2String [] = throwError $ NumArgs 1 []
number2String args@(_ : _) = throwError $ NumArgs 1 args

buildNil:: [LispVal] -> ThrowsError LispVal
buildNil [] = return $ fromSimple $ Nil ""
buildNil badArgList = throwError $ NumArgs 0 badArgList

list2Simple :: [LispVal] -> ThrowsError LispVal
list2Simple [] = return $ fromSimple $ SimpleList []
list2Simple [List x] = if all simple x
                        then return $ fromSimple $ SimpleList $ map toSimple x
                        else throwError $ BadSpecialForms "expected simple elements" $
                             (filter (\e -> not $ simple e) x)
    where simple (SimpleVal _) = True
          simple _ = False
list2Simple [notList] = throwError $ TypeMismatch "list" notList
list2Simple badArgList = throwError $ NumArgs 1 badArgList
