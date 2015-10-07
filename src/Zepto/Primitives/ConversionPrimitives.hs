module Zepto.Primitives.ConversionPrimitives where
import Data.Char (ord)
import Control.Monad.Except (throwError)

import Zepto.Types

symbol2String :: LispVal -> ThrowsError LispVal
symbol2String (SimpleVal (Atom a)) = return $ fromSimple $ String a
symbol2String notAtom = throwError $ TypeMismatch "symbol" notAtom

string2Symbol :: LispVal -> ThrowsError LispVal
string2Symbol (SimpleVal (String s)) = return $ fromSimple $ Atom s
string2Symbol notString = throwError $ TypeMismatch "string" notString

charToInteger :: LispVal -> ThrowsError LispVal
charToInteger (SimpleVal (Character c)) = return $ fromSimple $ Number $ NumI $ toInteger $ ord c
charToInteger notChar = throwError $ TypeMismatch "character" notChar

number2String :: LispVal -> ThrowsError LispVal
number2String (SimpleVal (Number x)) = return $ fromSimple $ String $ show x
number2String notNumber = throwError $ TypeMismatch "number" notNumber

buildNil :: ThrowsError LispVal
buildNil = return $ fromSimple $ Nil ""

buildInf :: ThrowsError LispVal
buildInf = return $ fromSimple $ Number $ NumF $ 1 / 0

list2Simple :: LispVal -> ThrowsError LispVal
list2Simple (List x) = if all simple x
                        then return $ fromSimple $ SimpleList $ map toSimple x
                        else throwError $ BadSpecialForms "expected simple elements"
                             (filter (not . simple) x)
    where simple (SimpleVal _) = True
          simple _ = False
list2Simple notList = throwError $ TypeMismatch "list" notList

simple2List :: LispVal -> ThrowsError LispVal
simple2List (SimpleVal (SimpleList x)) = return $ List $ map fromSimple x
simple2List notList = throwError $ TypeMismatch "list" notList
