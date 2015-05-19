module Zepto.Primitives.CharStrPrimitives where
import Data.Char
import Control.Monad.Except

import Zepto.Types
import Zepto.Primitives.ListPrimitives

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String s] = return $ List $ fmap Character s
stringToList [badType] = throwError $ TypeMismatch "string" badType
stringToList badArgList = throwError $ NumArgs 1 badArgList

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List []] = return $ String ""
listToString [List l] = buildString l
listToString [badType] = throwError $ TypeMismatch "list" badType
listToString badArgList = throwError $ NumArgs 1 badArgList

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String s] = return $ String s
stringCopy [badType] = throwError $ TypeMismatch "string" badType
stringCopy badArgList = throwError $ NumArgs 1 badArgList

charDowncase :: [LispVal] -> ThrowsError LispVal
charDowncase [Character c] = return $ Character $ toLower c
charDowncase [badType] = throwError $ TypeMismatch "character" badType
charDowncase badArgList = throwError $ NumArgs 1 badArgList

charUpcase :: [LispVal] -> ThrowsError LispVal
charUpcase [Character c] = return $ Character $ toUpper c
charUpcase [badType] = throwError $ TypeMismatch "character" badType
charUpcase badArgList = throwError $ NumArgs 1 badArgList

stringDowncase :: [LispVal] -> ThrowsError LispVal
stringDowncase [String c] = return $ String $ map toLower c
stringDowncase [badType] = throwError $ TypeMismatch "string" badType
stringDowncase badArgList = throwError $ NumArgs 1 badArgList

stringUpcase :: [LispVal] -> ThrowsError LispVal
stringUpcase [String c] = return $ String $ map toUpper c
stringUpcase [badType] = throwError $ TypeMismatch "string" badType
stringUpcase badArgList = throwError $ NumArgs 1 badArgList

