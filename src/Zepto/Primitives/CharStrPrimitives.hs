module Zepto.Primitives.CharStrPrimitives where
import Data.Char
import Data.List.Utils
import Control.Monad.Except

import Zepto.Types
import Zepto.Primitives.ListPrimitives

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [SimpleVal (String s)] = return $ List $ fmap (fromSimple . Character) s
stringToList [badType] = throwError $ TypeMismatch "string" badType
stringToList badArgList = throwError $ NumArgs 1 badArgList

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List []] = return $ fromSimple $ String ""
listToString [List l] = buildString l
listToString [badType] = throwError $ TypeMismatch "list" badType
listToString badArgList = throwError $ NumArgs 1 badArgList

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [SimpleVal (String s)] = return $ fromSimple $ String s
stringCopy [badType] = throwError $ TypeMismatch "string" badType
stringCopy badArgList = throwError $ NumArgs 1 badArgList

charDowncase :: [LispVal] -> ThrowsError LispVal
charDowncase [SimpleVal (Character c)] = return $ fromSimple $ Character $ toLower c
charDowncase [badType] = throwError $ TypeMismatch "character" badType
charDowncase badArgList = throwError $ NumArgs 1 badArgList

charUpcase :: [LispVal] -> ThrowsError LispVal
charUpcase [SimpleVal (Character c)] = return $ fromSimple $ Character $ toUpper c
charUpcase [badType] = throwError $ TypeMismatch "character" badType
charUpcase badArgList = throwError $ NumArgs 1 badArgList

stringDowncase :: [LispVal] -> ThrowsError LispVal
stringDowncase [SimpleVal (String c)] = return $ fromSimple $ String $ map toLower c
stringDowncase [badType] = throwError $ TypeMismatch "string" badType
stringDowncase badArgList = throwError $ NumArgs 1 badArgList

stringUpcase :: [LispVal] -> ThrowsError LispVal
stringUpcase [SimpleVal (String c)] = return $ fromSimple $ String $ map toUpper c
stringUpcase [badType] = throwError $ TypeMismatch "string" badType
stringUpcase badArgList = throwError $ NumArgs 1 badArgList

stringSub :: [LispVal] -> ThrowsError LispVal
stringSub [SimpleVal (String source), SimpleVal (String pattern), SimpleVal (String substitute)] =
  return $ fromSimple $ String $ replace pattern substitute source
stringSub [err@(_), SimpleVal (String _), SimpleVal (String _)] = throwError $ TypeMismatch "string" err
stringSub [SimpleVal (String _), err@(_), SimpleVal (String _)] = throwError $ TypeMismatch "string" err
stringSub [SimpleVal (String _), SimpleVal (String _), err@(_)] = throwError $ TypeMismatch "string" err
stringSub badArgList = throwError $ NumArgs 3 badArgList
