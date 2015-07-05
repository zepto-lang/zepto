module Zepto.Primitives.HashPrimitives where

import Control.Monad.Except
import Data.Map hiding (map)

import Zepto.Types

hashKeys :: [LispVal] -> ThrowsError LispVal
hashKeys [HashMap x] = return $ List $ map SimpleVal (keys x)
hashKeys [x] = throwError $ TypeMismatch "hashmap" x
hashKeys badArgList = throwError $ NumArgs 1 badArgList

hashVals :: [LispVal] -> ThrowsError LispVal
hashVals [HashMap x] = return $ List $ map ((!) x) (keys x)
hashVals [x] = throwError $ TypeMismatch "hashmap" x
hashVals badArgList = throwError $ NumArgs 1 badArgList

inHash :: [LispVal] -> ThrowsError LispVal
inHash [HashMap x, SimpleVal e] = return $ fromSimple $ Bool $ member e x
inHash [x, SimpleVal _] = throwError $ TypeMismatch "hashmap" x
inHash [HashMap _, e] = throwError $ TypeMismatch "simple value" e
inHash [x, _] = throwError $ TypeMismatch "hashmap and simple value" x
inHash badArgList = throwError $ NumArgs 2 badArgList

makeHash :: [LispVal] -> ThrowsError LispVal
makeHash [List x] = makeHash x
makeHash l = case keyVal l [] of
              Right x -> return $ HashMap $ fromList x
              Left x  -> throwError $ TypeMismatch "simple value/hashmap/list" x
    where keyVal [] acc = Right acc
          keyVal ((List [x, y]) : r) acc = keyVal (x : y : r) acc
          keyVal ((SimpleVal x) : y : r) acc = keyVal r $ (x, y) : acc
          keyVal ((HashMap x) : r) acc = keyVal r $ acc ++ toList x
          keyVal (x : _)  _ = Left x
