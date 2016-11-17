module Zepto.Primitives.ErrorPrimitives where

import Control.Monad.Except (throwError)

import Zepto.Types

errorTextDoc :: String
errorTextDoc = "get the body of the error.\n\
\n\
  params:\n\
    - err: the error\n\
  complexity: O(1)\n\
  returns: the error message as string"

errorText :: LispVal -> ThrowsError LispVal
errorText (Error e) = return $ fromSimple $ String $ show e
errorText x = throwError $ TypeMismatch "error" x

makeErrorDoc :: String
makeErrorDoc = "create an error from a string.\n\
\n\
  params:\n\
    - str: the error string\n\
  complexity: O(1)\n\
  returns: the error"

makeError :: LispVal -> ThrowsError LispVal
makeError (SimpleVal (String s)) = return $ Error $ Default s
makeError x = throwError $ TypeMismatch "string" x

throwZErrorDoc :: String
throwZErrorDoc = "throw an error.\n\
\n\
  params:\n\
    - err: the error to throw (if this is not an error, another type of error will be thrown!)\n\
  complexity: O(1)\n\
  returns: a thrown error"

throwZError :: LispVal -> ThrowsError LispVal
throwZError (Error e) = throwError e
throwZError x = throwError $ TypeMismatch "error" x
