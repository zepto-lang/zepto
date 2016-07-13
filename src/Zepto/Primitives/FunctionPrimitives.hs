module Zepto.Primitives.FunctionPrimitives where
import Control.Monad.Except (throwError)

import Zepto.Types


functionName :: LispVal -> ThrowsError LispVal
functionName (PrimitiveFunc n _) = return $ fromSimple $ String n
functionName (IOFunc n _) = return $ fromSimple $ String n
functionName (Func n _) = return $ fromSimple $ String n
functionName (EvalFunc n _) = return $ fromSimple $ String n
functionName x = throwError $ TypeMismatch "function" x

functionArgs :: LispVal -> ThrowsError LispVal
functionArgs (Func _ (LispFun {params=p, vararg=v})) =
  return $ List $ map (\x -> fromSimple $ String x) $
    case v of
      Just arg -> p ++ [arg]
      Nothing  -> p
functionArgs x = throwError $ TypeMismatch "function" x

functionBody :: LispVal -> ThrowsError LispVal
functionBody (Func _ (LispFun {body=b})) = return $ List $ b
functionBody x = throwError $ TypeMismatch "function" x

functionDocs :: LispVal -> ThrowsError LispVal
functionDocs (Func _ (LispFun {docstring=d})) = return $ fromSimple $ String $ d
functionDocs x = throwError $ TypeMismatch "function" x
