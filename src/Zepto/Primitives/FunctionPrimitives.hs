module Zepto.Primitives.FunctionPrimitives where
import Control.Monad.Except (throwError)

import Zepto.Types

functionNameDoc :: String
functionNameDoc = "get the function name as a string (does not work for\n\
macros).\n\
\n\
  params:\n\
    - f: the function\n\
  complexity: O(1)\n\
  returns: a string representing the function name"

functionName :: LispVal -> ThrowsError LispVal
functionName (PrimitiveFunc n _) = return $ fromSimple $ String n
functionName (IOFunc n _) = return $ fromSimple $ String n
functionName (Func n _) = return $ fromSimple $ String n
functionName (EvalFunc n _) = return $ fromSimple $ String n
functionName x = throwError $ TypeMismatch "function" x

functionArgsDoc :: String
functionArgsDoc = "get the function arguments as a list of strings\n\
(does not work for primitives or macros).\n\
\n\
  params:\n\
    - f: the function\n\
  complexity: O(1)\n\
  returns: a list of strings representing the parameters"

functionArgs :: LispVal -> ThrowsError LispVal
functionArgs (Func _ (LispFun {params=p, vararg=v})) =
  return $ List $ map (\x -> fromSimple $ String x) $
    case v of
      Just arg -> p ++ [arg]
      Nothing  -> p
functionArgs x = throwError $ TypeMismatch "function" x

functionBodyDoc :: String
functionBodyDoc = "get the function body as a list (does not work for\n\
primitives or macros).\n\
\n\
  params:\n\
    - f: the function\n\
  complexity: O(1)\n\
  returns: a list representing the function body"

functionBody :: LispVal -> ThrowsError LispVal
functionBody (Func _ (LispFun {body=b})) = return $ List $ b
functionBody x = throwError $ TypeMismatch "function" x

functionDocsDoc :: String
functionDocsDoc = "get the function docstring as a string (does not work for\n\
primitives).\n\
\n\
  params:\n\
    - f: the function\n\
  complexity: O(1)\n\
  returns: the docstring as string"

functionDocs :: LispVal -> ThrowsError LispVal
functionDocs (Func _ (LispFun {docstring=d})) = return $ fromSimple $ String $ d
functionDocs (List ((SimpleVal (Atom "syntax-rules")) : (SimpleVal (String doc)) : _)) =
  return $ fromSimple $ String doc
functionDocs (List ((SimpleVal (Atom "syntax-rules")) : _)) =
  return $ fromSimple $ String "No documentation"
functionDocs x = throwError $ TypeMismatch "function" x
