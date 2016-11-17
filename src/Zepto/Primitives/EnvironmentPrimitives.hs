module Zepto.Primitives.EnvironmentPrimitives where

import Control.Monad.Except (liftIO, throwError)

import Zepto.Types
import Zepto.Variables (isBound)

makeNullEnvDoc :: String
makeNullEnvDoc = "make an empty environment.\n\
\n\
  complexity: O(1)\n\
  returns: an empty environment"

makeNullEnv :: [LispVal] -> IOThrowsError LispVal
makeNullEnv [] = do
    env <- liftIO nullEnv
    return $ Environ env
makeNullEnv args = throwError $ NumArgs 0 args

inEnvDoc :: String
inEnvDoc = "checks whether a variable is in an environment.\n\
\n\
  params:\n\
    - env: optional variable representing the environment. If it is not set, the current environment will be checked.\n\
    - name: the name to check. Can be astring or a symbol.\n\
  complexity: O(n)\n\
  returns: a boolean"

inEnv :: [LispVal] -> IOThrowsError LispVal
inEnv [Cont (Continuation env _ _ _ _ _), (SimpleVal (Atom name))] = do
  res <- liftIO $ isBound env name
  return $ fromSimple $ Bool res
inEnv [Cont (Continuation env _ _ _ _ _), (SimpleVal (String name))] = do
  res <- liftIO $ isBound env name
  return $ fromSimple $ Bool res
inEnv [_, Environ env, (SimpleVal (Atom name))] = do
  res <- liftIO $ isBound env name
  return $ fromSimple $ Bool res
inEnv [_, Environ env, (SimpleVal (String name))] = do
  res <- liftIO $ isBound env name
  return $ fromSimple $ Bool res
inEnv [_, x] = throwError $ TypeMismatch "symbol" x
inEnv x = throwError $ NumArgs 1 (tail x)
