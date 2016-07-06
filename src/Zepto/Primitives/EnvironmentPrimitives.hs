module Zepto.Primitives.EnvironmentPrimitives where

import Control.Monad.Except (liftIO, throwError)

import Zepto.Types
import Zepto.Variables (isBound)

makeNullEnv :: [LispVal] -> IOThrowsError LispVal
makeNullEnv [] = do
    env <- liftIO nullEnv
    return $ Environ env
makeNullEnv args = throwError $ NumArgs 0 args

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
