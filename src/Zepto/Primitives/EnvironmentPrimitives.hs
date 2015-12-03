module Zepto.Primitives.EnvironmentPrimitives where

import Control.Monad.Except (liftIO, throwError)

import Zepto.Types

makeNullEnv :: [LispVal] -> IOThrowsError LispVal
makeNullEnv [] = do
    env <- liftIO nullEnv
    return $ Environ env
makeNullEnv args = throwError $ NumArgs 0 args
