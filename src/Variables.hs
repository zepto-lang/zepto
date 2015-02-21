module Variables where
import Types
import Control.Monad
import Control.Monad.Except
import Data.IORef
import Data.Maybe

-- | the module namespace
mnamespace :: String
mnamespace = "m"

-- | the variable namespace
vnamespace :: String
vnamespace = "v"

-- | checks whether a variable is bound
isBound :: Env -> String -> IO Bool
isBound envRef = isNamespacedBound envRef vnamespace

-- | checks whether a variable is bound to a namespace
isNamespacedBound :: Env -> String -> String -> IO Bool
isNamespacedBound envRef namespace var = liftM (isJust . 
                                         lookup (namespace, var))
                                         (readIORef envRef)

-- | gets a variable from an environment
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef = getNamespacedVar envRef vnamespace

-- | gets a variable from an environment in a specific namespace
getNamespacedVar :: Env -> String -> String -> IOThrowsError LispVal
getNamespacedVar envRef namespace var = do 
        env <- liftIO $ readIORef envRef
        maybe (throwError $ UnboundVar "Getting an unbound variable" var)
              (liftIO . readIORef)
              (lookup (namespace, var) env)

setVar, defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
-- | sets a variable to an environment
setVar envRef = setNamespacedVar envRef vnamespace 
-- | defines a variable to an environment
defineVar envRef = defineNamespacedVar envRef vnamespace

-- | sets a variable to an environment in a specific namespace
setNamespacedVar :: Env -> String -> String -> LispVal -> IOThrowsError LispVal
setNamespacedVar envRef namespace var value = do 
        env <- liftIO $ readIORef envRef
        maybe (throwError $ UnboundVar "Setting an unbound variable: " var)
              (liftIO . (`writeIORef` value))
              (lookup (namespace, var) env)
        return value

-- | defines a variable to an environment in a specific namespace
defineNamespacedVar :: Env -> String -> String -> LispVal -> IOThrowsError LispVal
defineNamespacedVar envRef namespace var value = do
        alreadyDefined <- liftIO $ isNamespacedBound envRef namespace var
        if alreadyDefined
            then setNamespacedVar envRef namespace var value >> return value
            else liftIO $ do
                valueRef <- newIORef value
                env <- readIORef envRef
                writeIORef envRef (((namespace, var), valueRef) : env)
                return value

-- | binds multiple variables at once
bindVars :: Env -> [((String, String), LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv binds env = liftM (++ env) (mapM addBinding binds)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)
