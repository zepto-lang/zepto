module Variables where
import Types
import Control.Monad
import Control.Monad.Except
import Data.IORef

vnamespace :: String
vnamespace = "v"

isBound :: Env -> String -> IO Bool
isBound envRef = isNamespacedBound envRef vnamespace

isNamespacedBound :: Env -> String -> String -> IO Bool
isNamespacedBound envRef namespace var = liftM (maybe False (const True) . 
                                         lookup (namespace, var))
                                         (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef = getNamespacedVar envRef vnamespace

getNamespacedVar :: Env -> String -> String -> IOThrowsError LispVal
getNamespacedVar envRef namespace var = do 
        env <- liftIO $ readIORef envRef
        maybe (throwError $ UnboundVar "Getting an unbound variable" var)
              (liftIO . readIORef)
              (lookup (namespace, var) env)

setVar, defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef = setNamespacedVar envRef vnamespace 
defineVar envRef = defineNamespacedVar envRef vnamespace

setNamespacedVar :: Env -> String -> String -> LispVal -> IOThrowsError LispVal
setNamespacedVar envRef namespace var value = do 
        env <- liftIO $ readIORef envRef
        maybe (throwError $ UnboundVar "Setting an unbound variable: " var)
              (liftIO . (`writeIORef` value))
              (lookup (namespace, var) env)
        return value

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

bindVars :: Env -> [((String, String), LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv binds env = liftM (++ env) (mapM addBinding binds)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)
