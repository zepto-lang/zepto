module Types (LispNum(..),
              LispVal(..), 
              LispError(..), 
              Unpacker(AnyUnpacker), 
              ThrowsError, 
              Env,
              IOThrowsError,
              showVal, 
              showError, 
              trapError, 
              extractValue,
              nullEnv,
              liftThrows,
              runIOThrows,
              isBound,
              getVar,
              setVar,
              defineVar,
              bindVars) where
import Data.Int
import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

instance Show LispNum where show = showNum
instance Num LispNum where
    (NumI x) + (NumI y) = NumI $ x + y
    (NumF x) + (NumF y) = NumF $ x + y
    (NumI x) * (NumI y) = NumI $ x * y
    (NumF x) * (NumF y) = NumF $ x * y
    (NumI x) - (NumI y) = NumI $ x - y
    (NumF x) - (NumF y) = NumF $ x - y
    negate (NumI x) = NumI $ negate x
    negate (NumF x) = NumF $ negate x
    abs (NumI x) = NumI $ abs x
    abs (NumF x) = NumF $ abs x
    signum (NumI x) = NumI $ signum x
    signum (NumF x) = NumF $ signum x
    fromInteger x = NumI $ fromInteger x
instance Integral LispNum where
    toInteger (NumI x) = x
    {-quotRem (NumI x) (NumI y) = do z <- quotRem x y
                                   return (NumI $ (fst z), NumI $ (snd z))-}
instance Real LispNum where
    toRational (NumI x) = toRational x
    toRational (NumF x) = toRational x
instance Enum LispNum where
    {-toEnum (NumI x) = toEnum $ Int x
    toEnum (NumF x) = toEnum x
    -- fromEnum-}
data LispNum = NumI Integer
             | NumF Float
    deriving (Eq, Ord)

instance Show LispVal where show = showVal
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | String String
             | Character Char
             | Bool Bool
             | PrimitiveFunc  ([LispVal] -> ThrowsError LispVal)
             | IOFunc  ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Func {params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env}

instance Show LispError where show = showError
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

showNum :: LispNum -> String
showNum (NumF contents) = show contents
showNum (NumI contents) = show contents

showVal :: LispVal -> String
showVal (String contents) = "<String : " ++ contents ++ ">"
showVal (Atom name) = name
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = show c
showVal (Number n) = showNum n
showVal (List contents) = "(" ++ unwordsList contents ++")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ 
                                 showVal tail ++ ")"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++
                                    " args; found " ++ show (length found) 
                                    ++ "; values are " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++
                                          ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= 
                        return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar 
                              "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar
                                    "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
                    alreadyDefined <- liftIO $ isBound envRef var
                    if alreadyDefined
                        then setVar envRef var value >> return value
                        else liftIO $ do
                            valueRef <- newIORef value
                            env <- readIORef envRef
                            writeIORef envRef ((var, valueRef) : env)
                            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv
                           bindings >>= newIORef
                           where extendEnv bindings env = liftM (++ env) (mapM
                                    addBinding bindings)
                                 addBinding (var, value) = do ref <- newIORef value
                                                              return (var, ref)
