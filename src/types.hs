module Types (LispNum(..),
              LispVal(..), 
              LispPointer(..), 
              LispFun(..), 
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
              runIOThrows) where
import Data.Fixed
import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

instance Show LispNum where show = showNum
instance Eq LispNum where
    (NumI x) == (NumI y) = x == y
    (NumF x) == (NumI y) = x == fromIntegral y
    (NumI x) == (NumF y) = fromIntegral x == y
    (NumF x) == (NumF y) = x == y
instance Ord LispNum where
    compare (NumI x) (NumI y) = compare x y
    compare (NumF x) (NumI y) = compare x (fromIntegral y)
    compare (NumI x) (NumF y) = compare (fromIntegral x) y
    compare (NumF x) (NumF y) = compare x y
instance Num LispNum where
    (NumI x) + (NumI y) = NumI $ x + y
    (NumF x) + (NumI y) = NumF $ x + fromIntegral y
    (NumI x) + (NumF y) = NumF $ fromIntegral x + y
    (NumF x) + (NumF y) = NumF $ x + y
    (NumI x) * (NumI y) = NumI $ x * y
    (NumF x) * (NumI y) = NumF $ x * fromIntegral y
    (NumI x) * (NumF y) = NumF $ fromIntegral x * y
    (NumF x) * (NumF y) = NumF $ x * y
    (NumI x) - (NumI y) = NumI $ x - y
    (NumF x) - (NumI y) = NumF $ x - fromIntegral y
    (NumI x) - (NumF y) = NumF $ fromIntegral x - y
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
    toInteger (NumF x) = round x
    quotRem (NumI x) (NumI y) = (NumI $ quot x y, NumI $ rem x y)
    quotRem (NumF x) (NumI y) = (NumF $ x / fromIntegral y, NumF $ mod' x (fromIntegral y))
    quotRem (NumI x) (NumF y) = (NumF $ fromIntegral x / y, NumF $ mod' (fromIntegral x) y)
    quotRem (NumF x) (NumF y) = (NumF $ x / y, NumF $ mod' x y)
instance Real LispNum where
    toRational (NumI x) = toRational x
    toRational (NumF x) = toRational x
instance Enum LispNum where
    toEnum x = NumI $ toInteger x
    fromEnum (NumI x) = fromIntegral x
    fromEnum (NumF x) = round x
data LispNum = NumI Integer
             | NumF Double

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
             | Func LispFun
             | Nil String
             | Pointer LispPointer
             
             
data LispPointer = LispPointer { pointerVar :: String, pointerEnv :: Env } 
             
             
data LispFun = LispFun { params :: [String], vararg :: Maybe String,
                         body :: [LispVal], closure :: Env}

instance Show LispError where show = showError
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | ParseErr ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | InternalError String
               | Default String

type Env = IORef [((String, String), IORef LispVal)]
type ThrowsError = Either LispError
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
showVal (Func LispFun {params = args, vararg = varargs, body = _, closure = _}) = 
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Pointer (LispPointer p _)) = "<pointer " ++ p ++ ">"
showVal (Nil _) = ""

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++
                                    " args; found " ++ show (length found) 
                                    ++ "; values are " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++
                                          ", found " ++ show found
showError (ParseErr parseErr) = "Parse error at " ++ show parseErr
showError _ = "Unknown error"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

trapError :: (MonadError e m, Show e) =>  m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error("This should not be happening. " ++
                              "Please consider reporting this incident.")

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runExceptT (trapError action))
