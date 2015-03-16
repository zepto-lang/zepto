module Types (LispNum(..),
              LispVal(..), 
              LispFun(..), 
              LispError(..), 
              Unpacker(AnyUnpacker), 
              ThrowsError, 
              Env(..),
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
import Data.Array
import Data.Complex
import Data.IORef
import qualified Data.Map
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec.Error

-- | an unpacker for any LispVal
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

instance Show LispNum where show = showNum
instance Eq LispNum where
    (NumI x) == (NumI y) = x == y
    (NumF x) == (NumI y) = x == fromIntegral y
    (NumI x) == (NumF y) = fromIntegral x == y
    (NumF x) == (NumF y) = x == y
    (NumC x) == (NumC y) = x == y
    (NumC x) == (NumF y) = x == (mkPolar y 0)
    (NumF x) == (NumC y) = (mkPolar x 0) == y
    (NumC x) == (NumI y) = x == (mkPolar (fromIntegral y) 0)
    (NumI x) == (NumC y) = (mkPolar (fromIntegral x) 0) == y
instance Ord LispNum where
    compare (NumI x) (NumI y) = compare x y
    compare (NumF x) (NumI y) = compare x (fromIntegral y)
    compare (NumI x) (NumF y) = compare (fromIntegral x) y
    compare (NumF x) (NumF y) = compare x y
    compare (NumC x) (NumC y) = do
        let fx = compare (realPart x) (realPart y)
        if fx == EQ
            then compare (imagPart x) (imagPart y)
            else fx
    compare (NumC x) (NumF y) = do
        let fx = compare (realPart x) y
        if fx == EQ
            then compare (imagPart x) 0
            else fx
    compare (NumF x) (NumC y) = do
        let fx = compare (realPart y) x
        if fx == EQ
            then compare (imagPart y) 0
            else fx
    compare (NumC x) (NumI y) = do
        let fx = compare (realPart x) (fromIntegral y)
        if fx == EQ
            then compare (imagPart x) 0
            else fx
    compare (NumI x) (NumC y) = do
        let fx = compare (realPart y) (fromIntegral x)
        if fx == EQ
            then compare (imagPart y) 0
            else fx
instance Num LispNum where
    (NumI x) + (NumI y) = NumI $ x + y
    (NumF x) + (NumI y) = NumF $ x + fromIntegral y
    (NumI x) + (NumF y) = NumF $ fromIntegral x + y
    (NumF x) + (NumF y) = NumF $ x + y
    (NumC x) + (NumC y) = NumC $ x + y
    (NumC x) + (NumF y) = NumC $ x + (mkPolar y 0)
    (NumF x) + (NumC y) = NumC $ (mkPolar x 0) + y
    (NumC x) + (NumI y) = NumC $ x + (mkPolar (fromIntegral y) 0)
    (NumI x) + (NumC y) = NumC $ (mkPolar (fromIntegral x) 0) + y
    (NumI x) * (NumI y) = NumI $ x * y
    (NumF x) * (NumI y) = NumF $ x * fromIntegral y
    (NumI x) * (NumF y) = NumF $ fromIntegral x * y
    (NumF x) * (NumF y) = NumF $ x * y
    (NumC x) * (NumC y) = NumC $ x * y
    (NumC x) * (NumF y) = NumC $ x * (mkPolar y 0)
    (NumF x) * (NumC y) = NumC $ (mkPolar x 0) * y
    (NumC x) * (NumI y) = NumC $ x * (mkPolar (fromIntegral y) 0)
    (NumI x) * (NumC y) = NumC $ (mkPolar (fromIntegral x) 0) * y
    (NumI x) - (NumI y) = NumI $ x - y
    (NumF x) - (NumI y) = NumF $ x - fromIntegral y
    (NumI x) - (NumF y) = NumF $ fromIntegral x - y
    (NumF x) - (NumF y) = NumF $ x - y
    (NumC x) - (NumC y) = NumC $ x - y
    (NumC x) - (NumF y) = NumC $ x - (mkPolar y 0)
    (NumF x) - (NumC y) = NumC $ (mkPolar x 0) - y
    (NumC x) - (NumI y) = NumC $ x - (mkPolar (fromIntegral y) 0)
    (NumI x) - (NumC y) = NumC $ (mkPolar (fromIntegral x) 0) - y
    negate (NumI x) = NumI $ negate x
    negate (NumF x) = NumF $ negate x
    negate (NumC x) = NumC $ negate x
    abs (NumI x) = NumI $ abs x
    abs (NumF x) = NumF $ abs x
    abs (NumC x) = NumC $ abs x
    signum (NumI x) = NumI $ signum x
    signum (NumF x) = NumF $ signum x
    signum (NumC x) = NumC $ signum x
    fromInteger x = NumI $ fromInteger x
instance Integral LispNum where
    toInteger (NumI x) = x
    toInteger (NumF x) = round x
    toInteger (NumC x) = round $ realPart x
    quotRem (NumI x) (NumI y) = (NumI $ quot x y, NumI $ rem x y)
    quotRem (NumF x) (NumI y) = (NumF $ x / fromIntegral y, NumF $ mod' x (fromIntegral y))
    quotRem (NumI x) (NumF y) = (NumF $ fromIntegral x / y, NumF $ mod' (fromIntegral x) y)
    quotRem (NumF x) (NumF y) = (NumF $ x / y, NumF $ mod' x y)
    quotRem (NumC _) _ = (0, 0)
    quotRem _ (NumC _) = (0, 0)
instance Real LispNum where
    toRational (NumI x) = toRational x
    toRational (NumF x) = toRational x
    toRational (NumC _) = 0
instance Enum LispNum where
    toEnum x = NumI $ toInteger x
    fromEnum (NumI x) = fromIntegral x
    fromEnum (NumF x) = round x
    fromEnum (NumC _) = 0
-- | a LispNum data type comprising a float and an integer
data LispNum = NumI Integer
             | NumF Double
             | NumC (Complex Double)

instance Show LispVal where show = showVal
-- | a LispVal data type comprising all Lisp data types
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Number LispNum
             | String String
             | Character Char
             | Bool Bool
             | PrimitiveFunc  ([LispVal] -> ThrowsError LispVal)
             | IOFunc  ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Func LispFun
             | Nil String
             | Pointer { pointerVar :: String, pointerEnv :: Env } 
             
             
-- | a LispFun data type 
data LispFun = LispFun { params :: [String], vararg :: Maybe String,
                         body :: [LispVal], closure :: Env, docstring :: String}

instance Show LispError where show = showError
-- | a LispError data type comprising all errors that
-- | can be emitted from within the interpreter
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | ParseErr ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | InternalError String
               | Default String

instance Eq Env where
    (Environment _ xb xp) == (Environment _ yb yp) = (xb == yb) && (xp == yp)
-- | an Environment type where all variables are stored
data Env = Environment {
        parentEnv :: (Maybe Env)
      , bindings :: (IORef (Data.Map.Map String (IORef LispVal)))
      , pointers :: (IORef (Data.Map.Map String (IORef [LispVal])))
}

-- | a ThrowsError type containing either an error or a value
type ThrowsError = Either LispError
-- | a IOThrowsError type containing either an error or an IO
type IOThrowsError = ExceptT LispError IO

showNum :: LispNum -> String
showNum (NumF contents) = show contents
showNum (NumI contents) = show contents
showNum (NumC contents) = show (realPart contents) ++ "+" ++ show (imagPart contents) ++ "i"

-- | a show function for all LispVals
showVal :: LispVal -> String
showVal (String contents) = contents
showVal (Atom name) = name
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = show c
showVal (Number n) = showNum n
showVal (List contents) = "(" ++ unwordsList contents ++")"
showVal (Vector contents) = "#(" ++ unwordsList (elems contents) ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"
showVal (Func LispFun {params = args, vararg = varargs, body = _, closure = _,
                       docstring = doc}) = 
    doc ++ "; source: " ++
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Pointer p _) = "<pointer " ++ p ++ ">"
showVal (Nil _) = ""

-- | a show function for all LispErrors
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
showError (InternalError err) = "Internal error: " ++ err
showError (Default err) = err

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | traps an error and shows it
trapError :: (MonadError e m, Show e) =>  m String -> m String
trapError action = catchError action (return . show)

-- | extracts a value from a possible error
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error("This should not be happening. " ++
                              "Please consider reporting this incident.")

-- | returns a new empty environment
nullEnv :: IO Env
nullEnv = do
    nullb <- newIORef $ Data.Map.fromList []
    nullp <- newIORef $ Data.Map.fromList []
    return $ Environment Nothing nullb nullp

-- | lift a ThrowsError to an IOThrowsError
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- | lift an IOThrowsError to an IO monad
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runExceptT (trapError action))
