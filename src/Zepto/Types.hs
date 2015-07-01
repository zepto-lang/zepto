module Zepto.Types (LispNum(..),
                    Simple(..),
                    LispVal(..),
                    Continuation(..),
                    LispFun(..),
                    LispError(..),
                    Unpacker(AnyUnpacker),
                    ThrowsError,
                    Env(..),
                    IOThrowsError,
                    showVal,
                    showError,
                    fromSimple,
                    toSimple,
                    trapError,
                    extractValue,
                    typeString,
                    nullEnv,
                    nullCont,
                    liftThrows,
                    runIOThrows
                    ) where
import Data.Array
import Data.Complex
import Data.Fixed
import Data.Ratio
import Data.IORef
import Control.Monad
import Control.Monad.Except
import System.IO
import Text.ParserCombinators.Parsec.Error
import qualified Data.Map

-- | an unpacker for any LispVal
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

instance Show LispNum where show = showNum
instance Eq LispNum where
    (NumI x) == (NumI y) = x == y
    (NumF x) == (NumI y) = x == fromIntegral y
    (NumI x) == (NumF y) = fromIntegral x == y
    (NumF x) == (NumF y) = x == y
    (NumC x) == (NumC y) = x == y
    (NumC x) == (NumF y) = x == (y :+ 0)
    (NumF x) == (NumC y) = (x :+ 0) == y
    (NumC x) == (NumI y) = x == (fromIntegral y :+ 0)
    (NumI x) == (NumC y) = (fromIntegral x :+ 0) == y
    (NumR x) == (NumR y) = x == y
    (NumR x) == (NumI y) = x == toRational y
    (NumI x) == (NumR y) = toRational x == y
    (NumR x) == (NumF y) = fromRational x == y
    (NumF x) == (NumR y) = x == fromRational y
    (NumR x) == (NumC y) = (fromRational x :+ 0) == y
    (NumC x) == (NumR y) = x == (fromRational y :+ 0)
    (NumS x) == (NumS y) = x == y
    (NumS x) == (NumI y) = toInteger x == y
    (NumI x) == (NumS y) = x == toInteger y
    (NumS x) == (NumF y) = fromIntegral x == y
    (NumF x) == (NumS y) = x == fromIntegral y
    (NumS x) == (NumC y) = (fromIntegral x :+ 0) == y
    (NumC x) == (NumS y) = x == (fromIntegral y :+ 0)
    (NumS x) == (NumR y) = toRational x == y
    (NumR x) == (NumS y) = x == toRational y
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
    compare (NumR x) (NumR y) = compare x y
    compare (NumR x) (NumI y) = compare x (toRational y)
    compare (NumI x) (NumR y) = compare (toRational x) y
    compare (NumR x) (NumF y) = compare x (toRational y)
    compare (NumF x) (NumR y) = compare (toRational x) y
    compare (NumR x) (NumC y) = do
        let fx = compare (realPart y) (fromRational x)
        if fx == EQ
            then compare (imagPart y) 0
            else fx
    compare (NumC x) (NumR y) = do
        let fx = compare (realPart x) (fromRational y)
        if fx == EQ
            then compare (imagPart x) 0
            else fx
    compare (NumS x) (NumS y) = compare x y
    compare (NumS x) (NumI y) = compare (toInteger x) y
    compare (NumI x) (NumS y) = compare x (toInteger y)
    compare (NumF x) (NumS y) = compare x (fromIntegral y)
    compare (NumS x) (NumF y) = compare (fromIntegral x) y
    compare (NumR x) (NumS y) = compare x (toRational y)
    compare (NumS x) (NumR y) = compare (toRational x) y
    compare (NumS x) (NumC y) = do
        let fx = compare (realPart y) (fromIntegral x)
        if fx == EQ
            then compare (imagPart y) 0
            else fx
    compare (NumC x) (NumS y) = do
        let fx = compare (realPart x) (fromIntegral y)
        if fx == EQ
            then compare (imagPart x) 0
            else fx
instance Num LispNum where
    (NumI x) + (NumI y) = NumI $ x + y
    (NumF x) + (NumI y) = NumF $ x + fromIntegral y
    (NumI x) + (NumF y) = NumF $ fromIntegral x + y
    (NumF x) + (NumF y) = NumF $ x + y

    (NumC x) + (NumC y) = NumC $ x + y
    (NumC x) + (NumF y) = NumC $ x + (y :+ 0)
    (NumF x) + (NumC y) = NumC $ (x :+ 0) + y
    (NumC x) + (NumI y) = NumC $ x + (fromIntegral y :+ 0)
    (NumI x) + (NumC y) = NumC $ (fromIntegral x :+ 0) + y

    (NumR x) + (NumR y) = NumR $ x + y
    (NumR x) + (NumI y) = NumR $ x + fromIntegral y
    (NumI x) + (NumR y) = NumR $ fromIntegral x + y
    (NumR x) + (NumF y) = NumF $ fromRational x + y
    (NumF x) + (NumR y) = NumF $ x + fromRational y
    (NumR x) + (NumC y) = NumC $ (fromRational x :+ 0) + y
    (NumC x) + (NumR y) = NumC $ (fromRational y :+ 0) + x

    (NumS x) + (NumS y) = NumS $ x + y
    (NumS x) + (NumI y) = NumI $ toInteger x + y
    (NumI x) + (NumS y) = NumI $ x + toInteger y
    (NumS x) + (NumF y) = NumF $ fromIntegral x + y
    (NumF x) + (NumS y) = NumF $ x + fromIntegral y
    (NumS x) + (NumC y) = NumC $ (fromIntegral x :+ 0) + y
    (NumC x) + (NumS y) = NumC $ x + (fromIntegral y :+ 0)
    (NumS x) + (NumR y) = NumR $ fromIntegral x + y
    (NumR x) + (NumS y) = NumR $ x + fromIntegral y

    (NumI x) * (NumI y) = NumI $ x * y
    (NumF x) * (NumI y) = NumF $ x * fromIntegral y
    (NumI x) * (NumF y) = NumF $ fromIntegral x * y
    (NumF x) * (NumF y) = NumF $ x * y

    (NumC x) * (NumC y) = NumC $ x * y
    (NumC x) * (NumF y) = NumC $ x * (y :+ 0)
    (NumF x) * (NumC y) = NumC $ (x :+ 0) * y
    (NumC x) * (NumI y) = NumC $ x * (fromIntegral y :+ 0)
    (NumI x) * (NumC y) = NumC $ (fromIntegral x :+ 0) * y

    (NumR x) * (NumR y) = NumR $ x * y
    (NumI x) * (NumR y) = NumR $ fromIntegral x * y
    (NumR x) * (NumI y) = NumR $ x * fromIntegral y
    (NumF x) * (NumR y) = NumF $ x * fromRational y
    (NumR x) * (NumF y) = NumF $ fromRational x * y
    (NumR x) * (NumC y) = NumC $ (fromRational x :+ 0) * y
    (NumC x) * (NumR y) = NumC $ x * (fromRational y :+ 0)

    (NumS x) * (NumS y) = NumS $ x * y
    (NumS x) * (NumI y) = NumI $ toInteger x * y
    (NumI x) * (NumS y) = NumI $ x * toInteger y
    (NumS x) * (NumF y) = NumF $ fromIntegral x * y
    (NumF x) * (NumS y) = NumF $ x * fromIntegral y
    (NumS x) * (NumR y) = NumR $ toRational x * y
    (NumR x) * (NumS y) = NumR $ x * toRational y
    (NumS x) * (NumC y) = NumC $ (fromIntegral x :+ 0) * y
    (NumC x) * (NumS y) = NumC $ x * (fromIntegral y :+ 0)

    (NumI x) - (NumI y) = NumI $ x - y
    (NumF x) - (NumI y) = NumF $ x - fromIntegral y
    (NumI x) - (NumF y) = NumF $ fromIntegral x - y
    (NumF x) - (NumF y) = NumF $ x - y

    (NumC x) - (NumC y) = NumC $ x - y
    (NumC x) - (NumF y) = NumC $ x - (y :+ 0)
    (NumF x) - (NumC y) = NumC $ (x :+ 0) - y
    (NumC x) - (NumI y) = NumC $ x - (fromIntegral y :+ 0)
    (NumI x) - (NumC y) = NumC $ (fromIntegral x :+ 0) - y

    (NumR x) - (NumR y) = NumR $ x - y
    (NumI x) - (NumR y) = NumR $ fromIntegral x - y
    (NumR x) - (NumI y) = NumR $ x - fromIntegral y
    (NumF x) - (NumR y) = NumF $ x - fromRational y
    (NumR x) - (NumF y) = NumF $ fromRational x - y
    (NumR x) - (NumC y) = NumC $ (fromRational x :+ 0) - y
    (NumC x) - (NumR y) = NumC $ x - (fromRational y :+ 0)

    (NumS x) - (NumS y) = NumS $ x - y
    (NumS x) - (NumI y) = NumI $ toInteger x - y
    (NumI x) - (NumS y) = NumI $ x - toInteger y
    (NumS x) - (NumF y) = NumF $ fromIntegral x - y
    (NumF x) - (NumS y) = NumF $ x - fromIntegral y
    (NumS x) - (NumR y) = NumR $ toRational x - y
    (NumR x) - (NumS y) = NumR $ x - toRational y
    (NumS x) - (NumC y) = NumC $ (fromIntegral x :+ 0) - y
    (NumC x) - (NumS y) = NumC $ x - (fromIntegral y :+ 0)

    negate (NumI x) = NumI $ negate x
    negate (NumF x) = NumF $ negate x
    negate (NumC x) = NumC $ negate x
    negate (NumR x) = NumR $ negate x
    negate (NumS x) = NumS $ negate x
    abs (NumI x) = NumI $ abs x
    abs (NumF x) = NumF $ abs x
    abs (NumC x) = NumC $ abs x
    abs (NumR x) = NumR $ abs x
    abs (NumS x) = NumS $ abs x
    signum (NumI x) = NumI $ signum x
    signum (NumF x) = NumF $ signum x
    signum (NumC x) = NumC $ signum x
    signum (NumR x) = NumR $ signum x
    signum (NumS x) = NumS $ signum x
    fromInteger x = NumI $ fromInteger x
instance Integral LispNum where
    toInteger (NumI x) = x
    toInteger (NumS x) = toInteger x
    toInteger (NumF x) = round x
    toInteger (NumC x) = round $ realPart x
    toInteger (NumR x) = round x
    quotRem (NumI x) (NumI y) = (NumI $ quot x y, NumI $ rem x y)
    quotRem (NumF x) (NumI y) = (NumF $ x / fromIntegral y, NumF $ mod' x (fromIntegral y))
    quotRem (NumI x) (NumF y) = (NumF $ fromIntegral x / y, NumF $ mod' (fromIntegral x) y)
    quotRem (NumF x) (NumF y) = (NumF $ x / y, NumF $ mod' x y)

    quotRem (NumC x) (NumC y) = (NumC $ x / y, NumF $ 1/0)
    quotRem (NumC x) (NumI y) = (NumC $ x / (fromIntegral y :+ 0), NumF $ 1/0)
    quotRem (NumI x) (NumC y) = (NumC $ (fromIntegral x :+ 0) / y, NumF $ 1/0)
    quotRem (NumC x) (NumF y) = (NumC $ x / (y :+ 0), NumF $ 1/0)
    quotRem (NumF x) (NumC y) = (NumC $ (x :+ 0) / y, NumF $ 1/0)

    quotRem (NumR x) (NumR y) = (NumR $ x / fromRational y, NumR $ mod' x y)
    quotRem (NumI x) (NumR y) = (NumR $ toRational x / y, NumR $ mod' (toRational x) y)
    quotRem (NumR x) (NumI y) = (NumR $ x / toRational y, NumR $ mod' x (toRational y))
    quotRem (NumF x) (NumR y) = (NumF $ x / fromRational y, NumF $ mod' x (fromRational y))
    quotRem (NumR x) (NumF y) = (NumF $ fromRational x / y, NumF $ mod' (fromRational x) y)
    quotRem (NumC x) (NumR y) = (NumC $ x / (fromRational y :+ 0), NumF $ 1/0)
    quotRem (NumR x) (NumC y) = (NumC $ (fromRational x :+ 0) / y, NumF $ 1/0)

    quotRem (NumS x) (NumS y) = (NumS $ quot x y, NumS $ rem x y)
    quotRem (NumI x) (NumS y) = (NumI $ quot x (toInteger y), NumI $ rem x (toInteger y))
    quotRem (NumS x) (NumI y) = (NumI $ quot (toInteger x) y, NumI $ rem (toInteger x) y)
    quotRem (NumF x) (NumS y) = (NumF $ x / fromIntegral y, NumF $ mod' x (fromIntegral y))
    quotRem (NumS x) (NumF y) = (NumF $ fromIntegral x / y, NumF $ mod' (fromIntegral x) y)
    quotRem (NumC x) (NumS y) = (NumC $ x / (fromIntegral y :+ 0), NumF $ 1/0)
    quotRem (NumS x) (NumC y) = (NumC $ (fromIntegral x :+ 0) / y, NumF $ 1/0)
    quotRem (NumR x) (NumS y) = (NumR $ x / toRational y, NumR $ mod' x (toRational y))
    quotRem (NumS x) (NumR y) = (NumR $ toRational x / y, NumR $ mod' (toRational x) y)
instance Real LispNum where
    toRational (NumI x) = toRational x
    toRational (NumF x) = toRational x
    toRational (NumC _) = 0
    toRational (NumR x) = x
    toRational (NumS x) = toRational x
instance Enum LispNum where
    toEnum x = NumI $ toInteger x
    fromEnum (NumI x) = fromIntegral x
    fromEnum (NumF x) = round x
    fromEnum (NumC _) = 0
    fromEnum (NumR _) = 0
    fromEnum (NumS x) = fromIntegral x
-- | a LispNum data type comprising a float and an integer
data LispNum = NumI Integer
             | NumF Double
             | NumC (Complex Double)
             | NumR Rational
             | NumS Int

data Simple = Atom String
            | Number LispNum
            | String String
            | Character Char
            | Bool Bool
            | Nil String
            | SimpleList [Simple]
    deriving (Eq, Ord)

instance Show LispVal where show = showVal
-- | a LispVal data type comprising all Lisp data types
data LispVal = SimpleVal Simple
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | HashMap (Data.Map.Map Simple LispVal)
             | PrimitiveFunc  ([LispVal] -> ThrowsError LispVal)
             | IOFunc  ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Func LispFun
             | EvalFunc ([LispVal] -> IOThrowsError LispVal)
             | Pointer { pointerVar :: String, pointerEnv :: Env }
             | Cont Continuation
             | ListComprehension LispVal LispVal LispVal (Maybe LispVal)

data Continuation = Continuation { contClosure :: Env
                                 , cbody :: [LispVal]
                                 , cont :: LispVal
                                 , frameFunc :: Maybe LispVal
                                 , frameEvaledArgs :: Maybe [LispVal]
                                 }

-- | a LispFun data type
data LispFun = LispFun { params :: [String]
                       , vararg :: Maybe String
                       , body :: [LispVal]
                       , closure :: Env
                       , docstring :: String
                       }

instance Show LispError where show = showError
-- | a LispError data type comprising all errors that
-- | can be emitted from within the interpreter
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | ParseErr ParseError
               | BadSpecialForm String LispVal
               | BadSpecialForms String [LispVal]
               | NotFunction String String
               | UnboundVar String String
               | InternalError String
               | Default String

instance Eq Env where
    (Environment _ xb xp) == (Environment _ yb yp) = (xb == yb) && (xp == yp)
-- | an Environment type where all variables are stored
data Env = Environment {
        parentEnv :: Maybe Env
      , bindings :: IORef (Data.Map.Map String (IORef LispVal))
      , pointers :: IORef (Data.Map.Map String (IORef [LispVal]))
}

-- | a ThrowsError type containing either an error or a value
type ThrowsError = Either LispError
-- | a IOThrowsError type containing either an error or an IO
type IOThrowsError = ExceptT LispError IO

showNum :: LispNum -> String
showNum (NumF contents) = show contents
showNum (NumI contents) = show contents
showNum (NumC contents) = show (realPart contents) ++ "+" ++ show (imagPart contents) ++ "i"
showNum (NumR contents) = show (numerator contents) ++ "/" ++ show (denominator contents)
showNum (NumS contents) = show contents ++ "s"

-- | a show function for all LispVals
showVal :: LispVal -> String
showVal (SimpleVal (String contents)) = contents
showVal (SimpleVal (Atom name)) = name
showVal (SimpleVal (Bool True)) = "#t"
showVal (SimpleVal (Bool False)) = "#f"
showVal (SimpleVal (Character c)) = show c
showVal (SimpleVal (Number n)) = showNum n
showVal (SimpleVal (SimpleList contents)) = "simple(" ++ unwordsList (map SimpleVal contents) ++")"
showVal (List contents) = "(" ++ unwordsList contents ++")"
showVal (Vector contents) = "#(" ++ unwordsList (elems contents) ++ ")"
showVal (HashMap contents) = "#{" ++ unwordsList (Data.Map.elems contents) ++ "}"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (EvalFunc _) = "<eval primitive>"
showVal (Port _) = "<IO port>"
showVal (Func LispFun {params = args, vararg = varargs, body = _, closure = _,
                       docstring = doc}) =
    doc ++ "\n  source: " ++
    "(lambda (" ++ unwords (fmap show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Pointer p _) = "<pointer " ++ p ++ ">"
showVal (SimpleVal (Nil _)) = "nil"
showVal (Cont _) = "<continuation>"
showVal (ListComprehension expr filt _ _) = "<list comprehension: " ++
                                            show expr ++ " : " ++
                                            show filt ++ ">"

-- | a show function for all LispErrors
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form ++
                                          " (type: " ++ typeString form ++ ")"
showError (BadSpecialForms message forms) = message ++ ": " ++ (init $
                                           init $ unwords $
                                           map (\form -> show form ++
                                                " (type: " ++ typeString form ++
                                                "), ") forms)
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
        let x = "Expected " ++ show expected ++
                " args; found " ++ show (length found)
        in if length found /= 0
              then x ++ "; values are " ++ unwords (map extractStr found)
              else x
    where extractStr val = let y = typeString val
                           in if y /= "function"
                                then show val ++ " (type: " ++ y ++ ") "
                                else "<compiled function>"
showError (TypeMismatch expected found) =
        let x = typeString found
            y = "Invalid type: expected " ++ expected ++ ", found " ++ x
        in if x /= "function"
              then y ++ " (value: " ++ show found ++ ")"
              else y
showError (ParseErr parseErr) = "Parse error at " ++ show parseErr
showError (InternalError err) = "Internal error: " ++ err
showError (Default err) = err

fromSimple :: Simple -> LispVal
fromSimple x = SimpleVal x

toSimple :: LispVal -> Simple
toSimple (SimpleVal x) = x
toSimple _ = Nil ""

typeString :: LispVal -> String
typeString (SimpleVal (Number (NumI _))) = "integer"
typeString (SimpleVal (Number (NumS _))) = "small integer"
typeString (SimpleVal (Number (NumF _))) = "float"
typeString (SimpleVal (Number (NumR _))) = "rational"
typeString (SimpleVal (Number (NumC _))) = "complex"
typeString (SimpleVal (Bool _)) = "boolean"
typeString (SimpleVal (Character _)) = "character"
typeString (SimpleVal (String _)) = "string"
typeString (SimpleVal (Atom (':' : _))) = "atom"
typeString (SimpleVal (Atom _)) = "symbol"
typeString (SimpleVal (Nil _)) = "nil"
typeString (SimpleVal (SimpleList _)) = "simple list"
typeString (List _) = "list"
typeString (DottedList _ _) = "dotted list"
typeString (Vector _) = "vector"
typeString (HashMap _) = "hashmap"
typeString (PrimitiveFunc _) = "primitive"
typeString (IOFunc _) = "io primitive"
typeString (EvalFunc _) = "eval primitive"
typeString (Port _) = "port"
typeString (Func _) = "function"
typeString (Pointer _ _) = "pointer"
typeString (Cont _) = "continuation"
typeString (ListComprehension _ _ _ _) = "list comprehension"

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap showVal

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

nullCont :: Env -> LispVal
nullCont env = Cont $ Continuation env [] (fromSimple (Nil "")) Nothing Nothing

-- | lift a ThrowsError to an IOThrowsError
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- | lift an IOThrowsError to an IO monad
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runExceptT (trapError action))
