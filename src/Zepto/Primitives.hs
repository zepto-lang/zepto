module Zepto.Primitives(primitives
                       , ioPrimitives
                       , evalPrimitives
                       , eval
                       , versionStr
                       , evalString) where
import Data.Array
import Data.Char hiding(isNumber, isSymbol)
import Data.Complex
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Except
import System.Directory
import System.Exit
import System.IO
import System.IO.Error

import Paths_zepto
import Zepto.Types
import Zepto.Parser
import Zepto.Variables
import Zepto.Macro

-- | a list of all regular primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal, String)]
primitives = [ ("+", numericPlusop (+), "add two values")
             , ("-", numericMinop (-), "subtract two values/negate value")
             , ("*", numericBinop (*), "multiply two values")
             , ("/", numericBinop div, "divide two values")
             , ("mod", numericBinop mod, "modulo of two values")
             , ("modulo", numericBinop mod, "modulo of two values")
             , ("quotient", numericBinop quot, "quotient of two values")
             , ("remainder", numericBinop rem, "remainder of two values")
             , ("round", numRound round, "rounds a number")
             , ("floor", numRound floor, "floors a number")
             , ("ceiling", numRound ceiling, "ceils a number")
             , ("truncate", numRound truncate, "truncates a number")
             , ("expt", numPow, "power of function")
             , ("pow", numPow, "power of function")
             , ("^", numPow, "power of function")
             , ("**", numPow, "power of function")
             , ("sqrt", numSqrt, "square root function")
             , ("log", numLog, "logarithm function")
             , ("abs", numOp abs, "get absolute value")
             , ("sin", numOp sin, "sine function")
             , ("cos", numOp cos, "cosine function")
             , ("tan", numOp tan, "tangens function")
             , ("asin", numOp asin, "asine function")
             , ("acos", numOp acos, "acosine function")
             , ("atan", numOp atan, "atangens function")
             , ("=", numBoolBinop (==), "compare equality of two values")
             , ("<", numBoolBinop (<), "compare equality of two values")
             , (">", numBoolBinop (>), "compare equality of two values")
             , ("/=", numBoolBinop (/=), "compare equality of two values")
             , (">=", numBoolBinop (>=), "compare equality of two values")
             , ("<=", numBoolBinop (<=), "compare equality of two values")
             , ("&&", boolMulop (&&), "and operation")
             , ("||", boolMulop (||), "or operation")

             , ("string=?", strBoolBinop (==), "compare equality of two strings")
             , ("string>?", strBoolBinop (>), "compare equality of two strings")
             , ("string<?", strBoolBinop (<), "compare equality of two strings")
             , ("string<=?", strBoolBinop (<=), "compare equality of two strings")
             , ("string>=?", strBoolBinop (>=), "compare equality of two strings")
             , ("string-ci=?", strCIBoolBinop (==), "compare equality of two strings(case insensitive)")
             , ("string-ci>?", strCIBoolBinop (>), "compare equality of two strings(case insensitive)")
             , ("string-ci<?", strCIBoolBinop (<), "compare equality of two strings(case insensitive)")
             , ("string-ci<=?", strCIBoolBinop (<=), "compare equality of two strings(case insensitive)")
             , ("string-ci>=?", strBoolBinop (>=), "compare equality of two strings")
             , ("newline", printNewline, "print a newline")
             , ("car", car, "take head of list")
             , ("cdr", cdr, "take tail of list")
             , ("cons", cons, "construct list")
             , ("eq?", eqv, "check equality")
             , ("eqv?", eqv, "check equality")
             , ("equal?", equal, "check equality")

             , ("pair?", isDottedList, "check whether variable is a pair")
             , ("procedure?", isProcedure, "check whether variable is a procedure")
             , ("number?", isNumber, "check whether variable is a number")
             , ("integer?", isInteger, "check whether variable is an integer")
             , ("rational?", isRational, "check whether variable is an integer")
             , ("real?", isReal, "check whether variable is a real number")
             , ("list?", unaryOp isList, "check whether variable is list")
             , ("null?", isNull, "check whether variable is null")
             , ("symbol?", isSymbol, "check whether variable is symbol")
             , ("vector?", unaryOp isVector, "check whether variable is vector")
             , ("string?", isString, "check whether variable is string")
             , ("char?", isChar, "check whether vairable is char")
             , ("boolean?", isBoolean, "check whether variable is boolean")
             , ("vector", buildVector, "build a new vector")
             , ("string", buildString, "build a new string")
             , ("char-downcase", charDowncase, "downcases a char")
             , ("vector-length", vectorLength, "get length of vector")
             , ("string-length", stringLength, "get length of string")
             , ("make-string", makeString, "make a new string")
             , ("make-vector", makeVector, "create a vector")
             , ("make-small", makeSmall, "create a small integer")
             , ("char->integer", charToInteger, "makes integer from char")
             , ("vector->list", vectorToList, "makes list from vector")
             , ("list->vector", listToVector, "makes vector from list")
             , ("symbol->string", symbol2String, "makes string from symbol")
             , ("string->symbol", string2Symbol, "makes symbol from string")
             , ("string->number", stringToNumber, "makes number from string")
             , ("string->list", stringToList, "makes list from string")
             , ("list->string", listToString, "makes string from list")
             , ("string-copy", stringCopy, "copy string")
             , ("substring", substring, "makes substring from string")
             , ("vector-ref", vectorRef, "get element from vector")
             , ("string-ref", stringRef, "get element from string")
             , ("string-find", stringFind, "find first occurrence in string")
             , ("string-append", stringAppend, "append to string")
             , ("zepto-version", getVersion, "gets the version as a list")
             , ("zepto-version-str", getVersionStr, "gets the version as a string")
             , ("zepto-major-version", getMajVersion, "gets the major version number")
             , ("zepto-minor-version", getMinVersion, "gets the minor version number")
             , ("zepto-patch-version", getPatchVersion, "gets the patch version number")
             ]

-- | a list of all io-bound primitives
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
ioPrimitives = [ ("open-input-file", makePort ReadMode, "open a file for reading")
               , ("open-output-file", makePort WriteMode, "open a file for writing")
               , ("close-input-file", closePort, "close a file opened for reading")
               , ("close-output-file", closePort, "close a file opened for writing")
               , ("read", readProc, "read from file")
               , ("write", writeProc hPrint, "write to file")
               , ("display", writeProc (\ port obj ->
                        case obj of
                            String str -> hPutStr port str
                            _ -> hPutStr port $ show obj), "print to stdout")
               , ("error", errorProc, "write to stderr")
               , ("read-contents", readContents, "read contents of file")
               , ("read-all", readAll, "read and parse file")
               , ("exit", exitProc, "exit program")
               , ("color", colorProc, "colorize output")
               ]

evalPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
evalPrimitives = [ ("eval", evalFun, "evaluate list")
                 , ("apply", evalApply, "apply function to values")
                 , ("call-with-current-continuation", evalCallCC, "call with current continuation")
                 , ("call/cc", evalCallCC, "call with current continuation")
                 --, ("call-with-values", evalCallWValues, "call with values"),
                 --, ("load-ffi", evalFFI, "load foreign function")
                 ]

numericBinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op p = liftM (Number . foldl1 op) (mapM unpackNum p)

numericMinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericMinop _ [Number l] = return $ Number $ negate l
numericMinop op p = liftM (Number . foldl1 op) (mapM unpackNum p)

numericPlusop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericPlusop _ [Number l] = if l > 0 then return $ Number l
                                      else return $ Number $ negate l
numericPlusop op p = liftM (Number . foldl1 op) (mapM unpackNum p)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

boolMulop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolMulop op p = liftM (Bool . foldl1 op) (mapM unpackBool p)

numBoolBinop :: (LispNum -> LispNum -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

strCIBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strCIBoolBinop = boolBinop unpackCIStr

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ _ = throwError $ InternalError "Internal error in unaryOp"

unpackNum :: LispVal -> ThrowsError LispNum
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackCIStr :: LispVal -> ThrowsError String
unpackCIStr (String s) = return $ fmap toLower s
unpackCIStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

numRound :: (Double -> Integer) -> [LispVal] -> ThrowsError LispVal
numRound _ [n@(Number (NumI _))] = return n
numRound _ [n@(Number (NumS _))] = return n
numRound op [Number (NumF n)] = return $ Number $ NumI $ op n
numRound op [Number (NumC n)] = return $ Number $ NumC $ fromInteger (op $ realPart n) :+ fromInteger (op $ imagPart n)
numRound op [Number (NumR n)] = return $ Number $ NumI $ op $ fromRational n
numRound _ [x] = throwError $ TypeMismatch "number" x
numRound _ badArgList = throwError $ NumArgs 1 badArgList

numOp :: (Double -> Double) -> [LispVal] -> ThrowsError LispVal
numOp op [Number (NumI n)] = return $ Number $ NumF $ op $ fromInteger n
numOp op [Number (NumS n)] = return $ Number $ NumF $ op $ fromIntegral n
numOp op [Number (NumF n)] = return $ Number $ NumF $ op n
numOp op [Number (NumR n)] = return $ Number $ NumR $ toRational $ op $ fromRational n
numOp op [Number (NumC n)] = return $ Number $ NumC $ op (realPart n) :+ op (imagPart n)
numOp _ [x] = throwError $ TypeMismatch "number" x
numOp _ badArgList = throwError $ NumArgs 1 badArgList

numLog :: [LispVal] -> ThrowsError LispVal
numLog [Number (NumI n)] = return $ Number $ NumF $ log $ fromInteger n
numLog [Number (NumS n)] = return $ Number $ NumF $ log (fromIntegral n)
numLog [Number (NumF n)] = return $ Number $ NumF $ log n
numLog [Number (NumR n)] = return $ Number $ NumF $ log $ fromRational n
numLog [Number (NumC n)] = return $ Number $ NumC $ log n
numLog [Number (NumI n), Number (NumI base)] =
    return $ Number $ NumF $ logBase (fromInteger base) (fromIntegral n)
numLog [Number (NumS n), Number (NumI base)] =
    return $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [Number (NumI n), Number (NumS base)] =
    return $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [Number (NumS n), Number (NumS base)] =
    return $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [Number (NumF n), Number (NumI base)] =
    return $ Number $ NumF $ logBase (fromInteger base) n
numLog [Number (NumC n), Number (NumI base)] =
    return $ Number $ NumC $ logBase (fromInteger base) n
numLog [Number (NumF n), Number (NumS base)] =
    return $ Number $ NumF $ logBase (fromIntegral base) n
numLog [Number (NumC n), Number (NumS base)] =
    return $ Number $ NumC $ logBase (fromIntegral base) n
numLog [x] = throwError $ TypeMismatch "number" x
numLog badArgList = throwError $ NumArgs 1 badArgList

--TODO SmallInt support
numPow :: [LispVal] -> ThrowsError LispVal
numPow [Number (NumI n), wrong@(Number (NumI base))] =
    if base > -1
        then return $ Number $ NumI $ n ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [Number (NumS n), wrong@(Number (NumI base))] =
    if base > -1
        then return $ Number $ NumI $ (fromIntegral n) ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [Number (NumI n), wrong@(Number (NumS base))] =
    if base > -1
        then return $ Number $ NumI $ n ^ (fromIntegral base::Integer)
        else throwError $ TypeMismatch "positive" wrong
numPow [Number (NumS n), wrong@(Number (NumS base))] =
    if base > -1
        then return $ Number $ NumS $ n ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [Number (NumF n), Number (NumI base)] =
    return $ Number $ NumF $ n ** fromIntegral base
numPow [Number (NumI n), Number (NumF base)] =
    return $ Number $ NumF $ fromIntegral n ** base
numPow [Number (NumF n), Number (NumF base)] =
    return $ Number $ NumF $ n ** base
numPow [Number (NumC n), Number (NumI base)] =
    return $ Number $ NumC $ n ** fromIntegral base
numPow [Number (NumC n), Number (NumF base)] =
    return $ Number $ NumC $ n ** (base :+ 0)
numPow [Number (NumR n), Number (NumR base)] =
    return $ Number $ NumF $ fromRational n ** fromRational base
numPow [Number (NumR n), Number (NumI base)] =
    return $ Number $ NumF $ fromRational n ** fromIntegral base
numPow [Number (NumI n), Number (NumR base)] =
    return $ Number $ NumF $ fromIntegral n ** fromRational base
numPow [Number (NumR n), Number (NumF base)] =
    return $ Number $ NumF $ fromRational n ** base
numPow [Number (NumF n), Number (NumR base)] =
    return $ Number $ NumF $ n ** fromRational base
numPow [Number (NumC n), Number (NumR base)] =
    return $ Number $ NumC $ n ** (fromRational base :+ 0)
numPow [Number _, x] = throwError $ TypeMismatch "number" x
numPow [x, Number _] = throwError $ TypeMismatch "number(not complex)" x
numPow badArgList = throwError $ NumArgs 2 badArgList

numSqrt :: [LispVal] -> ThrowsError LispVal
numSqrt [Number (NumI n)] = if n >= 0 then return $ Number $ NumF $ sqrt $ fromInteger n
                                      else return $ Number $ NumC $ sqrt (fromInteger n :+ 0)
numSqrt [Number (NumS n)] = if n >= 0 then return $ Number $ NumF $ sqrt $ fromIntegral n
                                      else return $ Number $ NumC $ sqrt (fromIntegral n :+ 0)
numSqrt [Number (NumF n)] = if n >= 0 then return $ Number $ NumF $ sqrt n
                                      else return $ Number $ NumC $ sqrt (n :+ 0)
numSqrt [Number (NumC n)] = return $ Number $ NumC $ sqrt n
numSqrt [Number (NumR n)] = return $ Number $ NumF $ sqrt $ fromRational n
numSqrt [x] = throwError $ TypeMismatch "number" x
numSqrt badArgList = throwError $ NumArgs 1 badArgList

printNewline :: [LispVal] -> ThrowsError LispVal
printNewline [] = return $ String $ unlines [""]
printNewline [badArg] = throwError $ TypeMismatch "nothing" badArg
printNewline badArgList = throwError $ NumArgs 1 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [x@(EvalFunc _), y@(EvalFunc _)] = return $ Bool $ show x == show y
eqv [x@(PrimitiveFunc _), y@(PrimitiveFunc _)] = return $ Bool $ show x == show y
eqv [x@(IOFunc _), y@(IOFunc _)] = return $ Bool $ show x == show y
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                  and (zipWith (curry eqvPair) arg1 arg2)
                                  where eqvPair (x, y) = case eqv[x, y] of
                                                            Left _ -> False
                                                            Right (Bool val) -> val
                                                            _ -> False
eqv [Vector arg1, Vector arg2] = eqv [List (elems arg1), List (elems arg2)]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List arg1, List arg2] =
        return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                Left _           -> False
                                Right (Bool val) -> val
                                _                -> False
eqvList _ _ = throwError $ InternalError "Unexpected error in eqvList"

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) =
        do unpacked1 <- unpacker x
           unpacked2 <- unpacker y
           return $ unpacked1 == unpacked2
        `catchError` const (return False)

equal :: [LispVal] ->ThrowsError LispVal
equal [lx@(List _), ly@(List _)] = eqvList equal [lx, ly]
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [Vector arg1, Vector arg2] = eqvList equal [List (elems arg1), List (elems arg2)]
equal [x, y] = do
           primitiveEquals <- liftM or $ mapM (unpackEquals x y)
                              [AnyUnpacker unpackNum, AnyUnpacker unpackStr,
                               AnyUnpacker unpackBool]
           eqvEquals <- eqv [x, y]
           return $ Bool (primitiveEquals || let (Bool z) = eqvEquals in z)
equal badArgList = throwError $ NumArgs 2 badArgList

makeSmall :: [LispVal] -> ThrowsError LispVal
makeSmall [Number (NumI n)] = return $ Number $ NumS $ fromInteger n
makeSmall [badType] = throwError $ TypeMismatch "integer" badType
makeSmall badArgList = throwError $ NumArgs 1 badArgList

makeVector, buildVector, vectorLength, vectorRef, vectorToList, listToVector, stringRef, stringFind :: [LispVal] -> ThrowsError LispVal
makeVector [Number n] = makeVector [Number n, List []]
makeVector [Number (NumI n), a] = do
    let l = replicate (fromInteger n) a
    return $ Vector $ listArray (0, length l - 1) l
makeVector [Number (NumS n), a] = do
    let l = replicate n a
    return $ Vector $ listArray (0, length l - 1) l
makeVector [badType] = throwError $ TypeMismatch "integer" badType
makeVector badArgList = throwError $ NumArgs 1 badArgList

buildVector (o:os) = do
    let l = o:os
    return $ Vector $ listArray (0, length l - 1) l
buildVector badArgList = throwError $ NumArgs 1 badArgList

vectorLength [Vector v] = return $ Number $ NumI $ toInteger $ length (elems v)
vectorLength [badType] = throwError $ TypeMismatch "vector" badType
vectorLength badArgList = throwError $ NumArgs 1 badArgList

vectorRef [Vector v, Number (NumI n)] = return $ v ! fromInteger n
vectorRef [Vector v, Number (NumS n)] = return $ v ! n
vectorRef [badType] = throwError $ TypeMismatch "vector integer" badType
vectorRef badArgList = throwError $ NumArgs 2 badArgList

vectorToList [Vector v] = return $ List $ elems v
vectorToList [badType] = throwError $ TypeMismatch "vector" badType
vectorToList badArgList = throwError $ NumArgs 1 badArgList

listToVector [List l] = return $ Vector $ listArray (0, length l - 1) l
listToVector [badType] = throwError $ TypeMismatch "list" badType
listToVector badArgList = throwError $ NumArgs 1 badArgList

buildString :: [LispVal] -> ThrowsError LispVal
buildString [List []] = return $ String ""
buildString [Character c] = return $ String [c]
buildString (Character c:rest) = do
    cs <- buildString rest
    case cs of
        String s -> return $ String $ c : s
        badType -> throwError $ TypeMismatch "character" badType
buildString [badType] = throwError $ TypeMismatch "character" badType
buildString badArgList = throwError $ NumArgs 1 badArgList

makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number n] = return $ _makeString n ' ' ""
    where _makeString count ch s =
            if count == 0
                then String s
                else _makeString (count - 1) ch (s ++ [ch])
makeString badArgList = throwError $ NumArgs 1 badArgList

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return $ Number $ foldr (const (+1)) 0 s
stringLength [badType] = throwError $ TypeMismatch "string" badType
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringRef [String v, Number (NumI n)] =
        if n >= 0
           then return $ Character $ v !! fromInteger n
           else return $ Character $ v !! ((length v) - fromInteger n)
stringRef [String v, Number (NumS n)] =
        if n >= 0
           then return $ Character $ v !! n
           else return $ Character $ v !! ((length v) - n)
stringRef [badType] = throwError $ TypeMismatch "string integer" badType
stringRef badArgList = throwError $ NumArgs 2 badArgList

stringFind [String v, Character x] =
        case findIndex (\m -> x == m) v of
           Just n -> return $ Number $ NumI $ toInteger n
           _      -> return $ Number $ NumI $ -1
stringFind [badType, Character _] = throwError $ TypeMismatch "string" badType
stringFind [String _, badType] = throwError $ TypeMismatch "string" badType
stringFind badArgList = throwError $ NumArgs 2 badArgList

substring :: [LispVal] -> ThrowsError LispVal
substring [String s, Number (NumI start), Number (NumI end)] = do
    let len = fromInteger $ end - start
    let begin = fromInteger start
    return $ String $ (take len . drop begin) s
substring [String s, Number (NumS start), Number (NumS end)] = do
    let len = end - start
    return $ String $ (take len . drop start) s
substring [badType] = throwError $ TypeMismatch "string integer integer" badType
substring badArgList = throwError $ NumArgs 3 badArgList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [String s] = return $ String s
stringAppend (String st:sts) = do
    rest <- stringAppend sts
    case rest of
        String s -> return $ String $ st ++ s
        elsewise -> throwError $ TypeMismatch "string" elsewise
stringAppend [badType] = throwError $ TypeMismatch "string" badType
stringAppend badArgList = throwError $ NumArgs 1 badArgList

stringToNumber :: [LispVal] -> ThrowsError LispVal
stringToNumber [String s] = do
        result <- readExpr s
        case result of
            n@(Number _) -> return n
            _ -> return $ Bool False
stringToNumber [String s, Number base] =
    case base of
        2 -> stringToNumber [String $ "#b" ++ s]
        8 -> stringToNumber [String $ "#o" ++ s]
        10 -> stringToNumber [String s]
        16 -> stringToNumber [String $ "#x" ++ s]
        _ -> throwError $ Default $ "Invalid base: " ++ show base
stringToNumber [badType] = throwError $ TypeMismatch "string" badType
stringToNumber badArgList = throwError $ NumArgs 1 badArgList

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String s] = return $ List $ fmap Character s
stringToList [badType] = throwError $ TypeMismatch "string" badType
stringToList badArgList = throwError $ NumArgs 1 badArgList

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List []] = return $ String ""
listToString [List l] = buildString l
listToString [badType] = throwError $ TypeMismatch "list" badType
listToString badArgList = throwError $ NumArgs 1 badArgList

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String s] = return $ String s
stringCopy [badType] = throwError $ TypeMismatch "string" badType
stringCopy badArgList = throwError $ NumArgs 1 badArgList

charDowncase :: [LispVal] -> ThrowsError LispVal
charDowncase [Character c] = return $ Character $ toLower c
charDowncase [badType] = throwError $ TypeMismatch "character" badType
charDowncase badArgList = throwError $ NumArgs 1 badArgList

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber ([Number _]) = return $ Bool True
isNumber _ = return $ Bool False

isReal :: [LispVal] -> ThrowsError LispVal
isReal ([Number (NumC x)]) = return $ Bool $ imagPart x == 0
isReal ([Number _]) = return $ Bool True
isReal _ = return $ Bool False

isInteger :: [LispVal] -> ThrowsError LispVal
isInteger ([Number (NumI _)]) = return $ Bool True
isInteger ([Number (NumS _)]) = return $ Bool True
isInteger _ = return $ Bool False

isRational :: [LispVal] -> ThrowsError LispVal
isRational ([Number (NumR _)]) = return $ Bool True
isRational ([Number (NumS _)]) = return $ Bool True
isRational ([Number (NumI _)]) = return $ Bool True
isRational ([Number (NumF x)]) =
        if x == fromInteger (round x)
            then return $ Bool True
            else return $ Bool False
isRational _ = return $ Bool False

isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList ([DottedList _ _]) = return $ Bool True
isDottedList _ = return $ Bool False

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure ([PrimitiveFunc _]) = return $ Bool True
isProcedure ([EvalFunc _]) = return $ Bool True
isProcedure ([Func _]) = return $ Bool True
isProcedure ([IOFunc _]) = return $ Bool True
isProcedure ([Cont _]) = return $ Bool True
isProcedure _ = return $ Bool False

isVector, isList :: LispVal -> ThrowsError LispVal
isVector (Vector _) = return $ Bool True
isVector _ = return $ Bool False
isList (List _) = return $ Bool True
isList _ = return $ Bool False

isNull :: [LispVal] -> ThrowsError LispVal
isNull ([List []]) = return $ Bool True
isNull _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol ([Atom _]) = return $ Bool True
isSymbol _ = return $ Bool False

symbol2String :: [LispVal] -> ThrowsError LispVal
symbol2String ([Atom a]) = return $ String a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbol2String [] = return $ Bool False
symbol2String _ = return $ Bool False

string2Symbol :: [LispVal] -> ThrowsError LispVal
string2Symbol [] = return $ Bool False
string2Symbol ([String s]) = return $ Atom s
string2Symbol [notString] = throwError $ TypeMismatch "string" notString
string2Symbol args@(_ : _) = throwError $ NumArgs 1 args

charToInteger :: [LispVal] -> ThrowsError LispVal
charToInteger [] = return $ Bool False
charToInteger ([Character c]) = return $ Number $ NumI $ toInteger $ ord c
charToInteger [notChar] = throwError $ TypeMismatch "character" notChar
charToInteger args@(_ : _) = throwError $ NumArgs 1 args

isString :: [LispVal] -> ThrowsError LispVal
isString ([String _]) = return $ Bool True
isString _ = return $ Bool False

isChar :: [LispVal] -> ThrowsError LispVal
isChar ([Character _]) = return $ Bool True
isChar _ = return $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean ([Bool _]) = return $ Bool True
isBoolean _ = return $ Bool False

version' :: [Int]
version' = [0, 6, 10]

versionStr :: String
versionStr = intercalate "." $ fmap show version'

majorVersion :: Int
majorVersion = head version'

minorVersion :: Int
minorVersion = version' !! 1

patchVersion :: Int
patchVersion = version' !! 2

getVersion :: [LispVal] -> ThrowsError LispVal
getVersion [] = return $ List $ fmap (String . show) version'
getVersion badList = throwError $ NumArgs 0 badList

getVersionStr :: [LispVal] -> ThrowsError LispVal
getVersionStr [] = return $ String versionStr
getVersionStr badList = throwError $ NumArgs 0 badList

getMajVersion :: [LispVal] -> ThrowsError LispVal
getMajVersion [] = return $ Number $ NumI $ toInteger majorVersion
getMajVersion badList = throwError $ NumArgs 0 badList

getMinVersion :: [LispVal] -> ThrowsError LispVal
getMinVersion [] = return $ Number $ NumI $ toInteger minorVersion
getMinVersion badList = throwError $ NumArgs 0 badList

getPatchVersion :: [LispVal] -> ThrowsError LispVal
getPatchVersion [] = return $ Number $ NumI $ toInteger patchVersion
getPatchVersion badList = throwError $ NumArgs 0 badList

-- | searches all primitives for a possible completion
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
        liftThrows (readExpr expr) >>=
        macroEval env >>=
        eval env (nullCont env)

contEval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
contEval _ (Cont (Continuation cEnv cBody cCont Nothing Nothing)) val =
    case cBody of
        [] ->
            case cCont of
                Cont (Continuation nEnv _ _ _ _) -> contEval nEnv cCont val
                _ -> return val
        [lval] -> eval cEnv (Cont (Continuation cEnv [] cCont Nothing Nothing)) lval
        (lval : lvals) -> eval cEnv (Cont (Continuation cEnv lvals cCont Nothing Nothing)) lval
contEval _ _ _ = throwError $ InternalError "This should never happen"

evalFun :: [LispVal] -> IOThrowsError LispVal
evalFun [Cont (Continuation env _ _ _ _), val] = macroEval env val
evalFun (_ : args) = throwError $ NumArgs 1 args
evalFun _ = throwError $ NumArgs 1 []

evalApply :: [LispVal] -> IOThrowsError LispVal
evalApply [conti@(Cont _), fun, List args] = apply conti fun args
evalApply (_ : args) = throwError $ NumArgs 2 args
evalApply _ = throwError $ NumArgs 2 []

evalCallCC :: [LispVal] -> IOThrowsError LispVal
evalCallCC [conti@(Cont _), fun] =
        case fun of
            Cont _ -> apply conti fun [conti]
            PrimitiveFunc f -> do
                result <- liftThrows $ f [conti]
                case conti of
                    Cont (Continuation cEnv _ _ _ _) -> contEval cEnv conti result
                    _ -> return result
            Func (LispFun _ (Just _) _ _ _) -> apply conti fun [conti]
            Func (LispFun aparams _ _ _ _) ->
                if length aparams == 1
                    then apply conti fun [conti]
                    else throwError $ NumArgs (toInteger $ length aparams) [conti]
            other -> throwError $ TypeMismatch "procedure" other
evalCallCC (_ : args) = throwError $ NumArgs 1 args
evalCallCC _ = throwError $ NumArgs 1 []

findFile' :: String -> ExceptT LispError IO String
findFile' filename = do
        fileAsLib <- liftIO $ getDataFileName $ "stdlib/" ++ filename
        exists <- fex filename
        existsLib <- fex fileAsLib
        case (exists, existsLib) of
            (Bool False, Bool True) -> return fileAsLib
            _ -> return filename
    where
        fex file = do ex <-liftIO $ doesFileExist file
                      return $ Bool ex


-- | evaluates a parsed expression
eval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
eval env conti val@(Nil _) = contEval env conti val
eval env conti val@(String _) = contEval env conti val
eval env conti val@(Number _) = contEval env conti val
eval env conti val@(Bool _) = contEval env conti val
eval env conti val@(Character _) = contEval env conti val
eval env conti val@(Vector _) = contEval env conti val
eval env conti (Atom a) = contEval env conti =<< getVar env a
eval _ _ (List [Atom "quote"]) = throwError $ NumArgs 1 []
eval env conti (List [Atom "quote", val]) = contEval env conti val
eval _ _ (List (Atom "quote" : x)) = throwError $ NumArgs 1 x
eval _ _ (List [Atom "if"]) = throwError $ NumArgs 3 []
eval env conti (List [Atom "if", p, conseq, alt]) = do
        result <- eval env conti p
        case result of
            Bool False -> eval env conti alt
            _          -> eval env conti conseq
eval env conti (List [Atom "if", predicate, conseq]) = do
        result <- eval env conti predicate
        case result of
            Bool True -> eval env conti conseq
            _         -> eval env conti $ List []
eval _ _ (List [Atom "if", x]) = throwError $ BadSpecialForm
                            ("if needs a predicate and a consequence "
                            ++ "plus an optional alternative clause")
                            x
eval _ _ (List (Atom "if" : x)) = throwError $ NumArgs 2 x
eval _ _ (List [Atom "set!"]) = throwError $ NumArgs 2 []
eval env conti (List [Atom "set!", Atom var, form]) = do
        result <- eval env (nullCont env) form >>= setVar env var
        contEval env conti result
eval _ _ (List [Atom "set!", x, _]) = throwError $ BadSpecialForm
                            ("set takes a previously defined variable and "
                            ++ "its new value")
                            x
eval _ _ (List (Atom "set!" : x)) = throwError $ NumArgs 2 x
eval _ _ (List [Atom "set-cdr!"]) = throwError $ NumArgs 2 []
eval env conti (List [Atom "set-cdr!", Atom var, form]) = do
            resolved_var <- eval env (nullCont env) (Atom var)
            resolved_form <- eval env (nullCont env) form
            x <- set_cdr resolved_var resolved_form
            contEval env conti =<< setVar env var x
    where set_cdr (List old) (List new_cdr) = return $ List $ head old : new_cdr
          set_cdr _ _ = return $ Nil "This should never happen"
eval _ _ (List (Atom "set-cdr!" : x)) = throwError $ NumArgs 2 x
eval _ _ (List [Atom "set-car!"]) = throwError $ NumArgs 2 []
eval env conti (List [Atom "set-car!", Atom var, form]) = do
            resolved_var <- eval env (nullCont env) (Atom var)
            resolved_form <- eval env (nullCont env) form
            x <- set_car resolved_var resolved_form
            contEval env conti =<< setVar env var x
    where set_car (List old) new_car = return $ List $ new_car : tail old
          set_car _ _ = return $ Nil "This should never happen"
eval _ _ (List (Atom "set-car!" : x)) = throwError $ NumArgs 2 x
eval _ _ (List [Atom "define"]) = throwError $ NumArgs 2 []
eval env conti (List [Atom "define", Atom var, form]) = do
        result <- eval env (nullCont env) form >>= defineVar env var
        contEval env conti result
eval env conti (List (Atom "define" : List (Atom var : p) : String doc : b)) =  do
        result <- makeDocFunc env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (Atom "define" : List (Atom var : p) : b)) = do
        result <- makeNormalFunc env p b >>= defineVar env var
        contEval env conti result
eval env conti (List (Atom "define" : DottedList (Atom var : p) varargs : String doc : b)) = do
        result <- makeVarargs varargs env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (Atom "define" : DottedList (Atom var : p) varargs : b)) = do
        result <- makeVarargs varargs env p b "No documentation" >>= defineVar env var
        contEval env conti result
eval _ _ (List (Atom "define" : x)) = throwError $ NumArgs 2 x
eval env conti (List (Atom "lambda" : List p : b)) =  do
        result <- makeNormalFunc env p b
        contEval env conti result
eval env conti (List (Atom "λ" : List p : b)) =  do
        result <- makeNormalFunc env p b
        contEval env conti result
eval env conti (List (Atom "lambda" : DottedList p varargs : b)) = do
        result <- makeVarargs varargs env p b "lambda"
        contEval env conti result
eval env conti (List (Atom "λ" : DottedList p varargs : b)) = do
        result <- makeVarargs varargs env p b "lambda"
        contEval env conti result
eval env conti (List (Atom "lambda" : varargs@(Atom _) : b)) = do
        result <- makeVarargs varargs env [] b "lambda"
        contEval env conti result
eval env conti (List (Atom "λ" : varargs@(Atom _) : b)) = do
        result <- makeVarargs varargs env [] b "lambda"
        contEval env conti result
eval _ _ (List [Atom "λ"]) = throwError $ NumArgs 2 []
eval _ _ (List [Atom "lambda"]) = throwError $ NumArgs 2 []
eval _ _ (List (Atom "λ" : x)) = throwError $ NumArgs 2 x
eval _ _ (List (Atom "lambda" : x)) = throwError $ NumArgs 2 x
eval _ _ (List [Atom "load"]) = throwError $ NumArgs 1 []
eval env conti (List [Atom "load", String file]) = do
        filename <- findFile' file
        result <- load filename >>= liftM last . mapM (evl env (nullCont env))
        contEval env conti result
    where evl env2 cont2 val = macroEval env2 val >>= eval env2 cont2
eval _ _ (List [Atom "load", x]) = throwError $ TypeMismatch "string" x
eval _ _ (List (Atom "load" : x)) = throwError $ NumArgs 1 x
eval _ _ (List [Atom "help"]) = throwError $ NumArgs 1 []
eval _ _ (List [Atom "doc"]) = throwError $ NumArgs 1 []
eval _ _ (List [Atom "help", String val]) =
        return $ String $ concat $
        fmap thirdElem (filter filterTuple primitives) ++
        fmap thirdElem (filter filterTuple ioPrimitives)
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval _ _ (List [Atom "doc", String val]) =
        return $ String $ concat $
        fmap thirdElem (filter filterTuple primitives) ++
        fmap thirdElem (filter filterTuple ioPrimitives)
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval env _ (List [Atom "help", Atom val]) = do
        let x = concat $
                fmap thirdElem (filter filterTuple primitives) ++
                fmap thirdElem (filter filterTuple ioPrimitives)
        if x == ""
            then getVar env val
            else return $ String x
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval env _ (List [Atom "doc", Atom val]) = do
        let x = concat $
                fmap thirdElem (filter filterTuple primitives) ++
                fmap thirdElem (filter filterTuple ioPrimitives)
        if x == ""
            then getVar env val
            else return $ String x
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval _ _ (List [Atom "help", x]) = throwError $ TypeMismatch "string/atom" x
eval _ _ (List (Atom "help" : x)) = throwError $ NumArgs 1 x
eval _ _ (List [Atom "doc", x]) = throwError $ TypeMismatch "string/atom" x
eval _ _ (List (Atom "doc" : x)) = throwError $ NumArgs 1 x
eval _ _ (List [Atom "quasiquote"]) = throwError $ NumArgs 1 []
eval env conti (List [Atom "quasiquote", val]) = contEval env conti =<<doUnQuote env val
    where doUnQuote :: Env -> LispVal -> IOThrowsError LispVal
          doUnQuote e v =
            case v of
                List [Atom "unquote", s] -> eval e (nullCont e) s
                List (x : xs) -> liftM List (unquoteListM e (x : xs))
                DottedList xs x -> do
                    rxs <- unquoteListM e xs
                    rx <- doUnQuote e x
                    case rx of
                        List [] -> return $ List rxs
                        List rxlst -> return $ List $ rxs ++ rxlst
                        DottedList rxlst rxlast -> return $ DottedList (rxs ++ rxlst) rxlast
                        _ -> return $ DottedList rxs rx
                Vector vec -> do
                    let len = length (elems vec)
                    vList <- unquoteListM env $ elems vec
                    return $ Vector $ listArray (0, len) vList
                _ -> eval e (nullCont e) (List [Atom "quote", v])
          unquoteListM e = foldlM (unquoteListFld e) []
          unquoteListFld e (acc) v =
            case v of
                List [Atom "unquote-splicing", x] -> do
                    value <- eval e (nullCont e) x
                    case value of
                        List t -> return (acc ++ t)
                        _ -> throwError $ TypeMismatch "proper list" value
                _ -> do result <- doUnQuote env v
                        return (acc ++ [result])
          foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
          foldlM f v (x : xs) = f v x >>= \ a -> foldlM f a xs
          foldlM _ v [] = return v
eval env conti (List [Atom "string-fill!", Atom var, character]) = do
    str <- eval env (nullCont env) =<< getVar env var
    ch <- eval env (nullCont env) character
    result <- eval env (nullCont env) (fillStr(str, ch)) >>= setVar env var
    contEval env conti result
  where fillStr (String str, Character ch) =
            doFillStr (String "", Character ch, length str)
        fillStr (_, _) = Nil "This should never happen"
        doFillStr (String str, Character ch, left) =
            if left == 0
                then String str
                else doFillStr(String $ ch : str, Character ch, left - 1)
        doFillStr (_, _, _) = Nil "This should never happen"
eval env conti (List [Atom "string-set!", Atom var, i, character]) = do
    idx <- eval env (nullCont env) i
    str <- eval env (nullCont env) =<< getVar env var
    result <- eval env (nullCont env) (substr(str, character, idx)) >>= setVar env var
    contEval env conti result
  where substr (String str, Character ch, Number (NumI j)) =
                              String $ (take (fromInteger j) . drop 0) str ++
                                       [ch] ++
                                       (take (length str) . drop (fromInteger j + 1)) str
        substr (_, _, _) = Nil "This should never happen"
eval env conti (List [Atom "vector-set!", Atom var, i, object]) = do
    idx <- eval env (nullCont env) i
    obj <- eval env (nullCont env) object
    vec <- eval env (nullCont env) =<< getVar env var
    result <- eval env (nullCont env) (updateVector vec idx obj) >>= setVar env var
    contEval env conti result
  where updateVector (Vector vec) (Number (NumI idx)) obj = Vector $ vec//[(fromInteger idx, obj)]
        updateVector _ _ _ = Nil "This should never happen"
eval _ _ (List (Atom "vector-set!" : x)) = throwError $ NumArgs 2 x
eval env conti (List [Atom "vector-fill!", Atom var, object]) = do
    obj <- eval env (nullCont env) object
    vec <- eval env (nullCont env) =<< getVar env var
    result <- eval env (nullCont env) (fillVector vec obj) >>= setVar env var
    contEval env conti result
  where fillVector (Vector vec) obj = do
          let l = replicate (lenVector vec) obj
          Vector $ listArray (0, length l - 1) l
        fillVector _ _ = Nil "This should never happen"
        lenVector v = length (elems v)
eval _ _ (List (Atom "vector-fill!" : x)) = throwError $ NumArgs 2 x
eval env conti (List (Atom "begin" : funs))
                        | null funs = eval env conti $ Nil ""
                        | length funs == 1 = eval env conti (head funs)
                        | otherwise = do
                                    let fs = tail funs
                                    _ <- eval env conti (head funs)
                                    eval env conti (List (Atom "begin" : fs))
eval env conti (List (function : args)) = do
        func <- eval env (nullCont env) function
        argVals <- mapM (eval env (nullCont env)) args
        apply conti func argVals
eval _ _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

exitProc :: [LispVal] -> IOThrowsError LispVal
exitProc [] = do _ <- liftIO $ tryIOError $ liftIO exitSuccess
                 return $ Nil ""
exitProc [Number (NumI x)] = do _ <- liftIO $ tryIOError $ liftIO $
                                     exitWith $ ExitFailure $ fromInteger x
                                return $ Nil ""
exitProc [Number (NumS x)] = do _ <- liftIO $ tryIOError $ liftIO $
                                     exitWith $ ExitFailure x
                                return $ Nil ""
exitProc [x] = throwError $ TypeMismatch "integer" x
exitProc badArg = throwError $ NumArgs 1 badArg

colorProc :: [LispVal] -> IOThrowsError LispVal
colorProc [String s] =
        case lookupColor s of
           Just found -> writeProc hPrint [String $ "\x1b[" ++ snd found ++ "m"]
           _          -> throwError $ BadSpecialForm "Color not found: " $ String s
    where lookupColor color = find (\t -> color == fst t) colors
          colors = [ ("black", "30")
                   , ("red", "31")
                   , ("green", "32")
                   , ("yellow", "33")
                   , ("blue", "34")
                   , ("magenta", "35")
                   , ("cyan", "36")
                   , ("white", "37")
                   , ("reset", "0")
                   , ("none", "0")
                   , ("", "0")
                   ]
colorProc [badArg] = throwError $ TypeMismatch "string" badArg
colorProc badArgs = throwError $ NumArgs 1 badArgs

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

writeProc :: (Handle -> LispVal -> IO a) -> [LispVal] -> IOThrowsError LispVal
writeProc fun [obj] = writeProc fun [obj, Port stdout]
writeProc fun [obj, Port port] = do
      out <- liftIO $ tryIOError (liftIO $ fun port obj)
      case out of
          Left _ -> throwError $ Default "IO Error writing to port"
          Right _ -> return $ Nil ""
writeProc _ badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

errorProc :: [LispVal] -> IOThrowsError LispVal
errorProc [obj] = liftIO $ hPrint stderr obj >> return (Bool True)
errorProc badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

load :: String -> IOThrowsError [LispVal]
load filename = do
    res <- liftIO $ doesFileExist filename
    if res
        then liftIO (readFile filename) >>= liftThrows . readExprList
        else throwError $ Default $ "File does not exist: " ++ filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ c@(Cont (Continuation env _ _ _ _)) args =
        if toInteger (length args) /= 1
            then throwError $ NumArgs 1 args
            else contEval env c $ head args
apply _ (IOFunc func) args = func args
apply _ (PrimitiveFunc func) args = liftThrows $ func args
apply conti (EvalFunc fun) args = fun (conti : args)
apply conti (Func (LispFun fparams varargs fbody fclosure _)) args =
        if num fparams /= num args && isNothing varargs
            then throwError $ NumArgs (num fparams) args
            else liftIO (extendEnv fclosure $ zip (fmap ((,) vnamespace) fparams) args) >>= bindVarArgs varargs >>= evalBody fbody
    where
        remainingArgs = drop (length fparams) args
        num = toInteger . length
        evalBody ebody env = case conti of
                                Cont (Continuation _ cBody cCont _ _) -> if null cBody
                                    then continueWithContinuation env ebody cCont
                                    else continueWithContinuation env ebody conti
                                _ -> continueWithContinuation env ebody conti
        continueWithContinuation env cebody continuation =
            contEval env (Cont (Continuation env cebody continuation Nothing Nothing)) $ Nil ""
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ extendEnv env [((vnamespace, argName), List remainingArgs)]
            Nothing -> return env
apply _ func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> String -> m LispVal
makeFunc varargs env p b doc = return $ Func $ LispFun (fmap showVal p) varargs b env doc

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc env p b = makeFunc Nothing env p b "No documentation available"

makeDocFunc :: Env -> [LispVal] -> [LispVal] -> String -> ExceptT LispError IO LispVal
makeDocFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> String -> ExceptT LispError IO LispVal
makeVarargs = makeFunc . Just . showVal
