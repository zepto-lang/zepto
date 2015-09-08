module Zepto.Primitives(primitives
                       , ioPrimitives
                       , evalPrimitives
                       , eval
                       , versionStr
                       , evalString
                       ) where
import Data.Array
import Data.Maybe
import Control.Monad
import Control.Monad.Except
import System.Directory
import System.IO
import System.IO.Error (tryIOError)
import qualified Data.Map

import Paths_zepto
import Zepto.Primitives.CharStrPrimitives
import Zepto.Primitives.ConversionPrimitives
import Zepto.Primitives.HashPrimitives
import Zepto.Primitives.IOPrimitives
import Zepto.Primitives.ListPrimitives
import Zepto.Primitives.LogMathPrimitives
import Zepto.Primitives.TypeCheckPrimitives
import Zepto.Primitives.VersionPrimitives
import Zepto.Types
import Zepto.Parser
import Zepto.Variables
import Zepto.Macro

-- | a list of all regular primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal, String)]
primitives = [ ("+", numericPlusop (+), "add two or more values")
             , ("-", numericMinop (-), "subtract two or more values/negate value")
             , ("*", numericBinop (*), "multiply two or more values")
             , ("/", numericBinop div, "divide two or more values")
             , ("mod", numericBinop mod, "modulo of two or more values")
             , ("modulo", numericBinop mod, "modulo of two or more values")
             , ("quotient", numericBinop quot, "quotient of two or more values")
             , ("remainder", numericBinop rem, "remainder of two or more values")
             , ("round", numRound round, "rounds a number")
             , ("floor", numRound floor, "floors a number")
             , ("ceiling", numRound ceiling, "ceils a number")
             , ("truncate", numRound truncate, "truncates a number")
             , ("arithmetic-shift", arithmeticShift, "do an arithmetic shift on an integer")
             , ("real", real, "gets real part of a number")
             , ("imaginary", imaginary, "gets imaginary part of a number")
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
             , ("car", car, "take head of list")
             , ("cdr", cdr, "take tail of list")
             , ("cons", cons, "construct list")
             , ("eq?", eqv, "check equality")
             , ("eqv?", eqv, "check equality")
             , ("equal?", equal, "check equality")

             , ("pair?", isDottedList, "check whether arg is a pair")
             , ("procedure?", isProcedure, "check whether arg is a procedure")
             , ("number?", isNumber, "check whether arg is a number")
             , ("integer?", isInteger, "check whether arg is an integer")
             , ("float?", isFloat, "check whether arg is a float")
             , ("small-int?", isSmall, "check whether arg is a small int")
             , ("rational?", isRational, "check whether arg is a rational")
             , ("real?", isReal, "check whether arg is a real number")
             , ("list?", unaryOp isList, "check whether arg is list")
             , ("null?", isNull, "check whether arg is null")
             , ("nil?", isNil, "check whether arg is nil")
             , ("symbol?", isSymbol, "check whether arg is symbol")
             , ("atom?", isAtom, "check whether arg is atom")
             , ("vector?", unaryOp isVector, "check whether arg is vector")
             , ("byte-vector?", unaryOp isByteVector, "check whether arg is bytevector")
             , ("string?", isString, "check whether arg is string")
             , ("port?", isPort, "check whether arg is port")
             , ("input-port?", isInputPort, "check whether arg is input port")
             , ("output-port?", isOutputPort, "check whether arg is output port")
             , ("char?", isChar, "check whether arg is char")
             , ("boolean?", isBoolean, "check whether arg is boolean")
             , ("simple?", isSimple, "check whether arg is of simple type")
             , ("hash-map?", isHash, "check whether arg is a hash-map")
             , ("typeof", checkType, "return type string")
             , ("nil", buildNil, "return nil")
             , ("inf", buildInf, "return inf")
             , ("vector", buildVector, "build a new vector")
             , ("byte-vector", buildByteVector, "build a new bytevector")
             , ("string", buildString, "build a new string")
             , ("char-lower-case", charDowncase, "converts a char to lower case")
             , ("char-upper-case", charUpcase, "converts a char to upper case")
             , ("string-lower-case", stringDowncase, "converts a string to lower case")
             , ("string-upper-case", stringUpcase, "converts a string to upper case")
             , ("vector-length", vectorLength, "get length of vector")
             , ("byte-vector-length", byteVectorLength, "get length of byte vector")
             , ("string-length", stringLength, "get length of string")
             , ("string-substitute", stringSub, "substitute pattern within string by string")
             , ("make-string", makeString, "make a new string")
             , ("make-simple-list", list2Simple, "make a new simple list")
             , ("make-vector", makeVector, "create a vector")
             , ("make-small", makeSmall, "create a small integer")
             , ("make-hash", makeHash, "create a hashmap")
             , ("make-byte-vector", makeByteVector, "create a byte vector")
             , ("char->integer", charToInteger, "makes integer from char")
             , ("vector->list", vectorToList, "makes list from vector")
             , ("list->vector", listToVector, "makes vector from list")
             , ("symbol->string", symbol2String, "makes string from symbol")
             , ("number->string", number2String, "makes string from number")
             , ("string->symbol", string2Symbol, "makes symbol from string")
             , ("string->number", stringToNumber, "makes number from string")
             , ("string->list", stringToList, "makes list from string")
             , ("list->string", listToString, "makes string from list")
             , ("string-copy", stringCopy, "copy string")
             , ("substring", substring, "makes substring from string")
             , ("vector-ref", vectorRef, "get element from vector")
             , ("byte-vector-ref", byteVectorRef, "get element from byte vector")
             , ("string-ref", stringRef, "get element from string")
             , ("string-find", stringFind, "find first occurrence in string")
             , ("string-append", stringExtend, "append to string")
             , ("byte-vector-append", byteVectorAppend, "append to bytevector")
             , ("list-append", listAppend, "append to list")
             , ("vector-append", vectorAppend, "append to vector")
             , ("+=", allAppend, "append to collection")
             , ("string-extend", stringExtend, "extend string")
             , ("list-extend", listExtend, "extend list")
             , ("vector-extend", vectorExtend, "extend vector")
             , ("byte-vector-extend", byteVectorAppend, "extend bytevector")
             , ("++", allExtend, "extend collection")
             , ("hash-keys", hashKeys, "get keys from hashmap")
             , ("hash-values", hashVals, "get vals from hashmap")
             , ("hash-contains?", inHash, "find out whether hashtable contains key")
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
               , ("peek-char", readCharProc hGetChar, "peek char from file")
               , ("read-char", readCharProc hLookAhead, "read char from file")
               , ("write-char", writeCharProc, "write char to file")
               , ("display", writeProc print', "print to stdout")
               , ("error", errorProc, "write to stderr")
               , ("read-contents", readContents, "read contents of file")
               , ("parse", readAll, "read and parse file")
               , ("exit", exitProc, "exit program")
               , ("system", systemProc, "call a system command")
               , ("unix-timestamp", timeProc, "get the unix timestamp as a list where the first element is seconds and the second nanoseconds")
               , ("escape-sequence", escapeProc, "send escape sequence to shell")
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

stringToNumber :: [LispVal] -> ThrowsError LispVal
stringToNumber [SimpleVal (String s)] = do
        result <- readExpr s
        case result of
            n@(SimpleVal (Number _)) -> return n
            _ -> return $ fromSimple $ Bool False
stringToNumber [SimpleVal (String s), SimpleVal (Number base)] =
    case base of
        2 -> stringToNumber [fromSimple $ String $ "#b" ++ s]
        8 -> stringToNumber [fromSimple $ String $ "#o" ++ s]
        10 -> stringToNumber [fromSimple $ String s]
        16 -> stringToNumber [fromSimple $ String $ "#x" ++ s]
        _ -> throwError $ Default $ "Invalid base: " ++ show base
stringToNumber [badType] = throwError $ TypeMismatch "string" badType
stringToNumber badArgList = throwError $ NumArgs 1 badArgList

-- | searches all primitives for a possible completion
evalString :: Env -> String -> IO String
evalString env expr =  runIOThrows $ liftM show $
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
evalFun [c@(Cont (Continuation env _ _ _ _)), val] = eval env c val
evalFun (_ : args) = throwError $ NumArgs 1 args
evalFun _ = throwError $ NumArgs 1 []

evalApply :: [LispVal] -> IOThrowsError LispVal
evalApply [conti@(Cont _), fun, List args] = apply conti fun args
evalApply [_, _,  arg] = throwError $ TypeMismatch "list" arg
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
        fileAsLib <- liftIO $ getDataFileName $ "zepto-stdlib/" ++ filename
        exists <- fex filename
        existsLib <- fex fileAsLib
        case (exists, existsLib) of
            (Bool False, Bool True) -> return fileAsLib
            _ -> return filename
    where
        fex file = do ex <-liftIO $ doesFileExist file
                      return $ Bool ex

filterAndApply :: String -> LispVal -> Maybe LispVal -> Env
                  -> LispVal -> LispVal -> IOThrowsError LispVal
filterAndApply set ret cond env conti x = do
    newenv <- liftIO $ tryIOError $ liftIO $ copyEnv env
    case newenv of
      Right envval -> do
          _ <- defineVar envval set x
          case cond of
            Nothing -> eval envval conti ret
            Just condition -> do
              t <- eval envval conti condition
              case t of
                SimpleVal (Bool True) -> eval envval conti ret
                _ -> return $ fromSimple $ Nil ""
      Left _ -> return $ fromSimple $ Nil ""

internalApply :: String -> LispVal -> Env -> LispVal
                 -> LispVal -> IOThrowsError LispVal
internalApply set ret env conti x = do
    newenv <- liftIO $ tryIOError $ liftIO $ copyEnv env
    case newenv of
      Right envval -> do
          _ <- defineVar envval set x
          eval envval conti ret
      Left _ -> return $ fromSimple $ Nil ""

isNotNil :: LispVal -> Bool
isNotNil (SimpleVal (Nil _)) = False
isNotNil _ = True

-- | evaluates a parsed expression
eval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
eval env conti val@(SimpleVal (Nil _)) = contEval env conti val
eval env conti val@(SimpleVal (String _)) = contEval env conti val
eval env conti val@(SimpleVal (Number _)) = contEval env conti val
eval env conti val@(SimpleVal (Bool _)) = contEval env conti val
eval env conti val@(SimpleVal (Character _)) = contEval env conti val
eval env conti val@(Vector _) = contEval env conti val
eval env conti val@(ByteVector _) = contEval env conti val
eval env conti val@(HashMap _) = contEval env conti val
eval _ _ (List [Vector x, SimpleVal (Number (NumI i))]) = return $ x ! fromIntegral i
eval _ _ (List [Vector x, SimpleVal (Number (NumS i))]) = return $ x ! fromIntegral i
eval _ _ (List [Vector _, wrong@(SimpleVal (Atom (':' : _)))]) =
                     throwError $ TypeMismatch "integer" wrong
eval env conti (List [Vector x, SimpleVal (Atom a)]) = do
                     i <- getVar env a
                     eval env conti (List [Vector x, i])
eval _ _ (List [HashMap x, SimpleVal i@(Atom (':' : _))]) = if Data.Map.member i x
                        then return $ x Data.Map.! i
                        else return $ fromSimple $ Nil ""
eval env conti (List [HashMap x, SimpleVal (Atom a)]) = do
                     i <- getVar env a
                     eval env conti (List [HashMap x, i])
eval _ _ (List [HashMap x, SimpleVal i]) = if Data.Map.member i x
                        then return $ x Data.Map.! i
                        else return $ fromSimple $ Nil ""
eval env conti (HashComprehension (keyexpr, valexpr) (SimpleVal (Atom key), SimpleVal (Atom val)) (SimpleVal (Atom iter)) cond) = do
        hash <- contEval env conti =<< getVar env iter
        case hash of
          HashMap e -> do
            keys <- mapM (filterAndApply key keyexpr cond env conti . fromSimple)
                     (Data.Map.keys e)
            vals <- mapM (internalApply val valexpr env conti) (Data.Map.elems e)
            return $ HashMap $ Data.Map.fromList $ buildTuples (map toSimple keys) vals []
          _ -> throwError $ TypeMismatch "hash-map" hash
    where buildTuples :: [Simple] -> [LispVal] -> [(Simple, LispVal)] -> [(Simple, LispVal)]
          buildTuples [] [] l = l
          buildTuples (ax:al) (bx:bl) x = case ax of
            Nil "" -> buildTuples al bl x
            _      -> buildTuples al bl (x ++ [(ax,bx)])
          buildTuples _ _ _ = error "Hash comprehension failed: internal error while building new hash-map"
eval env conti (HashComprehension (keyexpr, valexpr) (SimpleVal (Atom key), SimpleVal (Atom val)) v@(HashMap _) cond) = do
        hash <- contEval env conti v
        case hash of
          HashMap e -> do
            keys <- mapM (filterAndApply key keyexpr cond env conti . fromSimple)
                     (Data.Map.keys e)
            vals <- mapM (internalApply val valexpr env conti) (Data.Map.elems e)
            return $ HashMap $ Data.Map.fromList $ buildTuples (map toSimple keys) vals []
          _ -> throwError $ TypeMismatch "hash-map" hash
    where buildTuples :: [Simple] -> [LispVal] -> [(Simple, LispVal)] -> [(Simple, LispVal)]
          buildTuples [] [] l = l
          buildTuples (ax:al) (bx:bl) x = case ax of
            Nil "" -> buildTuples al bl x
            _      -> buildTuples al bl (x ++ [(ax,bx)])
          buildTuples _ _ _ = error "Hash comprehension failed: internal error while building new hash-map"
eval env conti (ListComprehension ret (SimpleVal (Atom set)) (SimpleVal (Atom iter)) cond) = do
        list <- contEval env conti =<< getVar env iter
        case list of
          List e -> do
            l <- mapM (filterAndApply set ret cond env conti) e
            return $ List $ filter isNotNil l
          _ -> throwError $ TypeMismatch "list" list
eval env conti (ListComprehension ret (SimpleVal (Atom set)) v@(List (SimpleVal (Atom "quote") : _)) cond) = do
        list <- eval env conti v
        case list of
          List e -> do
            l <-mapM (filterAndApply set ret cond env conti) e
            return $ List $ filter isNotNil l
          _ -> throwError $ TypeMismatch "list" list
eval env conti val@(SimpleVal (Atom (':' : _))) = contEval env conti val
eval env conti (SimpleVal (Atom a)) = contEval env conti =<< getVar env a
eval _ _ (List [List [SimpleVal (Atom "quote"), List x], v@(SimpleVal (Number (NumI i)))]) =
        if length x > fromIntegral i
          then return $ x !! fromIntegral i
          else throwError $ BadSpecialForm "index too large" v
eval _ _ (List [List [SimpleVal (Atom "quote"), List x], v@(SimpleVal (Number (NumS i)))]) =
        if length x > i
          then return $ x !! i
          else throwError $ BadSpecialForm "index too large" v
eval _ _ (List [SimpleVal (Atom "quote")]) = throwError $ NumArgs 1 []
eval env conti (List [SimpleVal (Atom "quote"), val]) = contEval env conti val
eval _ _ (List (SimpleVal (Atom "quote") : x)) = throwError $ NumArgs 1 x
eval _ _ (List [SimpleVal (Atom "if")]) = throwError $ NumArgs 3 []
eval env conti (List [SimpleVal (Atom "if"), p, conseq, alt]) = do
        result <- eval env conti p
        case result of
            SimpleVal (Bool False) -> eval env conti alt
            _                      -> eval env conti conseq
eval env conti (List [SimpleVal (Atom "if"), predicate, conseq]) = do
        result <- eval env conti predicate
        case result of
            SimpleVal (Bool True) -> eval env conti conseq
            _                     -> eval env conti $ fromSimple $ Nil ""
eval _ _ (List [SimpleVal (Atom "if"), x]) = throwError $ BadSpecialForm
                            ("if needs a predicate and a consequence "
                            ++ "plus an optional alternative clause")
                            x
eval _ _ (List (SimpleVal (Atom "if") : x)) = throwError $ NumArgs 2 x
eval _ _ (List [SimpleVal (Atom "set!")]) = throwError $ NumArgs 2 []
eval env conti (List [SimpleVal (Atom "set!"), SimpleVal (Atom var), form]) = do
        result <- eval env (nullCont env) form >>= setVar env var
        contEval env conti result
eval _ _ (List [SimpleVal (Atom "set!"), x, _]) = throwError $ BadSpecialForm
                            ("set takes a previously defined variable and "
                            ++ "its new value")
                            x
eval _ _ (List (SimpleVal (Atom "set!") : x)) = throwError $ NumArgs 2 x
eval _ _ (List [SimpleVal (Atom "set-cdr!")]) = throwError $ NumArgs 2 []
eval env conti (List [SimpleVal (Atom "set-cdr!"), var@(SimpleVal (Atom name)), form]) = do
            resolved_var <- eval env (nullCont env) var
            resolved_form <- eval env (nullCont env) form
            x <- set_cdr resolved_var resolved_form
            contEval env conti =<< setVar env name x
    where set_cdr (List old) (List new_cdr) = return $ List $ head old : new_cdr
          set_cdr _ _ = return $ fromSimple $ Nil "This should never happen"
eval _ _ (List (SimpleVal (Atom "set-cdr!") : x)) = throwError $ NumArgs 2 x
eval _ _ (List [SimpleVal (Atom "set-car!")]) = throwError $ NumArgs 2 []
eval env conti (List [SimpleVal (Atom "set-car!"), var@(SimpleVal (Atom name)), form]) = do
            resolved_var <- eval env (nullCont env) var
            resolved_form <- eval env (nullCont env) form
            x <- set_car resolved_var resolved_form
            contEval env conti =<< setVar env name x
    where set_car (List old) new_car = return $ List $ new_car : tail old
          set_car _ _ = return $ fromSimple $ Nil "This should never happen"
eval _ _ (List (SimpleVal (Atom "set-car!") : x)) = throwError $ NumArgs 2 x
eval _ _ (List [SimpleVal (Atom "define")]) = throwError $ NumArgs 2 []
eval _ _ (List [SimpleVal (Atom "ƒ")]) = throwError $ NumArgs 2 []
eval _ _ (List [SimpleVal (Atom "define"), a@(SimpleVal (Atom (':' : _))), _]) =
            throwError $ TypeMismatch "symbol" a
eval env conti (List [SimpleVal (Atom "define"), SimpleVal (Atom "_"), form]) = do
        _ <- eval env (nullCont env) form
        contEval env conti $ fromSimple $ Nil ""
eval env conti (List [SimpleVal (Atom "define"), SimpleVal (Atom var), form]) = do
        result <- eval env (nullCont env) form >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "define") : List (SimpleVal (Atom var) : p) : SimpleVal (String doc) : b)) =  do
        result <- makeDocFunc env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "define") : List (SimpleVal (Atom var) : p) : b)) = do
        result <- makeNormalFunc env p b >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "define") : DottedList (SimpleVal (Atom var) : p) varargs : SimpleVal (String doc) : b)) = do
        result <- makeVarargs varargs env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "define") : DottedList (SimpleVal (Atom var) : p) varargs : b)) = do
        result <- makeVarargs varargs env p b "No documentation" >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "ƒ") : List (SimpleVal (Atom var) : p) : SimpleVal (String doc) : b)) =  do
        result <- makeDocFunc env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "ƒ") : List (SimpleVal (Atom var) : p) : b)) = do
        result <- makeNormalFunc env p b >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "ƒ") : DottedList (SimpleVal (Atom var) : p) varargs : SimpleVal (String doc) : b)) = do
        result <- makeVarargs varargs env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "ƒ") : DottedList (SimpleVal (Atom var) : p) varargs : b)) = do
        result <- makeVarargs varargs env p b "No documentation" >>= defineVar env var
        contEval env conti result
eval _ _ (List (SimpleVal (Atom "define") : x)) = throwError $ NumArgs 2 x
eval _ _ (List (SimpleVal (Atom "ƒ") : x)) = throwError $ NumArgs 2 x
eval env conti (List (SimpleVal (Atom "lambda") : List p : b)) =  do
        result <- makeNormalFunc env p b
        contEval env conti result
eval env conti (List (SimpleVal (Atom "λ") : List p : b)) =  do
        result <- makeNormalFunc env p b
        contEval env conti result
eval env conti (List (SimpleVal (Atom "lambda") : DottedList p varargs : b)) = do
        result <- makeVarargs varargs env p b "lambda"
        contEval env conti result
eval env conti (List (SimpleVal (Atom "λ") : DottedList p varargs : b)) = do
        result <- makeVarargs varargs env p b "lambda"
        contEval env conti result
eval env conti (List (SimpleVal (Atom "lambda") : varargs@(SimpleVal (Atom _)) : b)) = do
        result <- makeVarargs varargs env [] b "lambda"
        contEval env conti result
eval env conti (List (SimpleVal (Atom "λ") : varargs@(SimpleVal (Atom _)) : b)) = do
        result <- makeVarargs varargs env [] b "lambda"
        contEval env conti result
eval _ _ (List [SimpleVal (Atom "λ")]) = throwError $ NumArgs 2 []
eval _ _ (List [SimpleVal (Atom "lambda")]) = throwError $ NumArgs 2 []
eval _ _ (List (SimpleVal (Atom "λ") : x)) = throwError $ NumArgs 2 x
eval _ _ (List (SimpleVal (Atom "lambda") : x)) = throwError $ NumArgs 2 x
eval _ _ (List [SimpleVal (Atom "global-load")]) = throwError $ NumArgs 1 []
eval env conti (List [SimpleVal (Atom "global-load"), SimpleVal (String file)]) = do
        let glob = globalEnv Nothing env
        filename <- findFile' file
        result <- load filename >>= liftM checkLast . mapM (evl glob (nullCont env))
        contEval env conti result
    where evl env' cont' val = macroEval env' val >>= eval env' cont'
          checkLast [] = fromSimple $ Nil ""
          checkLast [x] = x
          checkLast x = last x
eval _ _ (List [SimpleVal (Atom "global-load"), x]) = throwError $ TypeMismatch "string" x
eval _ _ (List (SimpleVal (Atom "global-load") : x)) = throwError $ NumArgs 1 x
eval _ _ (List [SimpleVal (Atom "load")]) = throwError $ NumArgs 1 []
eval env conti (List [SimpleVal (Atom "load"), SimpleVal (String file)]) = do
        filename <- findFile' file
        result <- load filename >>= liftM checkLast . mapM (evl env (nullCont env))
        contEval env conti result
    where evl env' cont' val = macroEval env' val >>= eval env' cont'
          checkLast [] = fromSimple $ Nil ""
          checkLast [x] = x
          checkLast x = last x
eval _ _ (List [SimpleVal (Atom "load"), x]) = throwError $ TypeMismatch "string" x
eval _ _ (List (SimpleVal (Atom "load") : x)) = throwError $ NumArgs 1 x
eval _ _ (List [SimpleVal (Atom "help")]) = throwError $ NumArgs 1 []
eval _ _ (List [SimpleVal (Atom "doc")]) = throwError $ NumArgs 1 []
eval _ _ (List [SimpleVal (Atom "help"), SimpleVal (String val)]) =
        return $ fromSimple $ String $ concat $
        fmap thirdElem (filter filterTuple primitives) ++
        fmap thirdElem (filter filterTuple ioPrimitives)
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval _ _ (List [SimpleVal (Atom "doc"), SimpleVal (String val)]) =
        return $ fromSimple $ String $ concat $
        fmap thirdElem (filter filterTuple primitives) ++
        fmap thirdElem (filter filterTuple ioPrimitives)
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval env _ (List [SimpleVal (Atom "help"), SimpleVal (Atom val)]) = do
        let x = concat $
                fmap thirdElem (filter filterTuple primitives) ++
                fmap thirdElem (filter filterTuple ioPrimitives)
        if x == ""
            then getVar env val
            else return $ fromSimple $ String x
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval env _ (List [SimpleVal (Atom "doc"), SimpleVal (Atom val)]) = do
        let x = concat $
                fmap thirdElem (filter filterTuple primitives) ++
                fmap thirdElem (filter filterTuple ioPrimitives)
        if x == ""
            then getVar env val
            else return $ fromSimple $ String x
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval _ _ (List [SimpleVal (Atom "help"), x]) = throwError $ TypeMismatch "string/symbol" x
eval _ _ (List (SimpleVal (Atom "help") : x)) = throwError $ NumArgs 1 x
eval _ _ (List [SimpleVal (Atom "doc"), x]) = throwError $ TypeMismatch "string/symbol" x
eval _ _ (List (SimpleVal (Atom "doc") : x)) = throwError $ NumArgs 1 x
eval _ _ (List [SimpleVal (Atom "quasiquote")]) = throwError $ NumArgs 1 []
eval env conti (List [SimpleVal (Atom "quasiquote"), val]) = contEval env conti =<<doUnQuote env val
    where doUnQuote :: Env -> LispVal -> IOThrowsError LispVal
          doUnQuote e v =
            case v of
                List [SimpleVal (Atom "unquote"), s] -> eval e (nullCont e) s
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
                _ -> eval e (nullCont e) (List [SimpleVal (Atom "quote"), v])
          unquoteListM e = foldlM (unquoteListFld e) []
          unquoteListFld e (acc) v =
            case v of
                List [SimpleVal (Atom "unquote-splicing"), x] -> do
                    value <- eval e (nullCont e) x
                    case value of
                        List t -> return (acc ++ t)
                        _ -> throwError $ TypeMismatch "proper list" value
                _ -> do result <- doUnQuote env v
                        return (acc ++ [result])
          foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
          foldlM f v (x : xs) = f v x >>= \ a -> foldlM f a xs
          foldlM _ v [] = return v
eval env conti (List [SimpleVal (Atom "string-fill!"), SimpleVal (Atom var), character]) = do
    str <- eval env (nullCont env) =<< getVar env var
    ch <- eval env (nullCont env) character
    result <- eval env (nullCont env) (fillStr(str, ch)) >>= setVar env var
    contEval env conti result
  where fillStr (SimpleVal (String str), SimpleVal (Character ch)) =
            doFillStr (String "", Character ch, length str)
        fillStr (_, _) = fromSimple $ Nil "This should never happen"
        doFillStr (String str, Character ch, left) =
            if left == 0
                then fromSimple $ String str
                else doFillStr(String $ ch : str, Character ch, left - 1)
        doFillStr (_, _, _) = fromSimple $ Nil "This should never happen"
eval env conti (List [SimpleVal (Atom "string-set!"), SimpleVal (Atom var), i, character]) = do
    idx <- eval env (nullCont env) i
    str <- eval env (nullCont env) =<< getVar env var
    result <- eval env (nullCont env) (substr(str, character, idx)) >>= setVar env var
    contEval env conti result
  where substr (SimpleVal (String str), SimpleVal (Character ch), SimpleVal (Number (NumI j))) =
                              fromSimple . String $ (take (fromInteger j) . drop 0) str ++
                                       [ch] ++
                                       (take (length str) . drop (fromInteger j + 1)) str
        substr (_, _, _) = fromSimple $ Nil "This should never happen"
eval env conti (List [SimpleVal (Atom "vector-set!"), SimpleVal (Atom var), i, object]) = do
    idx <- eval env (nullCont env) i
    obj <- eval env (nullCont env) object
    vec <- eval env (nullCont env) =<< getVar env var
    result <- eval env (nullCont env) (updateVector vec idx obj) >>= setVar env var
    contEval env conti result
  where updateVector (Vector vec) (SimpleVal (Number (NumI idx))) obj = Vector $ vec//[(fromInteger idx, obj)]
        updateVector _ _ _ = fromSimple $ Nil "This should never happen"
eval _ _ (List (SimpleVal (Atom "vector-set!") : x)) = throwError $ NumArgs 2 x
eval env conti (List [SimpleVal (Atom "vector-fill!"), SimpleVal (Atom var), object]) = do
    obj <- eval env (nullCont env) object
    vec <- eval env (nullCont env) =<< getVar env var
    result <- eval env (nullCont env) (fillVector vec obj) >>= setVar env var
    contEval env conti result
  where fillVector (Vector vec) obj = do
          let l = replicate (lenVector vec) obj
          Vector $ listArray (0, length l - 1) l
        fillVector _ _ = fromSimple $ Nil "This should never happen"
        lenVector v = length (elems v)
eval _ _ (List (SimpleVal (Atom "vector-fill!") : x)) = throwError $ NumArgs 2 x
eval env conti (List (SimpleVal (Atom "begin") : funs))
                        | null funs = eval env conti $ SimpleVal (Nil "")
                        | length funs == 1 = eval env conti (head funs)
                        | otherwise = do
                                    let fs = tail funs
                                    _ <- eval env conti (head funs)
                                    eval env conti (List (SimpleVal (Atom "begin") : fs))
eval env conti (List (function : args)) = do
        func <- eval env (nullCont env) function
        argVals <- mapM (eval env (nullCont env)) args
        case func of
          HashMap _ -> eval env conti (List (func : args))
          Vector _  -> eval env conti (List (func : args))
          _         -> apply conti func argVals
eval _ _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [SimpleVal (String filename)] = liftM List $ load filename
readAll badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

load :: String -> IOThrowsError [LispVal]
load filename = do
    res <- liftIO $ doesFileExist filename
    if res
        then liftIO (readFile filename) >>= liftThrows . readExprList
        else throwError $ Default $ "File does not exist: " ++ filename

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [SimpleVal (Atom ":stdin")] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

readCharProc :: (Handle -> IO Char) -> [LispVal] -> IOThrowsError LispVal
readCharProc fun [] = readCharProc fun [Port stdin]
readCharProc fun [Port p] = do
    liftIO $ hSetBuffering p NoBuffering
    input <-  liftIO $ tryIOError (liftIO $ fun p)
    liftIO $ hSetBuffering p LineBuffering
    case input of
        Left _ -> throwError $ Default "IO error while reading from port"
        Right inpChr -> return $ fromSimple $ Character inpChr
readCharProc _ args = if length args == 1
                         then throwError $ TypeMismatch "port" $ List args
                         else throwError $ NumArgs 1 args

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
            contEval env (Cont (Continuation env cebody continuation Nothing Nothing)) $ fromSimple $ Nil ""
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
