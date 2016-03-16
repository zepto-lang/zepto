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
import qualified Control.Exception as CE
import qualified Data.Map
import qualified Data.ByteString as BS (hPut, cons, splitAt, append, tail, index)

import Paths_zepto
import Zepto.Primitives.CharStrPrimitives
import Zepto.Primitives.ConversionPrimitives
import Zepto.Primitives.EnvironmentPrimitives
import Zepto.Primitives.HashPrimitives
import Zepto.Primitives.IOPrimitives
import Zepto.Primitives.ListPrimitives
import Zepto.Primitives.LogMathPrimitives
import Zepto.Primitives.SocketPrimitives
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
             , ("*", numericTimesop (*), "multiply two or more values")
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
             , ("bitwise-and", bitwiseAnd, "do a bitwise and on two integers")
             , ("bitwise-or", bitwiseOr, "do a bitwise or on two integers")
             , ("bitwise-not", bitwiseNot, "do a bitwise or on one integer")
             , ("real", unaryOp real, "gets real part of a number")
             , ("imaginary", unaryOp imaginary, "gets imaginary part of a number")
             , ("expt", numericBinopErr numPow, "power of function")
             , ("pow", numericBinopErr numPow, "power of function")
             , ("^", numericBinopErr numPow, "power of function")
             , ("**", numericBinopErr numPow, "power of function")
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

             , ("inspect", inspect, "inspect source code")

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
             , ("car", unaryOp car, "take head of list")
             , ("cdr", unaryOp cdr, "take tail of list")
             , ("cons", cons, "construct list")
             , ("eq?", eqv, "check equality")
             , ("eqv?", eqv, "check equality")
             , ("equal?", equal, "check equality")

             , ("pair?", unaryOp isDottedList, "check whether arg is a pair")
             , ("procedure?", unaryOp isProcedure, "check whether arg is a procedure")
             , ("number?", unaryOp isNumber, "check whether arg is a number")
             , ("integer?", unaryOp isInteger, "check whether arg is an integer")
             , ("float?", unaryOp isFloat, "check whether arg is a float")
             , ("small-int?", unaryOp isSmall, "check whether arg is a small int")
             , ("rational?", unaryOp isRational, "check whether arg is a rational")
             , ("real?", unaryOp isReal, "check whether arg is a real number")
             , ("list?", unaryOp isList, "check whether arg is list")
             , ("null?", unaryOp isNull, "check whether arg is null")
             , ("nil?", unaryOp isNil, "check whether arg is nil")
             , ("symbol?", unaryOp isSymbol, "check whether arg is symbol")
             , ("atom?", unaryOp isAtom, "check whether arg is atom")
             , ("vector?", unaryOp isVector, "check whether arg is vector")
             , ("byte-vector?", unaryOp isByteVector, "check whether arg is bytevector")
             , ("string?", unaryOp isString, "check whether arg is string")
             , ("port?", unaryOp isPort, "check whether arg is port")
             , ("char?", unaryOp isChar, "check whether arg is char")
             , ("boolean?", unaryOp isBoolean, "check whether arg is boolean")
             , ("simple?", unaryOp isSimple, "check whether arg is of simple type")
             , ("simple-list?", unaryOp isSimpleList, "check whether arg is a simple list")
             , ("hash-map?", unaryOp isHash, "check whether arg is a hash-map")
             , ("typeof", unaryOp checkType, "return type string")
             , ("nil", noArg buildNil, "return nil")
             , ("inf", noArg buildInf, "return inf")
             , ("vector", buildVector, "build a new vector")
             , ("byte-vector", buildByteVector, "build a new bytevector")
             , ("string", buildString, "build a new string")
             , ("char:lower-case", unaryOp charDowncase, "converts a char to lower case")
             , ("char:upper-case", unaryOp charUpcase, "converts a char to upper case")
             , ("string:lower-case", unaryOp stringDowncase, "converts a string to lower case")
             , ("string:upper-case", unaryOp stringUpcase, "converts a string to upper case")
             , ("vector:length", unaryOp vectorLength, "get length of vector")
             , ("byte-vector:length", unaryOp byteVectorLength, "get length of byte vector")
             , ("vector:subvector", subVector, "get subvector from element to element")
             , ("byte-vector:subvector", subByteVector, "get subvector from element to element")
             , ("string:length", unaryOp stringLength, "get length of string")
             , ("string:substitute", stringSub, "substitute pattern within string by string")
             , ("make-string", makeString, "make a new string")
             , ("make-simple-list", unaryOp list2Simple, "make a new simple list")
             , ("from-simple-list", unaryOp simple2List, "make a list from a simple list")
             , ("make-vector", makeVector, "create a vector")
             , ("make-small", makeSmall, "create a small integer")
             , ("make-hash", makeHash, "create a hashmap")
             , ("make-byte-vector", makeByteVector, "create a byte vector")
             , ("char->integer", unaryOp charToInteger, "makes integer from char")
             , ("integer->char", unaryOp integer2Char, "makes char from integer")
             , ("vector->list", unaryOp vectorToList, "makes list from vector")
             , ("list->vector", unaryOp listToVector, "makes vector from list")
             , ("symbol->string", unaryOp symbol2String, "makes string from symbol")
             , ("number->string", number2String, "makes string from number")
             , ("number->bytes", unaryOp number2Bytes, "makes list of bytes from number")
             , ("string->symbol", unaryOp string2Symbol, "makes symbol from string")
             , ("string->number", stringToNumber, "makes number from string")
             , ("string->list", unaryOp stringToList, "makes list from string")
             , ("list->string", unaryOp listToString, "makes string from list")
             , ("byte-vector->string", unaryOp byteVectorToString, "makes string from byte-vector")
             , ("string->byte-vector", unaryOp stringToByteVector, "makes byte-vector from string")
             , ("string:parse", unaryOp stringParse, "parse string")
             , ("substring", substring, "makes substring from string")
             , ("vector:ref", vectorRef, "get element from vector")
             , ("byte-vector:ref", byteVectorRef, "get element from byte vector")
             , ("string:ref", stringRef, "get element from string")
             , ("string:find", stringFind, "find first occurrence in string")
             , ("string:append", stringExtend, "append to string")
             , ("byte-vector:append", byteVectorAppend, "append to bytevector")
             , ("list:append", listAppend, "append to list")
             , ("vector:append", vectorAppend, "append to vector")
             , ("+=", allAppend, "append to collection")
             , ("string:extend", stringExtend, "extend string")
             , ("list:extend", listExtend, "extend list")
             , ("vector:extend", vectorExtend, "extend vector")
             , ("byte-vector:extend", byteVectorAppend, "extend bytevector")
             , ("++", allExtend, "extend collection")
             , ("hash:keys", hashKeys, "get keys from hashmap")
             , ("hash:values", hashVals, "get vals from hashmap")
             , ("hash:contains?", inHash, "find out whether hashtable contains key")
             , ("zepto:version", noArg getVersion, "gets the version as a list")
             , ("zepto:version-str", noArg getVersionStr, "gets the version as a string")
             , ("zepto:major-version", noArg getMajVersion, "gets the major version number")
             , ("zepto:minor-version", noArg getMinVersion, "gets the minor version number")
             , ("zepto:patch-version", noArg getPatchVersion, "gets the patch version number")
             ]

-- | a list of all io-bound primitives
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
ioPrimitives = [ ("open-input-file", makePort ReadMode, "open a file for reading")
               , ("open-output-file", makePort WriteMode, "open a file for writing")
               , ("close-input-file", closePort, "close a file opened for reading")
               , ("close-output-file", closePort, "close a file opened for writing")
               , ("input-port?", unaryIOOp isInputPort, "check whether arg is input port")
               , ("output-port?", unaryIOOp isOutputPort, "check whether arg is output port")
               , ("get-home-dir", getHomeDir, "get the home directory")
               , ("zepto:home", getZeptoDir, "get the home directory")
               , ("read", readProc, "read from file")
               , ("write", writeProc printInternal, "write to file")
               , ("read-char", readCharProc hGetChar, "peek char from file")
               , ("peek-char", readCharProc hLookAhead, "read char from file")
               , ("write-char", writeCharProc, "write char to file")
               , ("display", writeProc print', "print to stdout")
               , ("read-contents", readContents, "read contents of file")
               , ("read-contents-binary", readBinaryContents, "read contents of file into bytevector")
               , ("parse", readAll, "read and parse file")
               , ("exit", exitProc, "exit program")
               , ("system", systemProc, "call a system command")
               , ("unix-timestamp", noIOArg timeProc, "get the unix timestamp as a list where the first element is seconds and the second nanoseconds")
               , ("escape-sequence", escapeProc, "send escape sequence to shell")
               , ("color", colorProc, "colorize output")
               , ("make-null-env", makeNullEnv, "make empty environment")
               , ("make-base-env", makeBaseEnv, "make standard environment")
               , ("env->hashmap", unaryIOOp env2HashMap, "makes hash-map from local binding of an env")

               , ("net:socket", socket, "opens a socket")
               , ("net:get-addr-info", getAddrInfo, "create an address info object")
               , ("net:connect", connect, "connect a socket to an address")
               , ("net:recv", recv, "receive data from a connected socket")
               , ("net:send", send, "send data to a connected socket")
               , ("net:bind-socket", bindSocket, "bind a socket to a specific address")
               , ("net:listen", listen, "listen on a bound socket")
               , ("net:accept", accept, "accept connection to a bound socket")
               , ("net:close-socket", close, "closes a socket; all future operations on thsi socket will fail")
               ]

evalPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
evalPrimitives = [ ("eval", evalFun, "evaluate list")
                 , ("macro-expand", macroEvalFun, "treat list as code and expand the macros")
                 , ("apply", evalApply, "apply function to values")
                 , ("call-with-current-continuation", evalCallCC, "call with current continuation")
                 , ("call/cc", evalCallCC, "call with current continuation")
                 , ("catch-vm-error", catchVMError, "catches any vm error")
                 --, ("call-with-values", evalCallWValues, "call with values"),
                 --, ("load-ffi", evalFFI, "load foreign function")
                 ]

printInternal :: Handle -> LispVal -> IO ()
printInternal handle val =
    case val of
      ByteVector x -> BS.hPut handle x
      _            -> hPrint handle val

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
contEval _ (Cont (Continuation cEnv cBody cCont Nothing Nothing _)) val =
    case cBody of
        [] ->
            case cCont of
                Cont (Continuation nEnv _ _ _ _ _) -> contEval nEnv cCont val
                _ -> return val
        [lval] -> eval cEnv (Cont (Continuation cEnv [] cCont Nothing Nothing [])) lval
        (lval : lvals) -> eval cEnv (Cont (Continuation cEnv lvals cCont Nothing Nothing [])) lval
contEval _ _ _ = throwError $ InternalError "This should never happen"

stringParse :: LispVal -> ThrowsError LispVal
stringParse (SimpleVal (String x)) = readExpr x
stringParse x = throwError $ TypeMismatch "string" x

makeBaseEnv :: [LispVal] -> IOThrowsError LispVal
makeBaseEnv [] = do
    env <- liftIO primitiveBindings
    return $ Environ env
  where
    primitiveBindings = nullEnv >>= flip extendEnv (fmap (makeBind IOFunc) ioPrimitives ++
                                  fmap (makeBind PrimitiveFunc) primitives ++
                                  fmap (makeBind EvalFunc) evalPrimitives)
                  where makeBind constructor (var, func, _) = ((vnamespace, var), constructor var func)
makeBaseEnv args = throwError $ NumArgs 0 args

macroEvalFun :: [LispVal] -> IOThrowsError LispVal
macroEvalFun [(Cont (Continuation env _ _ _ _ _)), val] = macroEval env val
macroEvalFun [(Cont _), val, Environ env] = macroEval env val
macroEvalFun (_ : args) = throwError $ NumArgs 1 args
macroEvalFun _ = throwError $ NumArgs 1 []

evalFun :: [LispVal] -> IOThrowsError LispVal
evalFun [c@(Cont (Continuation env _ _ _ _ _)), val] = eval env c val
evalFun [c@(Cont _), val, Environ env] = eval env c val
evalFun (_ : args) = throwError $ NumArgs 1 args
evalFun _ = throwError $ NumArgs 1 []

evalApply :: [LispVal] -> IOThrowsError LispVal
evalApply [conti@(Cont _), fun, List args] = apply conti fun args
evalApply (conti@(Cont _) : fun : args) = apply conti fun args
evalApply [_, _, arg] = throwError $ TypeMismatch "list" arg
evalApply (_ : args) = throwError $ NumArgs 2 args
evalApply _ = throwError $ NumArgs 2 []

evalCallCC :: [LispVal] -> IOThrowsError LispVal
evalCallCC [conti@(Cont _), fun] =
        case fun of
            Cont _ -> apply conti fun [conti]
            PrimitiveFunc _ f -> do
                result <- liftThrows $ f [conti]
                case conti of
                    Cont (Continuation cEnv _ _ _ _ _) -> contEval cEnv conti result
                    _ -> return result
            Func _ (LispFun _ (Just _) _ _ _) -> apply conti fun [conti]
            Func _ (LispFun aparams _ _ _ _) ->
                if length aparams == 1
                    then apply conti fun [conti]
                    else throwError $ NumArgs (toInteger $ length aparams) [conti]
            other -> throwError $ TypeMismatch "procedure" other
evalCallCC (_ : args) = throwError $ NumArgs 1 args
evalCallCC x = throwError $ NumArgs 1 x

catchVMError :: [LispVal] -> IOThrowsError LispVal
catchVMError [c, x, Environ env] = do
          str <- liftIO $ CE.catch (runIOThrows $ liftM show $ eval env c x) handler
          return $ fromSimple $ String str
    where handler :: CE.SomeException -> IO String
          handler msg@(CE.SomeException _) = return $ show (msg::CE.SomeException)
catchVMError [c@(Cont (Continuation env _ _ _ _ _)), x] = catchVMError [c, x, Environ env]
catchVMError [x, _] = throwError $ TypeMismatch "continuation" x
catchVMError x = throwError $ NumArgs 1 (tail x)

findFile' :: String -> ExceptT LispError IO String
findFile' filename = do
        let expanded = expand filename
        fileAsLib <- liftIO $ getDataFileName $ "zepto-stdlib/" ++ filename
        let fileAsLibExpanded = expand fileAsLib
        exists <- fex filename
        existsExpanded <- fex expanded
        existsLib <- fex fileAsLib
        existsLibExpanded <- fex fileAsLibExpanded
        case (exists, existsExpanded, existsLib, existsLibExpanded) of
            (Bool False, Bool False, Bool False, Bool True) -> return fileAsLibExpanded
            (Bool False, Bool False, Bool True, _)          -> return fileAsLib
            (Bool False, Bool True, _, _)                   -> return expanded
            _                                               -> return filename
    where
        expand x = x ++ ".zp"
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

stringifyFunction :: LispVal -> String
stringifyFunction (Func name (LispFun {params = args, vararg = varargs, body = _,
                                       closure = _, docstring = doc})) =
    doc ++ "\n  source: " ++
    "(" ++ name ++ " (" ++ unwords (fmap show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
stringifyFunction _ = ""

-- | evaluates a parsed expression
eval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
eval env conti val@(SimpleVal (Nil _)) = contEval env conti val
eval env conti val@(SimpleVal (String _)) = contEval env conti val
eval env conti val@(SimpleVal (Number _)) = contEval env conti val
eval env conti val@(SimpleVal (Bool _)) = contEval env conti val
eval env conti val@(SimpleVal (Character _)) = contEval env conti val
eval env conti val@(Vector _) = contEval env conti val
eval env conti val@(Func _ _) = contEval env conti val
eval env conti val@(IOFunc _ _) = contEval env conti val
eval env conti val@(EvalFunc _ _) = contEval env conti val
eval env conti val@(PrimitiveFunc _ _) = contEval env conti val
eval env conti val@(ByteVector _) = contEval env conti val
eval env conti val@(HashMap _) = contEval env conti val
eval env conti val@(Environ _) = contEval env conti val
eval _ _ (List [Vector x, SimpleVal (Number (NumI i))]) = return $ x ! fromIntegral i
eval _ _ (List [Vector x, SimpleVal (Number (NumS i))]) = return $ x ! fromIntegral i
eval _ _ (List [Vector _, wrong@(SimpleVal (Atom (':' : _)))]) =
        throwError $ TypeMismatch "integer" wrong
eval env conti (List [Vector x, SimpleVal (Atom a)]) = do
        i <- getVar env a
        eval env conti (List [Vector x, i])
eval _ _ (List [Vector _, x]) = throwError $ TypeMismatch "integer" x
eval _ _ (List [ByteVector x, SimpleVal (Number (NumI i))]) =
        return $ fromSimple $ Number $ NumS $ fromIntegral $ BS.index x (fromIntegral i)
eval _ _ (List [ByteVector x, SimpleVal (Number (NumS i))]) =
        return $ fromSimple $ Number $ NumS $ fromIntegral $ BS.index x (fromIntegral i)
eval _ _ (List [ByteVector _, wrong@(SimpleVal (Atom (':' : _)))]) =
        throwError $ TypeMismatch "integer" wrong
eval env conti (List [ByteVector x, SimpleVal (Atom a)]) = do
        i <- getVar env a
        eval env conti (List [ByteVector x, i])
eval _ _ (List [ByteVector _, x]) = throwError $ TypeMismatch "integer" x
eval _ _ (List [HashMap x, SimpleVal i@(Atom (':' : _))]) = if Data.Map.member i x
        then return $ x Data.Map.! i
        else return $ fromSimple $ Nil ""
eval env conti (List [HashMap x, SimpleVal (Atom a)]) = do
        i <- getVar env a
        eval env conti (List [HashMap x, i])
eval _ _ (List [HashMap x, SimpleVal i]) =
        if Data.Map.member i x
          then return $ x Data.Map.! i
          else return $ fromSimple $ Nil ""
eval env conti (List [HashMap x, form]) = do
        i <- eval env conti form
        eval env conti (List [HashMap x, i])
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
eval env conti (List [SimpleVal (Atom "set-cdr!"), sth, form]) = do
            resolved <- eval env (nullCont env) sth
            if matches resolved
              then do
                let var = fst $ unpack resolved
                let name = snd $ unpack resolved
                resolved_var <- eval env (nullCont env) var
                resolved_form <- eval env (nullCont env) form
                x <- set_cdr resolved_var resolved_form
                contEval env conti =<< setVar env name x
              else throwError $ TypeMismatch "symbol" resolved
    where set_cdr (List old) (List new_cdr) = return $ List $ head old : new_cdr
          set_cdr _ _ = return $ fromSimple $ Nil "This should never happen"
          matches (SimpleVal (Atom _)) = True
          matches _ = False
          unpack var@(SimpleVal (Atom name)) = (var, name)
          unpack _ = (fromSimple $ Nil "", "")
eval _ _ (List (SimpleVal (Atom "set-cdr!") : x)) = throwError $ NumArgs 2 x
eval _ _ (List [SimpleVal (Atom "set-car!")]) = throwError $ NumArgs 2 []
eval env conti (List [SimpleVal (Atom "set-car!"), var@(SimpleVal (Atom name)), form]) = do
            resolved_var <- eval env (nullCont env) var
            resolved_form <- eval env (nullCont env) form
            x <- set_car resolved_var resolved_form
            contEval env conti =<< setVar env name x
    where set_car (List old) new_car = return $ List $ new_car : tail old
          set_car _ _ = return $ fromSimple $ Nil "This should never happen"
eval env conti (List [SimpleVal (Atom "set-car!"), sth, form]) = do
            resolved <- eval env (nullCont env) sth
            if matches resolved
              then do
                let var = fst $ unpack resolved
                let name = snd $ unpack resolved
                resolved_var <- eval env (nullCont env) var
                resolved_form <- eval env (nullCont env) form
                x <- set_car resolved_var resolved_form
                contEval env conti =<< setVar env name x
              else throwError $ TypeMismatch "symbol" resolved
    where set_car (List old) new_car = return $ List $ new_car : tail old
          set_car _ _ = return $ fromSimple $ Nil "This should never happen"
          matches (SimpleVal (Atom _)) = True
          matches _ = False
          unpack var@(SimpleVal (Atom name)) = (var, name)
          unpack _ = (fromSimple $ Nil "", "")
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
        result <- makeDocFunc var env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "define") : List (SimpleVal (Atom var) : p) : b)) = do
        result <- makeNormalFunc var env p b >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "define") : DottedList (SimpleVal (Atom var) : p) varargs : SimpleVal (String doc) : b)) = do
        result <- makeVarargs var varargs env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "define") : DottedList (SimpleVal (Atom var) : p) varargs : b)) = do
        result <- makeVarargs var varargs env p b "No documentation" >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "ƒ") : List (SimpleVal (Atom var) : p) : SimpleVal (String doc) : b)) =  do
        result <- makeDocFunc var env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "ƒ") : List (SimpleVal (Atom var) : p) : b)) = do
        result <- makeNormalFunc var env p b >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "ƒ") : DottedList (SimpleVal (Atom var) : p) varargs : SimpleVal (String doc) : b)) = do
        result <- makeVarargs var varargs env p b doc >>= defineVar env var
        contEval env conti result
eval env conti (List (SimpleVal (Atom "ƒ") : DottedList (SimpleVal (Atom var) : p) varargs : b)) = do
        result <- makeVarargs var varargs env p b "No documentation" >>= defineVar env var
        contEval env conti result
eval _ _ (List (SimpleVal (Atom "define") : x)) = throwError $ NumArgs 2 x
eval _ _ (List (SimpleVal (Atom "ƒ") : x)) = throwError $ NumArgs 2 x
eval env conti (List (SimpleVal (Atom "lambda") : List p : b)) =  do
        result <- makeNormalFunc "lambda" env p b
        contEval env conti result
eval env conti (List (SimpleVal (Atom "λ") : List p : b)) =  do
        result <- makeNormalFunc "lambda" env p b
        contEval env conti result
eval env conti (List (SimpleVal (Atom "lambda") : DottedList p varargs : b)) = do
        result <- makeVarargs "lambda" varargs env p b "lambda"
        contEval env conti result
eval env conti (List (SimpleVal (Atom "λ") : DottedList p varargs : b)) = do
        result <- makeVarargs "lambda" varargs env p b "lambda"
        contEval env conti result
eval env conti (List (SimpleVal (Atom "lambda") : varargs@(SimpleVal (Atom _)) : b)) = do
        result <- makeVarargs "lambda" varargs env [] b "lambda"
        contEval env conti result
eval env conti (List (SimpleVal (Atom "λ") : varargs@(SimpleVal (Atom _)) : b)) = do
        result <- makeVarargs "lambda" varargs env [] b "lambda"
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
eval _ _ (List [SimpleVal (Atom "help")]) = throwError $ NumArgs 1 []
eval _ _ (List [SimpleVal (Atom "doc")]) = throwError $ NumArgs 1 []
eval env _ (List [SimpleVal (Atom "help"), SimpleVal (String val)]) = do
        let x = concat $
                fmap thirdElem (filter filterTuple primitives) ++
                fmap thirdElem (filter filterTuple ioPrimitives)
        if x == ""
            then do
              var <- getVar env val
              case var of
                f@(Func _ _) -> return $ fromSimple $ String $ stringifyFunction f
                IOFunc doc _ -> return $ fromSimple $ String $ doc
                PrimitiveFunc doc _ -> return $ fromSimple $ String $ doc
                EvalFunc doc _ -> return $ fromSimple $ String $ doc
                _ -> throwError $ Default $ val ++ " is not a function"
            else return $ fromSimple $ String x
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval env _ (List [SimpleVal (Atom "doc"), SimpleVal (String val)]) = do
        let x = concat $
                fmap thirdElem (filter filterTuple primitives) ++
                fmap thirdElem (filter filterTuple ioPrimitives)
        if x == ""
            then do
              var <- getVar env val
              case var of
                f@(Func _ _) -> return $ fromSimple $ String $ stringifyFunction f
                IOFunc doc _ -> return $ fromSimple $ String $ doc
                PrimitiveFunc doc _ -> return $ fromSimple $ String $ doc
                EvalFunc doc _ -> return $ fromSimple $ String $ doc
                _ -> throwError $ Default $ val ++ " is not a function"
            else return $ fromSimple $ String x
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval env conti (List [SimpleVal (Atom "help"), SimpleVal (Atom val)]) = do
        let x = concat $
                fmap thirdElem (filter filterTuple primitives) ++
                fmap thirdElem (filter filterTuple ioPrimitives)
        if x == ""
            then do
              var <- getVar env val
              case var of
                f@(Func _ _) -> return $ fromSimple $ String $ stringifyFunction f
                IOFunc doc _ -> return $ fromSimple $ String $ doc
                PrimitiveFunc doc _ -> return $ fromSimple $ String $ doc
                EvalFunc doc _ -> return $ fromSimple $ String $ doc
                f@(SimpleVal (Atom _)) -> eval env conti (List [SimpleVal (Atom "help"), f])
                err -> throwError $ Default $ val ++ " is not a function (is: " ++ typeString err ++ ")"
            else return $ fromSimple $ String x
    where
          filterTuple tuple = (== val) $ firstElem tuple
          firstElem (x, _, _) = x
          thirdElem (_, _, x) = x
eval env conti (List [SimpleVal (Atom "doc"), SimpleVal (Atom val)]) = do
        let x = concat $
                fmap thirdElem (filter filterTuple primitives) ++
                fmap thirdElem (filter filterTuple ioPrimitives)
        if x == ""
            then do
              var <- getVar env val
              case var of
                f@(Func _ _) -> return $ fromSimple $ String $ stringifyFunction f
                IOFunc doc _ -> return $ fromSimple $ String $ doc
                PrimitiveFunc doc _ -> return $ fromSimple $ String $ doc
                EvalFunc doc _ -> return $ fromSimple $ String $ doc
                f@(SimpleVal (Atom _)) -> eval env conti (List [SimpleVal (Atom "help"), f])
                _ -> throwError $ Default $ val ++ " is not a function"
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
eval env conti (List [SimpleVal (Atom "string:fill!"), SimpleVal (Atom var), character]) = do
    str <- eval env (nullCont env) =<< getVar env var
    ch <- eval env (nullCont env) character
    case ch of
      (SimpleVal (Character _)) -> do
        result <- eval env (nullCont env) (fillStr(str, ch)) >>= setVar env var
        contEval env conti result
      x -> throwError $ TypeMismatch "character" x
  where fillStr (SimpleVal (String str), SimpleVal (Character ch)) =
            doFillStr (String "", Character ch, length str)
        fillStr (_, _) = fromSimple $ Nil "This should never happen"
        doFillStr (String str, Character ch, left) =
            if left == 0
                then fromSimple $ String str
                else doFillStr(String $ ch : str, Character ch, left - 1)
        doFillStr (_, _, _) = fromSimple $ Nil "This should never happen"
eval env conti (List [SimpleVal (Atom "string:set!"), SimpleVal (Atom var), i, character]) = do
    idx <- eval env (nullCont env) i
    str <- eval env (nullCont env) =<< getVar env var
    case str of
      (SimpleVal (String _)) -> do
          result <- eval env (nullCont env) (substr(str, character, idx)) >>= setVar env var
          contEval env conti result
      x -> throwError $ TypeMismatch "string" x
  where substr (SimpleVal (String str), SimpleVal (Character ch), SimpleVal (Number (NumI j))) =
                              fromSimple . String $ (take (fromInteger j) . drop 0) str ++
                                       [ch] ++
                                       (take (length str) . drop (fromInteger j + 1)) str
        substr (_, _, _) = fromSimple $ Nil "This should never happen"
eval env conti (List [SimpleVal (Atom "vector:set!"), SimpleVal (Atom var), i, object]) = do
    idx <- eval env (nullCont env) i
    obj <- eval env (nullCont env) object
    vec <- eval env (nullCont env) =<< getVar env var
    case vec of
      Vector _ -> do result <- eval env (nullCont env) (updateVector vec idx obj) >>= setVar env var
                     contEval env conti result
      x -> throwError $ TypeMismatch "vector" x
  where updateVector (Vector vec) (SimpleVal (Number (NumI idx))) obj = Vector $ vec//[(fromInteger idx, obj)]
        updateVector _ _ _ = fromSimple $ Nil "This should never happen"
eval _ _ (List (SimpleVal (Atom "vector:set!") : x)) = throwError $ NumArgs 3 x
eval env conti (List [SimpleVal (Atom "byte-vector:set!"), SimpleVal (Atom var), i, object]) = do
    idx <- eval env (nullCont env) i
    obj <- eval env (nullCont env) object
    case obj of
      (SimpleVal (Number (NumS _))) -> do
        vec <- eval env (nullCont env) =<< getVar env var
        case vec of
          ByteVector _ -> do
            result <- eval env (nullCont env) (updateBVector vec idx obj) >>= setVar env var
            contEval env conti result
          x -> throwError $ TypeMismatch "byte-vector" x
      x -> throwError $ TypeMismatch "small int" x
  where updateBVector (ByteVector vec) (SimpleVal (Number (NumI idx))) (SimpleVal (Number (NumS obj))) =
            let (t, d) = BS.splitAt (fromInteger idx) vec
            in ByteVector $ BS.append t (BS.cons (fromIntegral obj) (BS.tail d))
        updateBVector _ _ _ = fromSimple $ Nil ""
eval _ _ (List (SimpleVal (Atom "byte-vector:set!") : x)) = throwError $ NumArgs 3 x
eval env conti (List [SimpleVal (Atom "vector:fill!"), SimpleVal (Atom var), object]) = do
    obj <- eval env (nullCont env) object
    vec <- eval env (nullCont env) =<< getVar env var
    case vec of
      Vector _ -> do result <- eval env (nullCont env) (fillVector vec obj) >>= setVar env var
                     contEval env conti result
      x -> throwError $ TypeMismatch "vector" x
  where fillVector (Vector vec) obj = do
          let l = replicate (lenVector vec) obj
          Vector $ listArray (0, length l - 1) l
        fillVector _ _ = fromSimple $ Nil "This should never happen"
        lenVector v = length (elems v)
eval _ _ (List (SimpleVal (Atom "vector:fill!") : x)) = throwError $ NumArgs 2 x
eval env conti (List (SimpleVal (Atom "begin") : funs))
                        | null funs = eval env conti $ SimpleVal (Nil "")
                        | length funs == 1 = eval env conti (head funs)
                        | otherwise = do
                                    let fs = tail funs
                                    _ <- eval env conti (head funs)
                                    eval env conti (List (SimpleVal (Atom "begin") : fs))
eval env _ (List [SimpleVal (Atom "current-env")]) = return $ Environ env
eval _ _ (List ((SimpleVal (Atom "current-env")) : x)) = throwError $ NumArgs 0 x
eval env conti (List (function : args)) = do
        func <- eval env (nullCont env) function
        argVals <- mapM (eval env (nullCont env)) args
        case func of
          HashMap _ -> eval env conti (List (func : args))
          Vector _  -> eval env conti (List (func : args))
          ByteVector _  -> eval env conti (List (func : args))
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
readProc [x@(SimpleVal (Atom ":no-eval"))] = readProc [Port stdin, x]
readProc [SimpleVal (Atom ":stdin"), x@(SimpleVal (Atom ":no-eval"))] = readProc [Port stdin, x]
readProc [Port port, SimpleVal (Atom ":no-eval")] = liftIO (hGetLine port) >>= return . fromSimple . String
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
apply (Cont (Continuation a b c d e cs)) fn args =
  apply' (Cont (Continuation a b c d e $! (buildCallHistory (fn, showArgs args) cs))) fn args
apply _ func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)

apply' :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply' _ c@(Cont (Continuation env _ _ _ _ _)) args =
        if toInteger (length args) /= 1
            then throwError $ NumArgs 1 args
            else contEval env c $ head args
apply' (Cont (Continuation _ _ _ _ _ cs)) (IOFunc _ func) args =
        catchError (func args) (throwHistorial cs)
apply' (Cont (Continuation _ _ _ _ _ cs))  (PrimitiveFunc _ func) args =
        catchError (liftThrows $ func args) (throwHistorial cs)
apply' conti@(Cont (Continuation _ _ _ _ _ cs)) (EvalFunc _ fun) args =
        catchError (fun (conti : args)) (throwHistorial cs)
apply' conti@(Cont (Continuation _ _ _ _ _ cs)) (Func _ (LispFun fparams varargs fbody fclosure _)) args =
        if num fparams /= num args && isNothing varargs
            then throwError $ NumArgs (num fparams) args
            else liftIO (extendEnv fclosure $ zip (fmap ((,) vnamespace) fparams) args)
                  >>= bindVarArgs varargs
                  >>= evalBody fbody
    where
        remainingArgs = drop (length fparams) args
        num = toInteger . length
        evalBody ebody env = case conti of
                                Cont (Continuation _ cBody cCont _ _ _) -> if null cBody
                                    then continueWithContinuation env ebody cCont
                                    else continueWithContinuation env ebody conti
                                _ -> continueWithContinuation env ebody conti
        continueWithContinuation env cebody continuation =
            catchError
              (contEval env (Cont (Continuation env cebody continuation Nothing Nothing [])) $ fromSimple $ Nil "")
              (throwHistorial cs)
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ extendEnv env [((vnamespace, argName), List remainingArgs)]
            Nothing -> return env
apply' _ func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)

makeFunc :: Monad m => String -> Maybe String -> Env -> [LispVal] -> [LispVal] -> String -> m LispVal
makeFunc name varargs env p b doc = return $ Func name $ LispFun (fmap showVal p) varargs b env doc

makeNormalFunc :: String -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc name env p b = makeFunc name Nothing env p b "No documentation available"

makeDocFunc :: String -> Env -> [LispVal] -> [LispVal] -> String -> ExceptT LispError IO LispVal
makeDocFunc name = makeFunc name Nothing

makeVarargs :: String -> LispVal -> Env -> [LispVal] -> [LispVal] -> String -> ExceptT LispError IO LispVal
makeVarargs name = makeFunc name . Just . showVal

