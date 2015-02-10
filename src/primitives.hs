module Primitives(primitives, ioPrimitives, eval) where
import Types
import Parser
import System.IO
import Control.Monad.Except

primitives :: [(String, [LispVal] -> ThrowsError LispVal, String)]
primitives = [("+", numericPlusop (+), "add two values"),
              ("-", numericMinop (-), "subtract two values/negate value"),
              ("*", numericBinop (*), "multiply two values"),
              ("/", numericBinop div, "divide two values"),
              ("mod", numericBinop mod, "modulo of two values"),
              ("quotient", numericBinop quot, "quotient of two values"),
              ("remainder", numericBinop rem, "remainder of two values"),
              ("=", numBoolBinop (==), "compare equality of two values"),
              ("<", numBoolBinop (<), "compare equality of two values"),
              (">", numBoolBinop (>), "compare equality of two values"),
              ("/=", numBoolBinop (/=), "compare equality of two values"),
              (">=", numBoolBinop (>=), "compare equality of two values"),
              ("<=", numBoolBinop (<=), "compare equality of two values"),
              ("&&", boolMulop (&&), "and operation"),
              ("and", boolMulop (&&), "and operation"),
              ("||", boolMulop (||), "or operation"),
              ("or", boolMulop (||), "or operation"),
              ("string=?", strBoolBinop (==), "compare equality of two strings"),
              ("string?", strBoolBinop (>), "compare equality of two strings"),
              ("string<?", strBoolBinop (<), "compare equality of two strings"),
              ("string<=?", strBoolBinop (<=), "compare equality of two strings"),
              ("string>=?", strBoolBinop (>=), "compare equality of two strings"),
              ("newline", printNewline, "print a newline"),
              ("car", car, "take head of list"),
              ("cdr", cdr, "take tail of list"),
              ("cons", cons, "construct list"),
              ("eq?", eqv, "check equality"),
              ("eqv?", eqv, "check equality"),
              ("equal?", equal, "check equality")]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
ioPrimitives = [("apply", applyProc, "apply function"),
                ("open-input-file", makePort ReadMode, "open a file for reading"),
                ("open-output-file", makePort WriteMode, "open a file for writing"),
                ("close-input-file", closePort, "close a file opened for reading"),
                ("close-output-file", closePort, "close a file opened for writing"),
                ("read", readProc, "read from file"),
                ("write", writeProc, "write to file"),
                ("read-contents", readContents, "read contents of file"),
                ("read-all", readAll, "read and parse file")]

numericBinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numericMinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericMinop op singleVal@[(Number l)] = return $ Number $ negate l
numericMinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numericPlusop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericPlusop op singleVal@[(Number l)] = if(l > 0) then return $ Number $ l
                                          else return $ Number $ negate l
numericPlusop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

boolMulop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolMulop op params = mapM unpackBool params >>= return . Bool . foldl1 op

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError LispNum
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

printNewline :: [LispVal] -> ThrowsError LispVal
printNewline [] = return $ String $ unlines [""]
printNewline [badArg] = throwError $ TypeMismatch "nothing" badArg
printNewline badArgList = throwError $ NumArgs 1 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                  (and $ map eqvPair $ zip arg1 arg2)
                                  where eqvPair (x, y) = case eqv[x, y] of
                                                            Left err -> False
                                                            Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) =
        do unpacked1 <- unpacker x
           unpacked2 <- unpacker y
           return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] ->ThrowsError LispVal
equal [x, y] = 
        do primitiveEquals <- liftM or $ mapM (unpackEquals x y)
                              [AnyUnpacker unpackNum, AnyUnpacker unpackStr, 
                               AnyUnpacker unpackBool]
           eqvEquals <- eqv [x, y]
           return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do result <- eval env pred
                                                    case result of
                                                        Bool False -> eval env alt
                                                        otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = 
                            makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
                            makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = 
                            makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = 
                            makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = 
                            makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
                            load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "display", String val]) = eval env $ String val
eval env (List [Atom "display", List (function : args)]) = eval env $ List (function : args)
eval env (List [Atom "display", List val]) = eval env  $ List val
eval env (List [Atom "display", Atom val]) = eval env $ Atom val
eval env (List [Atom "display", DottedList(beginning) end]) = return $ String $ showVal $ DottedList beginning end
eval env (List [Atom "display", Number val]) = return $ String $ showVal $ Number val
eval env val@(Atom ident) = getVar env ident
eval env (List (function : args)) = do
                                        func <- eval env function
                                        argVals <- mapM (eval env) args
                                        apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func p varargs b c) args =
    if num p /= num args && varargs == Nothing
        then throwError $ NumArgs (num p) args
        else (liftIO $ bindVars c $ zip p args) >>=
            bindVarArgs varargs >>= evalBody
        where remainingArgs = drop (length p) args
              num = toInteger . length
              evalBody env = liftM last $ mapM (eval env) b
              bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
apply badVal badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ foldl concatenate badVal badArgs 
        where 
            concatenate :: LispVal -> LispVal -> LispVal
            concatenate a b = String $ (showVal a) ++  " " ++ (showVal b)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env p b = return $ Func (map showVal p) varargs b env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeVarargs = makeFunc . Just . showVal
