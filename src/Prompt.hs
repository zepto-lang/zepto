module Prompt(runRepl, runSingleStatement) where
import Types
import Primitives
import Variables
import Data.List
import System.IO
import System.Directory
import Control.Monad
import System.Console.Haskeline
import System.IO.Unsafe
import Paths_zepto

keywords :: [String]
keywords = ["apply", "define", "help", "if", "lambda"]

-- | searches all primitives for a possible completion
completionSearch :: Env -> String -> [Completion]
completionSearch env str = map simpleCompletion $ filter(str `isPrefixOf`) $ 
                       map ("(" ++) (keywords ++ getDefs env)
                where getDefs e = map getAtom $ unsafePerformIO $ recExportsFromEnv e
                      getAtom (Atom a) = a
                      getAtom _ = ""

-- | returns a fresh settings variable
addSettings :: Env -> Settings IO
addSettings env = Settings { historyFile = Just getDir
                           , complete = completeWord Nothing " \t" $ 
                                        return . completionSearch env
                           , autoAddHistory = True
                           }
            where 
                  getDir :: FilePath
                  getDir = unsafePerformIO getHomeDirectory ++ "/.zepto_history"

-- | adds primitive bindings to an empty environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip extendEnv (map (makeFunc IOFunc) ioPrimitives ++
                                map (makeFunc PrimitiveFunc) primitives)
                where makeFunc constructor (var, func, _) = ((vnamespace, var), constructor func)

-- | prints help for all primitives
printHelp :: IO [()]
printHelp = mapM putStrLn $ ["Primitives:"] ++ map getHelp primitives ++ 
                             ["", "IO Primitives:"] ++ map getHelp ioPrimitives ++ [""]
                where getHelp tuple = firstEl tuple ++ " - " ++ thirdEl tuple
                      firstEl (x, _, _) = x
                      thirdEl (_, _, x) = x

-- | prints help for all keywords
printKeywords :: IO ()
printKeywords = putStrLn("Keywords:\n" ++
                           "apply   - apply function to value\n" ++
                           "define  - define global variable\n" ++
                           "error   - print value to stderr\n" ++
                           ":help    - display this help message(use without s-expression)\n" ++
                           "help    - display help for function" ++
                           "if      - branch on condition\n" ++
                           "lambda  - create unnamed function\n" ++
                           "let     - define local variable\n" ++
                           "display - print value to stdout\n" ++
                           "quit    - quit interpreter(use without s-expression)")

-- | the main interpreter loop; gets input and hands everything except help and quit over
until_ :: IO String -> (String -> IO a) -> IO ()
until_ prompt action = do result <- prompt
                          case result of
                            ":help"  -> do
                                _ <- printHelp
                                printKeywords
                                until_ prompt action
                            ":license" -> do
                                printFileContents "stdlib/license_interactive"
                                until_ prompt action
                            ":complete-license" -> do
                                printFileContents "stdlib/complete_license"
                                until_ prompt action
                            ":quit"  -> do
                                putStrLn "\nMoriturus te saluto."
                                return ()
                            _ -> action result >> until_ prompt action
        where printFileContents file = do
                    filename <- getDataFileName file
                    fhandle <- openFile filename ReadMode
                    contents <- hGetContents fhandle
                    putStrLn contents
                    hClose fhandle

-- | reads from the prompt
readPrompt :: String -> Env -> IO String
readPrompt prompt env = runInputT (addSettings env) $ poll prompt
                where
                    poll :: String -> InputT IO String
                    poll p = do
                        input <- getInputLine p
                        case input of
                            Nothing -> return "(print \"\")"
                            Just strinput -> return strinput

-- | evaluate a line of code and print it
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- | run a single statement
runSingleStatement :: [String] -> IO ()
runSingleStatement args = do
        env <- primitiveBindings >>= flip extendEnv[((vnamespace, "args"), 
                                                    List $ map String $ drop 1 args)]
        lib <- getDataFileName "stdlib/module.scm"
        _ <- loadFile env lib
        runIOThrows (liftM show $ eval env (nullCont env) (List [Atom "load", String $ head args]))
            >>= hPutStrLn stderr
    where loadFile env file = evalString env $ "(load \"" ++ file ++ "\")"


-- | run the REPL
runRepl :: IO ()
runRepl = do
        env <- primitiveBindings
        lib <- getDataFileName "stdlib/module.scm"
        _ <- loadFile env lib
        until_ (readPrompt "zepto> " env) (evalAndPrint env)
    where loadFile env file = evalString env $ "(load \"" ++ file ++ "\")"
                          
