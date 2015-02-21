module Prompt(runRepl, runSingleStatement) where
import Types
import Primitives
import Variables
import Data.List
import System.IO
import Control.Monad
import System.Console.Haskeline
import Paths_r5rs

keywords :: [String]
keywords = ["apply", "define", "error", "help", "if", "lambda", "let", "display"]

-- | searches all primitives for a possible completion
completionSearch :: String -> [Completion]
completionSearch str = map simpleCompletion $ filter(str `isPrefixOf`) $ 
                       map ("(" ++) keywords 
                       ++ map extractString primitives 
                       ++ map extractString ioPrimitives
                where extractString tuple = "(" ++ firstEl tuple
                      firstEl (x, _, _) = x

-- | returns a fresh settings variable
addSettings :: Settings IO
addSettings = Settings { historyFile = Just ".r5rs_history"
                       , complete = completeWord Nothing " \t" $ return . completionSearch
                       , autoAddHistory = True
                       }

-- | adds primitive bindings to an empty environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++
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
                           "help    - display this help message(use without s-expression)\n" ++
                           "help    - display help for functio" +
                           "if      - branch on condition\n" ++
                           "lambda  - create unnamed function\n" ++
                           "let     - define local variable\n" ++
                           "display - print value to stdout\n" ++
                           "quit    - quit interpreter(use without s-expression)")

-- | the main interpreter loop; gets input and hands everything except help and quit over
until_ :: IO String -> (String -> IO a) -> IO ()
until_ prompt action = do result <- prompt
                          case result of
                            "help"  -> do
                                _ <- printHelp
                                printKeywords
                                until_ prompt action
                            "quit"  -> do
                                putStrLn "\nMoriturus te saluto."
                                return ()
                            _ -> action result >> until_ prompt action

-- | reads from the prompt
readPrompt :: String -> IO String
readPrompt prompt = runInputT addSettings $ poll prompt
                where
                    poll :: String -> InputT IO String
                    poll p = do
                        input <- getInputLine p
                        case input of
                            Nothing -> return "(print \"\")"
                            Just strinput -> return strinput

-- | evaluate a line of code and print it
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalLine env expr >>= putStrLn

-- | run a single statement
runSingleStatement :: [String] -> IO ()
runSingleStatement args = do
        env <- primitiveBindings >>= flip bindVars[((vnamespace, "args"), 
                                                    List $ map String $ drop 1 args)]
        lib <- getDataFileName "stdlib/module.scm"
        _ <- loadFile env lib
        runIOThrows (liftM show $ eval env (List [Atom "load", String $ head args]))
            >>= hPutStrLn stderr
    where loadFile env file = evalLine env $ "(load \"" ++ file ++ "\")"


-- | run the REPL
runRepl :: IO ()
runRepl = do
        env <- primitiveBindings
        lib <- getDataFileName "stdlib/module.scm"
        _ <- loadFile env lib
        until_ (readPrompt "R5RS> ") (evalAndPrint env)
    where loadFile env file = evalLine env $ "(load \"" ++ file ++ "\")"
                          
