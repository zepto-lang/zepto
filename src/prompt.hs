module Prompt(runRepl, runSingleStatement) where
import Types
import Primitives
import Variables
import Data.List
import System.IO
import Control.Monad
import System.Console.Haskeline

completionSearch :: String -> [Completion]
completionSearch str = map simpleCompletion $ filter(str `isPrefixOf`) $ 
                       map extractString primitives ++ map extractString ioPrimitives
                where extractString tuple = "(" ++ firstEl tuple
                      firstEl (x, _, _) = x

addSettings :: Settings IO
addSettings = Settings { historyFile = Just ".r5rs_history"
                       , complete = completeWord Nothing " \t" $ return . completionSearch
                       , autoAddHistory = True
                       }

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++
                                map (makeFunc PrimitiveFunc) primitives)
                where makeFunc constructor (var, func, _) = ((vnamespace, var), constructor func)

printHelp :: IO [()]
printHelp = mapM putStrLn $ ["Primitives:"] ++ map getHelp primitives ++ 
                             ["", "IO Primitives:"] ++ map getHelp ioPrimitives ++ [""]
                where getHelp tuple = firstEl tuple ++ " - " ++ thirdEl tuple
                      firstEl (x, _, _) = x
                      thirdEl (_, _, x) = x

printKeywords :: IO ()
printKeywords = putStrLn("Keywords:\n" ++
                           "apply   - apply function to value\n" ++
                           "define  - define global variable\n" ++
                           "error   - print value to stderr\n" ++
                           "help    - display this help message(use without s-expression)\n" ++
                           "if      - branch on condition\n" ++
                           "lambda  - create unnamed function\n" ++
                           "let     - define local variable\n" ++
                           "display - print value to stdout\n" ++
                           "quit    - quit interpreter(use without s-expression)")

until_ :: (String -> Bool) -> IO String -> (String -> IO a) -> IO ()
until_ predicate prompt action = do result <- prompt
                                    if predicate result
                                    then do
                                        putStrLn "\nMoriturus te saluto."
                                        return ()
                                    else 
                                        if result == "help" then do
                                            _ <- printHelp
                                            printKeywords
                                            until_ predicate prompt action
                                        else action result >> until_ predicate prompt action

readPrompt :: String -> IO String
readPrompt prompt = runInputT addSettings $ poll prompt
                where
                    poll :: String -> InputT IO String
                    poll p = do
                        input <- getInputLine p
                        case input of
                            Nothing -> return "(print \"\")"
                            Just strinput -> return strinput

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalLine env expr >>= putStrLn

runSingleStatement :: [String] -> IO ()
runSingleStatement args = do
    env <- primitiveBindings >>= flip bindVars[((vnamespace, "args"), 
                                                List $ map String $ drop 1 args)]
    _ <- loadFile env "stdlib/module.scm"
    runIOThrows (liftM show $ eval env (List [Atom "load", String $ head args]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
        env <- primitiveBindings
        _ <- loadFile env "stdlib/module.scm"
        until_ (== "quit") (readPrompt "R5RS> ") (evalAndPrint env)
    where loadFile env file = evalLine env $ "(load \"" ++ file ++ "\")"
                          
