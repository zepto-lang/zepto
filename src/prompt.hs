module Prompt(runRepl, runSingleStatement) where
import Types
import Parser
import Primitives
import Data.List
import System.IO
import Control.Monad
import System.Console.Haskeline
import System.Console.Haskeline.History

completion = ["print", "error", "lambda", "quote", "define", "if", "set!", 
              "apply", "open-input-file", "open-output-file", 
              "close-input-file", "close-output-file", "read", "write",
              "read-contents", "read-all", "let"]

completionSearch :: String -> [Completion]
completionSearch str = map simpleCompletion $ filter(str `isPrefixOf`) $ map("(" ++) completion

addSettings :: Settings IO
addSettings = Settings { historyFile = Just ".r5rs_history"
                       , complete = completeWord Nothing " \t" $ return . completionSearch
                       , autoAddHistory = True
                       }

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++
                                map (makeFunc PrimitiveFunc) primitives)
                where makeFunc constructor (var, func) = (var, constructor func)

printHelp :: IO ()
printHelp = putStrLn("apply  - apply function to value\n" ++
                     "define - define global variable\n" ++
                     "error  - print value to stderr\n" ++
                     "help   - display this help message(use without s-expression)\n" ++
                     "if     - branch on condition\n" ++
                     "lambda - create unnamed function\n" ++
                     "let    - define local variable\n" ++
                     "print  - print value to stdout\n" ++
                     "quit   - quit interpreter(use without s-expression)")

-- That's dead wrong; what is right?
-- until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do result <- prompt
                               if pred result
                               then do
                                    putStrLn("\nMoriturus te saluto.")
                                    return ()
                               else do 
                                   if (result == "help") then do
                                       printHelp
                                       until_ pred prompt action
                                   else action result >> until_ pred prompt action

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = runInputT addSettings $ poll prompt
                where
                    poll :: String -> InputT IO String
                    poll prompt = do
                        input <- getInputLine prompt
                        case input of
                            Nothing -> return "(print \"\")"
                            Just strinput -> return strinput

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runSingleStatement :: [String] -> IO ()
runSingleStatement args = do
    env <- primitiveBindings >>= flip bindVars[("args", 
                                                List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "R5RS> ") . evalAndPrint
