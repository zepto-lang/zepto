module Prompt(runRepl, runSingleStatement) where
import Types
import Parser
import Primitives
import Data.List
import System.IO
import Control.Monad
import System.Console.Haskeline
import System.Console.Haskeline.History

completion = ["print", "error", "lambda", "quoted"]

completionSearch :: String -> [Completion]
completionSearch str = map simpleCompletion $ filter(str `isPrefixOf`) completion

addSettings :: Settings IO
addSettings = Settings { historyFile = Just ".r5rs_history"
                       , complete = completeWord Nothing " \t" $ return . completionSearch
                       , autoAddHistory = True
                       }

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++
                                map (makeFunc PrimitiveFunc) primitives)
                where makeFunc constructor (var, func) = (var, constructor func)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do result <- prompt
                               if pred result
                               then return ()
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
