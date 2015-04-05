module Zepto.Prompt( runRepl
                    , runSingleStatement
                    ) where
import Zepto.Libraries.DDate
import Zepto.Types
import Zepto.Primitives
import Zepto.Variables
import Data.Char
import Data.List
import Data.List.Split
import System.IO
import System.Directory
import Control.Monad
import qualified Control.Exception
import System.Console.Haskeline
import System.IO.Unsafe
import Paths_zepto

defaultPrompt :: String
defaultPrompt = "zepto> "

keywords :: [String]
keywords = [ "apply"
           , "define"
           , "help"
           , "if"
           , "lambda"
           ]

metaKeywords :: [String]
metaKeywords = [ ":quit"
               , ":exit"
               , ":help"
               , ":license"
               , ":complete-license"
               , ":prompt"
               , ":prompt-toggle-space"
               ]

completionSearch :: Env -> String -> [Completion]
completionSearch env str = map simpleCompletion $ filter(str `isPrefixOf`) $ 
                       (map ("(" ++) keywords) ++ getDefs ++ metaKeywords
                where getDefs :: [String]
                      getDefs = map (\x -> "(" ++ getAtom x) $ unsafePerformIO $ recExportsFromEnv env
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
                  getDir = (unsafePerformIO getHomeDirectory) ++ "/.zepto_history"

-- | adds primitive bindings to an empty environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip extendEnv (map (makeFunc IOFunc) ioPrimitives ++
                                map (makeFunc PrimitiveFunc) primitives ++
                                map (makeFunc EvalFunc) evalPrimitives)
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
until_ :: (String -> IO String) -> (String -> IO a) -> String -> IO ()
until_ prompt action text = do result <- prompt text
                               repl_ result
        where repl_ x | matches x ":help" = do
                                _ <- printHelp
                                printKeywords
                                until_ prompt action text
                      | setter x ":prompt" = do
                                until_ prompt action (getOpt x)
                      | matches x ":prompt-toggle-space" = 
                                if last text == ' '
                                    then until_ prompt action (init text)
                                    else until_ prompt action (text ++ " ")
                      | matches x ":license" =
                                printFileContents "license_interactive"
                      | matches x ":complete-license" = do
                                printFileContents "complete_license"
                      | matches x ":easteregg" =
                                printFileContents "grandeur"
                      | matches x ":ddate" = do
                                ddate >>= putStrLn
                                until_ prompt action text
                      | matches x ":quit" || matches x ":exit" = do
                                putStrLn "\nMoriturus te saluto."
                                return ()
                      | otherwise = action x >> until_ prompt action text
              printFileContents file = do
                    filename <- getDataFileName ("assets/" ++ file ++ ".as")
                    fhandle <- openFile filename ReadMode
                    contents <- hGetContents fhandle
                    putStrLn contents
                    hClose fhandle
                    until_ prompt action text
              matches :: String -> String -> Bool
              matches el matcher = 
                    let x = wordsBy isSpace el
                    in length x == 1 && (x !! 0) == matcher
              setter :: String -> String -> Bool
              setter el opt = 
                    let x = wordsBy isSpace el
                    in length x == 2 && (x !! 0) == opt
              getOpt :: String -> String
              getOpt el =
                    let x = wordsBy isSpace el
                    in x !! 1

-- | reads from the prompt
readPrompt :: Env -> String -> IO String
readPrompt env text = runInputT (addSettings env) $ poll text
    where poll :: String -> InputT IO String
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
        until_ (readPrompt env) (evaluation env) defaultPrompt
    where loadFile env file = evalString env $ "(load \"" ++ file ++ "\")"
          evaluation env x = Control.Exception.catch (evalAndPrint env x) handler
          handler msg@(Control.Exception.SomeException _) = putStrLn $ 
                "Caught error: " ++ show (msg::Control.Exception.SomeException)
