module Zepto.Prompt( runRepl
                    , runSingleStatement
                    , evalAndPrint
                    , runFile
                    ) where
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import System.Console.Haskeline
import System.Directory
import System.IO
import System.IO.Unsafe
import qualified Control.Exception

import Paths_zepto
import Zepto.Libraries.DDate
import Zepto.Types
import Zepto.Primitives
import Zepto.Variables

defaultPrompt :: String
defaultPrompt = "zepto> "

metaPrefix :: Char
metaPrefix = ':'

keywords :: [String]
keywords = [ "apply"
           , "define"
           , "help"
           , "if"
           , "lambda"
           ]

metaKeywords :: [String]
metaKeywords = map metaize
               [ "quit"
               , "exit"
               , "help"
               , "meta-help"
               , "license"
               , "complete-license"
               , "prompt"
               , "prompt-toggle-space"
               , "prompt-color"
               ]

metaize :: String -> String
metaize cmd = metaPrefix : cmd

completionSearch :: Env -> String -> [Completion]
completionSearch env str = map simpleCompletion $ filter(str `isPrefixOf`) $
                       map ("(" ++) keywords ++ getDefs ++ metaKeywords
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
                  getDir = unsafePerformIO getHomeDirectory ++ "/.zepto_history"

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
printKeywords = putStrLn ("Keywords:\n" ++
                          "apply   - apply function to value\n" ++
                          "define  - define global variable\n" ++
                          "error   - print value to stderr\n" ++
                          "help    - display help for function" ++
                          "if      - branch on condition\n" ++
                          "lambda  - create unnamed function\n" ++
                          "let     - define local variable\n")

printMetaKeywords :: IO ()
printMetaKeywords = putStrLn ("Meta Keywords:\n" ++
                              ":exit      - quit interpreter\n" ++
                              ":help      - print help for all available commands\n" ++
                              ":license   - print license text\n" ++
                              ":meta-help - displays this help message\n" ++
                              ":prompt    - changes prompt message (takes additional command)\n")

-- | the main interpreter loop; gets input and hands everything except help and quit over
until_ :: (String -> IO String) -> (String -> IO a) -> String -> IO ()
until_ prompt action text = do result <- prompt text
                               repl_ result
        where repl_ x | emptyInput x =
                                until_ prompt action text
                      | matches x "help" = do
                                _ <- printHelp
                                printKeywords
                                until_ prompt action text
                      | matches x "meta-help" = do
                                printMetaKeywords
                                until_ prompt action text
                      | setter x "prompt" =
                                until_ prompt action (getOpt x)
                      | matches x "prompt" = argMissing "prompt"
                      | matches x "prompt-toggle-space" =
                                if last text == ' '
                                    then until_ prompt action (init text)
                                    else until_ prompt action (text ++ " ")
                      | setter x "prompt-color" =
                                until_ prompt action (colorize text (getOpt x))
                      | matches x "prompt-color" = argMissing "prompt-color"
                      | matches x "license" =
                                printFileContents "license_interactive"
                      | matches x "complete-license" =
                                printFileContents "complete_license"
                      | matches x "easteregg" =
                                printFileContents "grandeur"
                      | matches x "ddate" = do
                                ddate >>= putStrLn
                                until_ prompt action text
                      | matches x "quit" || matches x "exit" = do
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
              argMissing :: String -> IO ()
              argMissing cmd = do
                      putStrLn ("Error: the " ++ cmd ++
                                " meta command takes one additional argument")
                      until_ prompt action text
              emptyInput :: String -> Bool
              emptyInput el =
                    let x = wordsBy isSpace el
                    in null x
              matches :: String -> String -> Bool
              matches el matcher =
                    let x = wordsBy isSpace el
                    in length x == 1 && head x == metaize matcher
              setter :: String -> String -> Bool
              setter el opt =
                    let x = wordsBy isSpace el
                    in length x == 2 && head x == metaize opt
              getOpt :: String -> String
              getOpt el =
                    let x = wordsBy isSpace el
                    in x !! 1
              colorize :: String -> String -> String
              colorize oldPrompt c =
                    case lookupColor c of
                      Just colorstring ->  "\x1b[" ++ snd colorstring ++
                                        "m" ++ resetPrompt oldPrompt ++
                                        "\x1b[0m"
                      _                 -> oldPrompt
                where resetPrompt p = if "\x1b[0m" `isInfixOf` p
                                        then slice (length "\x1b[30m")
                                                    (length p - length "\x1b[0m")
                                                    p
                                        else p
                      slice f t l = take (t - f + 1) (drop f l)
                      lookupColor color = find (\t -> color == fst t) colors
                      colors = [ ("black", "30")
                               , ("red", "31")
                               , ("green", "32")
                               , ("yellow", "33")
                               , ("blue", "34")
                               , ("magenta", "35")
                               , ("cyan", "36")
                               , ("white", "37")
                               ]

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
runSingleStatement :: String -> IO ()
runSingleStatement statement = do
        env <- primitiveBindings
        lib <- getDataFileName "stdlib/module.scm"
        _ <- loadFile env lib
        evalAndPrint env statement
    where loadFile env file = evalString env $ "(load \"" ++ file ++ "\")"

-- | run a file
runFile :: [String] -> IO ()
runFile args = do
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
