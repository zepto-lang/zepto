module Zepto.Prompt( runRepl
                    , runSingleStatement
                    , evalAndPrint
                    , runFile
                    ) where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe (fromMaybe)
import System.Console.Haskeline hiding (listFiles, completeFilename)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import qualified Control.Exception

import Paths_zepto
import Zepto.Libraries.DDate
import Zepto.Types
import Zepto.Primitives
import Zepto.Variables

defaultPrompt :: String
defaultPrompt = "zepto> "

defaultSecond :: String
defaultSecond = ">>> "

metaPrefix :: Char
metaPrefix = ':'

keywords :: [String]
keywords = [ "apply"
           , "define"
           , "help"
           , "if"
           , "lambda"
           , "load"
           , "quote"
           , "quasiquote"
           , "set!"
           , "set-car"
           , "set-cdr"
           , "string-fill"
           , "string-set"
           , "vector-fill"
           , "vector-set"
           ]

metaKeywords :: [String]
metaKeywords = fmap metaize
               [ "quit"
               , "q"
               , "exit"
               , "help"
               , "keyword-help"
               , "meta-help"
               , "license"
               , "complete-license"
               , "prompt"
               , "prompt-toggle-space"
               , "prompt-color"
               , "prompt-bold"
               ]

metaize :: String -> String
metaize cmd = metaPrefix : cmd

completeFilename :: MonadIO m => CompletionFunc m
completeFilename  = completeQuotedWord (Just '\\') "\"'" listFiles $
                    completeWord (Just '\\')
                                 ("\"\'" ++ filenameWordBreakChars)
                                 listFiles

fixPath :: String -> IO String
fixPath "" = return "."
fixPath ('~' : c : path) | isPathSeparator c = do
      home <- getHomeDirectory
      return (home </> path)
fixPath path = return path

completion :: String -> Completion
completion str = Completion str str True

setReplacement :: (String -> String) -> Completion -> Completion
setReplacement f c = c {replacement = f $ replacement c}

listFiles :: MonadIO m => FilePath -> m [Completion]
listFiles path = liftIO $ do
    fixedDir <- fixPath dir
    dirExists <- doesDirectoryExist fixedDir
    std <- getDataFileName "zepto-stdlib"
    stdfiles <- (map completion . filterPrefix) <$>
                  getDirectoryContents std
    allFiles <- if not dirExists
                    then return stdfiles
                    else do
                        curfiles <- (map completion . filterPrefix) <$>
                            getDirectoryContents fixedDir
                        return (curfiles ++ stdfiles)
    forM allFiles $ \c -> do
            isDir <- doesDirectoryExist (fixedDir </> replacement c)
            return $ setReplacement fullName $ alterIfDir isDir c
  where
    (dir, file) = splitFileName path
    filterPrefix = filter (\f -> notElem f [".",".."]
                                        && file `isPrefixOf` f)
    alterIfDir False c = c
    alterIfDir True c = c {replacement = addTrailingPathSeparator (replacement c),
                            isFinished = False}
    fullName = replaceFileName path

completionSearch :: Env -> (String, String) -> IO (String, [Completion])
completionSearch env (l, r) = complete' $ prepare l
        where getDefs :: IO [String]
              getDefs = do exports <- recExportsFromEnv env
                           return $ fmap getAtom exports
              getAtom (SimpleVal (Atom a)) = a
              getAtom _ = ""
              prepare x = reverse $ readIn x
              readIn (x : xs) | x == '(' = []
                              | isSpace x = []
                              | otherwise = x : readIn xs
              readIn [] = []
              complete' ('"' : _) = liftIO $ completeFilename (l, r)
              complete' left = do
                   defs <- getDefs
                   let allD = keywords ++ metaKeywords ++ defs
                   let filtered = filter (left `isPrefixOf`) allD
                   let found = map (\x -> Completion x x False) filtered
                   let rest = fromMaybe l (stripPrefix (reverse left) l)
                   return (rest, found)

-- | returns a fresh settings variable
addSettings :: Env -> IO (Settings IO)
addSettings env = do dir <- getHomeDirectory
                     return Settings { historyFile = Just (dir ++ "/.zepto_history")
                                     , complete = completionSearch env
                                     , autoAddHistory = True
                                     }

-- | adds primitive bindings to an empty environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip extendEnv (fmap (makeFunc IOFunc) ioPrimitives ++
                                fmap (makeFunc PrimitiveFunc) primitives ++
                                fmap (makeFunc EvalFunc) evalPrimitives)
                where makeFunc constructor (var, func, _) = ((vnamespace, var), constructor func)

-- | prints help for all primitives
printHelp :: IO [()]
printHelp = mapM putStrLn $ ["Primitives:"] ++ fmap getHelp primitives ++
                             ["", "IO Primitives:"] ++ fmap getHelp ioPrimitives ++ [""]
                where getHelp tuple = firstEl tuple ++ " - " ++ thirdEl tuple
                      firstEl (x, _, _) = x
                      thirdEl (_, _, x) = x

-- | prints help for all keywords
printKeywords :: IO ()
printKeywords = putStrLn ("Keywords:\n" ++
                          "apply   - apply function to value\n" ++
                          "define  - define global variable\n" ++
                          "error   - print value to stderr\n" ++
                          "help    - display help for function\n" ++
                          "doc     - display help for function(alias for help)\n" ++
                          "if      - branch on condition\n" ++
                          "lambda  - create unnamed function\n" ++
                          "let     - define local variable\n")

printMetaKeywords :: IO ()
printMetaKeywords = putStrLn ("Meta Keywords:\n" ++
                              ":exit                - quit interpreter\n" ++
                              ":help                - print help for all available commands\n" ++
                              ":license             - print license text\n" ++
                              ":keyword-help        - displays help for all keywords\n" ++
                              ":meta-help           - displays this help message\n" ++
                              ":prompt              - changes prompt message*\n" ++
                              ":prompt-color        - changes prompt color*\n" ++
                              ":prompt-bold         - changes prompt font weight\n" ++
                              ":prompt-toggle-space - appends a space to the prompt\n" ++
                              "\n" ++
                              "-- Commands denoted with * take an additional argument")

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
                      | matches x "keyword-help" = do
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
                      | matches x "prompt-bold" =
                                until_ prompt action (fontweight text)
                      | matches x "license" =
                                printFileContents "license_interactive"
                      | matches x "complete-license" =
                                printFileContents "complete_license"
                      | matches x "easteregg" =
                                printFileContents "delusion_of_grandeur"
                      | matches x "ddate" = do
                                ddate >>= putStrLn
                                until_ prompt action text
                      | setter x "quit" || setter x "exit" =
                                let code = read $ getOpt x
                                in
                                  if code == 0
                                    then exitter (liftIO $ tryIOError $ liftIO
                                              exitSuccess)
                                    else exitter (liftIO $ tryIOError $ liftIO $
                                              exitWith $ ExitFailure code)
                      | matches x "quit" || matches x "exit" || matches x "q" = do
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
              exitter x = do _ <- x
                             return ()
              argMissing :: String -> IO ()
              argMissing cmd = do
                      putStrLn ("Error: the " ++ cmd ++
                                " meta command takes one additional argument")
                      until_ prompt action text
              emptyInput :: String -> Bool
              emptyInput el =
                    let x = parse el
                    in null x
              matches :: String -> String -> Bool
              matches el matcher =
                    let x = parse el
                    in length x == 1 && head x == metaize matcher
              setter :: String -> String -> Bool
              setter el opt =
                    let x = parse el
                    in length x == 2 && head x == metaize opt
              getOpt :: String -> String
              getOpt el =
                    let x = parse el
                    in x !! 1
              parse :: String -> [String]
              parse x = case elemIndex '"' x of
                          Just _ ->  let y = wordsBy ('"' ==) x
                                  in trim (head y) : tail y
                          _ -> wordsBy isSpace x
              trim = let f = reverse . dropWhile isSpace
                    in f . f
              slice :: Int -> Int -> [a] -> [a]
              slice f t l = take (t - f + 1) (drop f l)
              fontweight :: String -> String
              fontweight p =
                    if "\x1b[1m" `isInfixOf` p
                        then slice (length "\x1b[1m")
                                   (length p - length "\x1b[0m" - 1)
                                   p
                        else "\x1b[1m" ++ p ++ "\x1b[0m"
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
                      lookupColor color = find (\t -> color == fst t) colors
                      colors = [ ("black", "30")
                               , ("red", "31")
                               , ("green", "32")
                               , ("yellow", "33")
                               , ("blue", "34")
                               , ("magenta", "35")
                               , ("cyan", "36")
                               , ("white", "37")
                               , ("none", "0")
                               , ("reset", "0")
                               ]

-- | reads from the prompt
readPrompt :: Env -> String -> IO String
readPrompt env text = do set <- addSettings env
                         runInputT set $ poll text
    where poll :: String -> InputT IO String
          poll p = do
            input <- getInputLine p
            case input of
                Nothing -> return "(print \"\")"
                Just strinput -> do
                  inputL <- getMore [strinput]
                  return $ unlines inputL
          getMore p =
            if notBalanced p
              then do
                inp <- getInputLine defaultSecond
                case inp of
                  Nothing -> return p
                  Just i -> getMore $ p ++ [i]
              else return p
            where notBalanced x =
                    let before = (sum $ map (count '(') x)::Integer
                        after  = (sum $ map (count ')') x)::Integer
                    in before > after
                  count e = foldr (\x s -> if x == e then s+1 else s) 0

-- | evaluate a line of code and print it
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- | run a single statement
runSingleStatement :: String -> IO ()
runSingleStatement statement = do
        env <- primitiveBindings
        lib <- getDataFileName "zepto-stdlib/module.zp"
        _   <- loadFile env lib
        evalAndPrint env statement
    where loadFile env file = evalString env $ "(load \"" ++ file ++ "\")"

-- | run a file
runFile :: [String] -> IO ()
runFile args = do
        env <- primitiveBindings >>= flip extendEnv[((vnamespace, "args"),
                                                    List $ fromSimple . String <$> drop 1 args)]
        lib <- getDataFileName "zepto-stdlib/module.zp"
        _   <- loadFile env lib
        _ <- runIOThrows (liftM show $ eval env (nullCont env)
          (List [fromSimple (Atom "load"), fromSimple $ String $ head args]))
        return ()
    where loadFile env file = evalString env $ "(load \"" ++ file ++ "\")"


-- | run the REPL
runRepl :: IO ()
runRepl = do
        env <- primitiveBindings
        lib <- getDataFileName "zepto-stdlib/module.zp"
        ret <- loadFile env lib
        _   <- putStrLn ret
        until_ (readPrompt env) (evaluation env) defaultPrompt
    where loadFile env file = evalString env $ "(load \"" ++ file ++ "\")"
          evaluation env x = Control.Exception.catch (evalAndPrint env x) handler
          handler msg@(Control.Exception.SomeException _) = putStrLn $
                "Caught error: " ++ show (msg::Control.Exception.SomeException)
