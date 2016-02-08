module Main where
import Data.List
import System.Environment

import Zepto.Prompt
import Zepto.Primitives

-- |Prints the usage
printUsage :: IO ()
printUsage = do printVersion
                putStrLn("\nUsage: " ++
                         "\n\twithout arguments - runs REPL" ++
                         "\n\t-h/--help         - display this help message" ++
                         "\n\t-S/--silent       - runs REPL without displaying the header" ++
                         "\n\t-s/--single       - runs single statement passed in as string" ++
                         "\n\t<zepto file>      - run file" ++
                         "\n\nMore information can be found at " ++
                         "http://zepto.veitheller.de")

-- |Prints the version
printVersion :: IO ()
printVersion = putStrLn ("zepto Version "
                        ++ versionStr
                        ++ ", compiled with GHC version "
                        ++ show (__GLASGOW_HASKELL__::Integer))

-- |Prints the commands that are possible within the REPL
printCommands :: IO ()
printCommands = putStrLn("Type ':quit' or press Ctrl-C to exit interpreter,\n" ++
                         "':help' to get a list of commands or ':license' to get the license text")

-- |Prints the copyright notice
printCopyright :: IO ()
printCopyright = putStrLn("Copyright (C) 2015 Veit Heller (GPL)\n" ++
                          "This is free software; " ++
                          "see the accompanying LICENSE " ++
                          "for copying conditions.\n" ++
                          "There is NO warranty whatsoever.\n" ++
                          "Hail Eris, all rites reversed.\n")

-- |Parses arguments and runs the REPL
main :: IO ()
main = do args <- getArgs
          main' args
    where main' :: [String] -> IO ()
          main' arg
            | null arg = do
              printVersion
              printCopyright
              printCommands
              putStrLn ""
              runRepl
            | hasIn arg (makeArg "h" "help") =
              printUsage
            | hasIn arg (makeArg "S" "silent") =
              runRepl
            | any (`elem` (makeArg "s" "single")) arg =
              runSingleStatement (getOpt arg (makeArg "s" "single"))
            | hasIn arg (makeArg "v" "version") =
              printVersion
            | noMeta (head arg) = runFile arg
            | otherwise = do
              putStrLn ("Unknown option: " ++ head arg)
              printUsage
          hasIn :: [String] -> [String] -> Bool
          hasIn x l = any (`elem` l) x && not (any noMeta x)
          makeArg :: String -> String -> [String]
          makeArg short long = ['-' : short, "--" ++ long]
          getOpt :: [String] -> [String] -> String
          getOpt x l = case findIndex (`elem` l) x of
                              Just i -> if length x > i + 1
                                        then x !! (i + 1)
                                        else "(display \"Malformed input: option " ++
                                             x !! i ++ " takes additional argument\")"
                              _      -> "Internal error"
          noMeta :: String -> Bool
          noMeta ('-' : '-' : _) = False
          noMeta ['-',_] = False
          noMeta _ = True
