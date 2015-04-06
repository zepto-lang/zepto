module Main where
import System.Environment

import Zepto.Prompt
import Zepto.Primitives

-- |Prints the usage
printUsage :: IO ()
printUsage = do printVersion
                putStrLn("\nUsage: " ++
                         "\n\twithout arguments  - runs REPL" ++
                         "\n\t-h/--help          - display this help message" ++
                         "\n\t-s/--silent        - runs REPL without displaying header" ++
                         "\n\t<some scheme file> - run file" ++
                         "\n\nMore information can be found on " ++
                         "https://github.com/hellerve/zepto")

-- |Prints the version
printVersion :: IO ()
printVersion = putStrLn ("zepto Version " ++ versionStr)

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
            | hasIn arg (makeArg "s" "silent") =
              runRepl
            | length arg == 1 = runSingleStatement arg
            | otherwise =
              printUsage
          hasIn :: [String] -> [String] -> Bool
          hasIn x l = any (\t -> elem t x) l
          makeArg :: String -> String -> [String]
          makeArg short long = ['-' : short, "--" ++ long]
