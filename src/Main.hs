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
          if null args
              then do printVersion
                      printCopyright
                      printCommands
                      putStrLn ""
                      runRepl
              else
                  if(head args ==  "-h") || (head args == "--help")
                      then printUsage
                      else runSingleStatement args
