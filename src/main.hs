module Main where
import Prompt
import System.Environment

printUsage :: IO ()
printUsage = do printVersion
                putStrLn("Usage: " ++
                         "\n\twithout arguments  - runs REPL" ++
                         "\n\t-h/--help          - display this help message" ++
                         "\n\t<some scheme file> - run file")

printVersion :: IO ()
printVersion = putStrLn "R5RS Version 0.3.1"

printCommands :: IO ()
printCommands = putStrLn("Type 'quit' or press Ctrl-C to exit interpreter\n" ++
                         "Type 'help' to get a simple help message")

main :: IO ()
main = do args <- getArgs
          if null args
              then do printVersion
                      printCommands
                      putStrLn ""
                      runRepl
              else 
                  if(head args ==  "-h") || (head args == "--help") 
                      then printUsage 
                      else runSingleStatement args
