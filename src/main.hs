module Main where
import Prompt
import System.Environment

printUsage :: IO ()
printUsage = do printVersion
                putStrLn("Usage: " ++
                    "\n\twithout arguments\t- runs REPL" ++
                    "\n\t1 argument\t\t- executes single statement")

printVersion :: IO ()
printVersion = putStrLn "R5RS Version 0.2"

printCommands :: IO ()
printCommands = putStrLn "Type 'quit' or press Ctrl-C to exit interpreter"

main :: IO ()
main = do args <- getArgs
          if null args
              then do printVersion
                      printCommands
                      putStrLn ""
                      runRepl
              else 
                  if ((args !! 0) ==  "-h") then printUsage 
                  else runSingleStatement $ args
