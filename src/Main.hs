module Main where
import Prompt
import System.Environment

-- |Prints the usage
printUsage :: IO ()
printUsage = do printVersion
                putStrLn("\nUsage: " ++
                         "\n\twithout arguments  - runs REPL" ++
                         "\n\t-h/--help          - display this help message" ++
                         "\n\t<some scheme file> - run file" ++
                         "\n\nMore information can be found on " ++
                         "https://github.com/hellerve/R5RS")

-- |Prints the version
printVersion :: IO ()
printVersion = putStrLn "R5RS Version 0.4.5"

-- |Prints the commands that are possible within the REPL
printCommands :: IO ()
printCommands = putStrLn("Type 'quit' or press Ctrl-C to exit interpreter\n" ++
                         "Type 'help' to get a simple help message")

-- |Parses arguments and runs the REPL
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
