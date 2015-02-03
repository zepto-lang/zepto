module Main where
import Prompt
import System.Environment

printUsage :: IO ()
printUsage = putStrLn("R5RS version 0.1\nUsage: " ++
                    "\n\twithout arguments\t- runs REPL" ++
                    "\n\t1 argument\t\t- executes single statement")

main :: IO ()
main = do args <- getArgs
          if null args
              then runRepl
              else 
                  if ((args !! 0) ==  "-h") then printUsage 
                  else runSingleStatement $ args
