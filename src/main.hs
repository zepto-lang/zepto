module Main where
import Prompt
import System.Environment

printUsage :: IO ()
printUsage = putStrLn("R5RS version 0.1\nUsage: " ++
                    "\n\twithout arguments - runs REPL" ++
                    "\n\t1 argument - executes single statement")

main :: IO ()
main = do args <- getArgs
          if null args
              then runRepl
              else 
                  if ((args !! 0) ==  "-h") then printUsage 
                  else runSingleStatement $ args
