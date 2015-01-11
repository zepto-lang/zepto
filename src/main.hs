module Main where
import Prompt
import System.Environment


main :: IO ()
main = do args <- getArgs
          case length args of
                0 -> runRepl
                1 -> evalAndPrint $ args !! 0
                otherwise -> 
                    putStrLn("R5RS version 0.1" ++
                    "\nUsage: " ++
                    "\n\twithout arguments - runs REPL" ++
                    "\n\t1 arguments - executes single statement")
