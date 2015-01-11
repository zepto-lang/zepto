module Main where
import Prompt
import System.Environment

printUsage :: IO ()
printUsage = putStrLn("R5RS version 0.1\nUsage: " ++
                    "\n\twithout arguments - runs REPL" ++
                    "\n\t1 argument - executes single statement")

main :: IO ()
main = do args <- getArgs
          case length args of
                0 -> runRepl
                1 -> runSingleStatement $ args !! 0
                otherwise -> printUsage
