module Main where
import Types
import Parser
import Eval
import System.Environment
import Control.Monad
import Control.Monad.Error

main :: IO()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled
