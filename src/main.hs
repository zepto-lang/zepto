module Main where
import Parser
import Eval
import System.Environment

main :: IO()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

