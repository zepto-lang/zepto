module Primitives(primitives) where
import Types

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", typeTest (String "")),
              ("symbol?", typeTest (Atom "")),
              ("number?", typeTest (Number 0))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

typeTest :: LispVal -> [LispVal] -> LispVal
typeTest (String _) [(String _)] = (Bool True)
typeTest (Number _) [(Number _)] = (Bool True)
typeTest (Atom _) [(Atom _)] = (Bool True)
typeTest _ _ = (Bool False)
