module Primitives(primitives) where
import Types
import Control.Monad.Error

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

typeTest :: LispVal -> [LispVal] -> ThrowsError LispVal
typeTest (String _) [(String _)] = return (Bool True)
typeTest (Number _) [(Number _)] = return (Bool True)
typeTest (Atom _) [(Atom _)] = return (Bool True)
typeTest _ _ = return (Bool False)
