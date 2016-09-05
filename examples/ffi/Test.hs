module Test where

import Zepto.Types

exports :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
exports = [("ret-one", retOne, "always returns one")]

retOne :: [LispVal] -> IOThrowsError LispVal
retOne [] = return $ fromSimple $ Number $ NumS 1
