module Zepto.Primitives.LogMathPrimitives where
import Data.Array (elems)
import Data.Char
import Control.Monad.Except
import Data.Complex

import Zepto.Types

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [x@(EvalFunc _), y@(EvalFunc _)] = return $ Bool $ show x == show y
eqv [x@(PrimitiveFunc _), y@(PrimitiveFunc _)] = return $ Bool $ show x == show y
eqv [x@(IOFunc _), y@(IOFunc _)] = return $ Bool $ show x == show y
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                  and (zipWith (curry eqvPair) arg1 arg2)
                                  where eqvPair (x, y) = case eqv[x, y] of
                                                            Left _ -> False
                                                            Right (Bool val) -> val
                                                            _ -> False
eqv [Vector arg1, Vector arg2] = eqv [List (elems arg1), List (elems arg2)]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List arg1, List arg2] =
        return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                Left _           -> False
                                Right (Bool val) -> val
                                _                -> False
eqvList _ _ = throwError $ InternalError "Unexpected error in eqvList"

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) =
        do unpacked1 <- unpacker x
           unpacked2 <- unpacker y
           return $ unpacked1 == unpacked2
        `catchError` const (return False)

equal :: [LispVal] ->ThrowsError LispVal
equal [lx@(List _), ly@(List _)] = eqvList equal [lx, ly]
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [Vector arg1, Vector arg2] = eqvList equal [List (elems arg1), List (elems arg2)]
equal [x, y] = do
           primitiveEquals <- liftM or $ mapM (unpackEquals x y)
                              [AnyUnpacker unpackNum, AnyUnpacker unpackStr,
                               AnyUnpacker unpackBool]
           eqvEquals <- eqv [x, y]
           return $ Bool (primitiveEquals || let (Bool z) = eqvEquals in z)
equal badArgList = throwError $ NumArgs 2 badArgList

numericBinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op p = liftM (Number . foldl1 op) (mapM unpackNum p)

numericMinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericMinop _ [Number l] = return $ Number $ negate l
numericMinop op p = liftM (Number . foldl1 op) (mapM unpackNum p)

numericPlusop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericPlusop _ [Number l] = if l > 0 then return $ Number l
                                      else return $ Number $ negate l
numericPlusop op p = liftM (Number . foldl1 op) (mapM unpackNum p)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

boolMulop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolMulop op p = liftM (Bool . foldl1 op) (mapM unpackBool p)

numBoolBinop :: (LispNum -> LispNum -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

strCIBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strCIBoolBinop = boolBinop unpackCIStr

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ _ = throwError $ InternalError "Internal error in unaryOp"

unpackNum :: LispVal -> ThrowsError LispNum
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackCIStr :: LispVal -> ThrowsError String
unpackCIStr (String s) = return $ fmap toLower s
unpackCIStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

numRound :: (Double -> Integer) -> [LispVal] -> ThrowsError LispVal
numRound _ [n@(Number (NumI _))] = return n
numRound _ [n@(Number (NumS _))] = return n
numRound op [Number (NumF n)] = return $ Number $ NumI $ op n
numRound op [Number (NumC n)] = return $ Number $ NumC $ fromInteger (op $ realPart n) :+ fromInteger (op $ imagPart n)
numRound op [Number (NumR n)] = return $ Number $ NumI $ op $ fromRational n
numRound _ [x] = throwError $ TypeMismatch "number" x
numRound _ badArgList = throwError $ NumArgs 1 badArgList

real :: [LispVal] -> ThrowsError LispVal
real [Number (NumC x)] = return $ Number $ NumF $ realPart x
real [val@(Number (NumF _))] = return val
real [val@(Number (NumR _))] = return val
real [val@(Number (NumI _))] = return val
real [val@(Number (NumS _))] = return val
real[x] = throwError $ TypeMismatch "number" x
real badArgList = throwError $ NumArgs 1 badArgList

imaginary :: [LispVal] -> ThrowsError LispVal
imaginary [Number (NumC x)] = return $ Number $ NumF $ imagPart x
imaginary [Number _] = return $ Number $ NumF $ 0
imaginary [x] = throwError $ TypeMismatch "number" x
imaginary badArgList = throwError $ NumArgs 1 badArgList

numOp :: (Double -> Double) -> [LispVal] -> ThrowsError LispVal
numOp op [Number (NumI n)] = return $ Number $ NumF $ op $ fromInteger n
numOp op [Number (NumS n)] = return $ Number $ NumF $ op $ fromIntegral n
numOp op [Number (NumF n)] = return $ Number $ NumF $ op n
numOp op [Number (NumR n)] = return $ Number $ NumR $ toRational $ op $ fromRational n
numOp op [Number (NumC n)] = return $ Number $ NumC $ op (realPart n) :+ op (imagPart n)
numOp _ [x] = throwError $ TypeMismatch "number" x
numOp _ badArgList = throwError $ NumArgs 1 badArgList

numLog :: [LispVal] -> ThrowsError LispVal
numLog [Number (NumI n)] = return $ Number $ NumF $ log $ fromInteger n
numLog [Number (NumS n)] = return $ Number $ NumF $ log (fromIntegral n)
numLog [Number (NumF n)] = return $ Number $ NumF $ log n
numLog [Number (NumR n)] = return $ Number $ NumF $ log $ fromRational n
numLog [Number (NumC n)] = return $ Number $ NumC $ log n
numLog [Number (NumI n), Number (NumI base)] =
    return $ Number $ NumF $ logBase (fromInteger base) (fromIntegral n)
numLog [Number (NumS n), Number (NumI base)] =
    return $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [Number (NumI n), Number (NumS base)] =
    return $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [Number (NumS n), Number (NumS base)] =
    return $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [Number (NumF n), Number (NumI base)] =
    return $ Number $ NumF $ logBase (fromInteger base) n
numLog [Number (NumC n), Number (NumI base)] =
    return $ Number $ NumC $ logBase (fromInteger base) n
numLog [Number (NumF n), Number (NumS base)] =
    return $ Number $ NumF $ logBase (fromIntegral base) n
numLog [Number (NumC n), Number (NumS base)] =
    return $ Number $ NumC $ logBase (fromIntegral base) n
numLog [x] = throwError $ TypeMismatch "number" x
numLog badArgList = throwError $ NumArgs 1 badArgList

numPow :: [LispVal] -> ThrowsError LispVal
numPow [Number (NumI n), wrong@(Number (NumI base))] =
    if base > -1
        then return $ Number $ NumI $ n ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [Number (NumS n), wrong@(Number (NumI base))] =
    if base > -1
        then return $ Number $ NumI $ (fromIntegral n) ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [Number (NumI n), wrong@(Number (NumS base))] =
    if base > -1
        then return $ Number $ NumI $ n ^ (fromIntegral base::Integer)
        else throwError $ TypeMismatch "positive" wrong
numPow [Number (NumS n), wrong@(Number (NumS base))] =
    if base > -1
        then return $ Number $ NumS $ n ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [Number (NumF n), Number (NumI base)] =
    return $ Number $ NumF $ n ** fromIntegral base
numPow [Number (NumI n), Number (NumF base)] =
    return $ Number $ NumF $ fromIntegral n ** base
numPow [Number (NumF n), Number (NumF base)] =
    return $ Number $ NumF $ n ** base
numPow [Number (NumC n), Number (NumI base)] =
    return $ Number $ NumC $ n ** fromIntegral base
numPow [Number (NumC n), Number (NumF base)] =
    return $ Number $ NumC $ n ** (base :+ 0)
numPow [Number (NumR n), Number (NumR base)] =
    return $ Number $ NumF $ fromRational n ** fromRational base
numPow [Number (NumR n), Number (NumI base)] =
    return $ Number $ NumF $ fromRational n ** fromIntegral base
numPow [Number (NumI n), Number (NumR base)] =
    return $ Number $ NumF $ fromIntegral n ** fromRational base
numPow [Number (NumR n), Number (NumF base)] =
    return $ Number $ NumF $ fromRational n ** base
numPow [Number (NumF n), Number (NumR base)] =
    return $ Number $ NumF $ n ** fromRational base
numPow [Number (NumC n), Number (NumR base)] =
    return $ Number $ NumC $ n ** (fromRational base :+ 0)
numPow [Number (NumF n), Number (NumS base)] =
    return $ Number $ NumF $ n ** fromIntegral base
numPow [Number (NumS n), Number (NumF base)] =
    return $ Number $ NumF $ fromIntegral n ** base
numPow [Number (NumR n), Number (NumS base)] =
    return $ Number $ NumF $ fromRational n ** fromIntegral base
numPow [Number (NumS n), Number (NumR base)] =
    return $ Number $ NumF $ fromIntegral n ** fromRational base
numPow [Number (NumC n), Number (NumS base)] =
    return $ Number $ NumC $ n ** fromIntegral base
numPow [Number _, x] = throwError $ TypeMismatch "number" x
numPow [x, Number _] = throwError $ TypeMismatch "number(not complex)" x
numPow badArgList = throwError $ NumArgs 2 badArgList

numSqrt :: [LispVal] -> ThrowsError LispVal
numSqrt [Number (NumI n)] = if n >= 0 then return $ Number $ NumF $ sqrt $ fromInteger n
                                      else return $ Number $ NumC $ sqrt (fromInteger n :+ 0)
numSqrt [Number (NumS n)] = if n >= 0 then return $ Number $ NumF $ sqrt $ fromIntegral n
                                      else return $ Number $ NumC $ sqrt (fromIntegral n :+ 0)
numSqrt [Number (NumF n)] = if n >= 0 then return $ Number $ NumF $ sqrt n
                                      else return $ Number $ NumC $ sqrt (n :+ 0)
numSqrt [Number (NumC n)] = return $ Number $ NumC $ sqrt n
numSqrt [Number (NumR n)] = return $ Number $ NumF $ sqrt $ fromRational n
numSqrt [x] = throwError $ TypeMismatch "number" x
numSqrt badArgList = throwError $ NumArgs 1 badArgList

