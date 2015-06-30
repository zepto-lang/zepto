module Zepto.Primitives.LogMathPrimitives where
import Data.Array (elems)
import Data.Char
import Control.Monad.Except
import Data.Complex

import Zepto.Types

eqv :: [LispVal] -> ThrowsError LispVal
eqv [SimpleVal (Bool arg1), SimpleVal (Bool arg2)] = return $ fromSimple $ Bool $ arg1 == arg2
eqv [SimpleVal (Number arg1), SimpleVal (Number arg2)] = return $ fromSimple $ Bool $ arg1 == arg2
eqv [SimpleVal (String arg1), SimpleVal (String arg2)] = return $ fromSimple $ Bool $ arg1 == arg2
eqv [SimpleVal (Atom arg1), SimpleVal (Atom arg2)] = return $ fromSimple $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [x@(EvalFunc _), y@(EvalFunc _)] = return $ fromSimple $ Bool $ show x == show y
eqv [x@(PrimitiveFunc _), y@(PrimitiveFunc _)] = return $ fromSimple $ Bool $ show x == show y
eqv [x@(IOFunc _), y@(IOFunc _)] = return $ fromSimple $ Bool $ show x == show y
eqv [List arg1, List arg2] = return $ fromSimple $ Bool $ (length arg1 == length arg2) &&
                                  and (zipWith (curry eqvPair) arg1 arg2)
                                  where eqvPair (x, y) = case eqv[x, y] of
                                                            Left _ -> False
                                                            Right (SimpleVal (Bool val)) -> val
                                                            _ -> False
eqv [Vector arg1, Vector arg2] = eqv [List (elems arg1), List (elems arg2)]
eqv [_, _] = return $ fromSimple $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List arg1, List arg2] =
        return $ fromSimple $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                Left _                       -> False
                                Right (SimpleVal (Bool val)) -> val
                                _                            -> False
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
           return $ fromSimple $ Bool (primitiveEquals || let (SimpleVal (Bool z)) = eqvEquals in z)
equal badArgList = throwError $ NumArgs 2 badArgList

numericBinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op p = liftM (fromSimple . Number . foldl1 op) (mapM unpackNum p)

numericMinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericMinop _ [SimpleVal (Number l)] = return $ fromSimple $ Number $ negate l
numericMinop op p = liftM (fromSimple . Number . foldl1 op) (mapM unpackNum p)

numericPlusop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericPlusop _ [SimpleVal (Number l)] = if l > 0 then return $ fromSimple $ Number l
                                      else return $ fromSimple $ Number $ negate l
numericPlusop op p = liftM (fromSimple . Number . foldl1 op) (mapM unpackNum p)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ fromSimple $ Bool $ left `op` right

boolMulop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolMulop op p = liftM (SimpleVal . Bool . foldl1 op) (mapM unpackBool p)

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
unpackNum (SimpleVal (Number n)) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (SimpleVal (String s)) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackCIStr :: LispVal -> ThrowsError String
unpackCIStr (SimpleVal (String s)) = return $ fmap toLower s
unpackCIStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (SimpleVal (Bool b)) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

numRound :: (Double -> Integer) -> [LispVal] -> ThrowsError LispVal
numRound _ [n@(SimpleVal (Number (NumI _)))] = return n
numRound _ [n@(SimpleVal (Number (NumS _)))] = return n
numRound op [SimpleVal (Number (NumF n))] = return $ fromSimple $ Number $ NumI $ op n
numRound op [SimpleVal (Number (NumC n))] = return $ fromSimple $ Number $ NumC $ fromInteger (op $ realPart n) :+ fromInteger (op $ imagPart n)
numRound op [SimpleVal (Number (NumR n))] = return $ fromSimple $ Number $ NumI $ op $ fromRational n
numRound _ [x] = throwError $ TypeMismatch "number" x
numRound _ badArgList = throwError $ NumArgs 1 badArgList

makeSmall :: [LispVal] -> ThrowsError LispVal
makeSmall [SimpleVal (Number (NumI n))] = return $ fromSimple $ Number $ NumS $ fromInteger n
makeSmall [badType] = throwError $ TypeMismatch "integer" badType
makeSmall badArgList = throwError $ NumArgs 1 badArgList

real :: [LispVal] -> ThrowsError LispVal
real [SimpleVal (Number (NumC x))] = return $ fromSimple $ Number $ NumF $ realPart x
real [val@(SimpleVal (Number (NumF _)))] = return val
real [val@(SimpleVal (Number (NumR _)))] = return val
real [val@(SimpleVal (Number (NumI _)))] = return val
real [val@(SimpleVal (Number (NumS _)))] = return val
real[x] = throwError $ TypeMismatch "number" x
real badArgList = throwError $ NumArgs 1 badArgList

imaginary :: [LispVal] -> ThrowsError LispVal
imaginary [SimpleVal (Number (NumC x))] = return $ fromSimple $ Number $ NumF $ imagPart x
imaginary [SimpleVal (Number _)] = return $ fromSimple $ Number $ NumF $ 0
imaginary [x] = throwError $ TypeMismatch "number" x
imaginary badArgList = throwError $ NumArgs 1 badArgList

numOp :: (Double -> Double) -> [LispVal] -> ThrowsError LispVal
numOp op [SimpleVal (Number (NumI n))] = return $ fromSimple $ Number $ NumF $ op $ fromInteger n
numOp op [SimpleVal (Number (NumS n))] = return $ fromSimple $ Number $ NumF $ op $ fromIntegral n
numOp op [SimpleVal (Number (NumF n))] = return $ fromSimple $ Number $ NumF $ op n
numOp op [SimpleVal (Number (NumR n))] = return $ fromSimple $ Number $ NumR $ toRational $ op $ fromRational n
numOp op [SimpleVal (Number (NumC n))] = return $ fromSimple $ Number $ NumC $ op (realPart n) :+ op (imagPart n)
numOp _ [x] = throwError $ TypeMismatch "number" x
numOp _ badArgList = throwError $ NumArgs 1 badArgList

numLog :: [LispVal] -> ThrowsError LispVal
numLog [SimpleVal (Number (NumI n))] = return $ fromSimple $ Number $ NumF $ log $ fromInteger n
numLog [SimpleVal (Number (NumS n))] = return $ fromSimple $ Number $ NumF $ log (fromIntegral n)
numLog [SimpleVal (Number (NumF n))] = return $ fromSimple $ Number $ NumF $ log n
numLog [SimpleVal (Number (NumR n))] = return $ fromSimple $ Number $ NumF $ log $ fromRational n
numLog [SimpleVal (Number (NumC n))] = return $ fromSimple $ Number $ NumC $ log n
numLog [SimpleVal (Number (NumI n)), SimpleVal (Number (NumI base))] =
    return $ fromSimple $ Number $ NumF $ logBase (fromInteger base) (fromIntegral n)
numLog [SimpleVal (Number (NumS n)), SimpleVal (Number (NumI base))] =
    return $ fromSimple $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [SimpleVal (Number (NumI n)), SimpleVal (Number (NumS base))] =
    return $ fromSimple $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [SimpleVal (Number (NumS n)), SimpleVal (Number (NumS base))] =
    return $ fromSimple $ Number $ NumF $ logBase (fromIntegral base) (fromIntegral n)
numLog [SimpleVal (Number (NumF n)), SimpleVal (Number (NumI base))] =
    return $ fromSimple $ Number $ NumF $ logBase (fromInteger base) n
numLog [SimpleVal (Number (NumC n)), SimpleVal (Number (NumI base))] =
    return $ fromSimple $ Number $ NumC $ logBase (fromInteger base) n
numLog [SimpleVal (Number (NumF n)), SimpleVal (Number (NumS base))] =
    return $ fromSimple $ Number $ NumF $ logBase (fromIntegral base) n
numLog [SimpleVal (Number (NumC n)), SimpleVal (Number (NumS base))] =
    return $ fromSimple $ Number $ NumC $ logBase (fromIntegral base) n
numLog [x] = throwError $ TypeMismatch "number" x
numLog badArgList = throwError $ NumArgs 1 badArgList

numPow :: [LispVal] -> ThrowsError LispVal
numPow [SimpleVal (Number (NumI n)), wrong@(SimpleVal (Number (NumI base)))] =
    if base > -1
        then return $ fromSimple $ Number $ NumI $ n ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [SimpleVal (Number (NumS n)), wrong@(SimpleVal (Number (NumI base)))] =
    if base > -1
        then return $ fromSimple $ Number $ NumI $ (fromIntegral n) ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [SimpleVal (Number (NumI n)), wrong@(SimpleVal (Number (NumS base)))] =
    if base > -1
        then return $ fromSimple $ Number $ NumI $ n ^ (fromIntegral base::Integer)
        else throwError $ TypeMismatch "positive" wrong
numPow [SimpleVal (Number (NumS n)), wrong@(SimpleVal (Number (NumS base)))] =
    if base > -1
        then return $ fromSimple $ Number $ NumS $ n ^ base
        else throwError $ TypeMismatch "positive" wrong
numPow [SimpleVal (Number (NumF n)), SimpleVal (Number (NumI base))] =
    return $ fromSimple $ Number $ NumF $ n ** fromIntegral base
numPow [SimpleVal (Number (NumI n)), SimpleVal (Number (NumF base))] =
    return $ fromSimple $ Number $ NumF $ fromIntegral n ** base
numPow [SimpleVal (Number (NumF n)), SimpleVal (Number (NumF base))] =
    return $ fromSimple $ Number $ NumF $ n ** base
numPow [SimpleVal (Number (NumC n)), SimpleVal (Number (NumI base))] =
    return $ fromSimple $ Number $ NumC $ n ** fromIntegral base
numPow [SimpleVal (Number (NumC n)), SimpleVal (Number (NumF base))] =
    return $ fromSimple $ Number $ NumC $ n ** (base :+ 0)
numPow [SimpleVal (Number (NumR n)), SimpleVal (Number (NumR base))] =
    return $ fromSimple $ Number $ NumF $ fromRational n ** fromRational base
numPow [SimpleVal (Number (NumR n)), SimpleVal (Number (NumI base))] =
    return $ fromSimple $ Number $ NumF $ fromRational n ** fromIntegral base
numPow [SimpleVal (Number (NumI n)), SimpleVal (Number (NumR base))] =
    return $ fromSimple $ Number $ NumF $ fromIntegral n ** fromRational base
numPow [SimpleVal (Number (NumR n)), SimpleVal (Number (NumF base))] =
    return $ fromSimple $ Number $ NumF $ fromRational n ** base
numPow [SimpleVal (Number (NumF n)), SimpleVal (Number (NumR base))] =
    return $ fromSimple $ Number $ NumF $ n ** fromRational base
numPow [SimpleVal (Number (NumC n)), SimpleVal (Number (NumR base))] =
    return $ fromSimple $ Number $ NumC $ n ** (fromRational base :+ 0)
numPow [SimpleVal (Number (NumF n)), SimpleVal (Number (NumS base))] =
    return $ fromSimple $ Number $ NumF $ n ** fromIntegral base
numPow [SimpleVal (Number (NumS n)), SimpleVal (Number (NumF base))] =
    return $ fromSimple $ Number $ NumF $ fromIntegral n ** base
numPow [SimpleVal (Number (NumR n)), SimpleVal (Number (NumS base))] =
    return $ fromSimple $ Number $ NumF $ fromRational n ** fromIntegral base
numPow [SimpleVal (Number (NumS n)), SimpleVal (Number (NumR base))] =
    return $ fromSimple $ Number $ NumF $ fromIntegral n ** fromRational base
numPow [SimpleVal (Number (NumC n)), SimpleVal (Number (NumS base))] =
    return $ fromSimple $ Number $ NumC $ n ** fromIntegral base
numPow [SimpleVal (Number _), x] = throwError $ TypeMismatch "number" x
numPow [x, SimpleVal (Number _)] = throwError $ TypeMismatch "number(not complex)" x
numPow badArgList = throwError $ NumArgs 2 badArgList

numSqrt :: [LispVal] -> ThrowsError LispVal
numSqrt [SimpleVal (Number (NumI n))] = if n >= 0 then return $ fromSimple $ Number $ NumF $ sqrt $ fromInteger n
                                                  else return $ fromSimple $ Number $ NumC $ sqrt (fromInteger n :+ 0)
numSqrt [SimpleVal (Number (NumS n))] = if n >= 0 then return $ fromSimple $ Number $ NumF $ sqrt $ fromIntegral n
                                                  else return $ fromSimple $ Number $ NumC $ sqrt (fromIntegral n :+ 0)
numSqrt [SimpleVal (Number (NumF n))] = if n >= 0 then return $ fromSimple $ Number $ NumF $ sqrt n
                                                  else return $ fromSimple $ Number $ NumC $ sqrt (n :+ 0)
numSqrt [SimpleVal (Number (NumC n))] = return $ fromSimple $ Number $ NumC $ sqrt n
numSqrt [SimpleVal (Number (NumR n))] = return $ fromSimple $ Number $ NumF $ sqrt $ fromRational n
numSqrt [x] = throwError $ TypeMismatch "number" x
numSqrt badArgList = throwError $ NumArgs 1 badArgList

