module Zepto.Primitives.ListPrimitives where
import Data.Array
import Data.List (findIndex)
import Data.Word (Word8)
import Control.Monad.Except

import qualified Data.ByteString as BS (length, index, replicate, singleton, cons,
                                        append, empty, pack, take, drop)
import qualified Data.ByteString.UTF8 as BS (fromString, toString)

import Zepto.Types

carDoc :: String
carDoc = "take the first element of the list <par>l</par>.\n\
\n\
  params:\n\
    - l: the input list\n\
  complexity: O(1)\n\
  returns: the first element"

car :: LispVal -> ThrowsError LispVal
car (List (x : _)) = return x
car (SimpleVal (SimpleList (x : _))) = return $ fromSimple x
car (DottedList (x : _) _) = return x
car badArg = throwError $ TypeMismatch "pair" badArg

cdrDoc :: String
cdrDoc = "take the tail of the list <par>l</par>.\n\
\n\
  params:\n\
    - l: the input list\n\
  complexity: O(1)\n\
  returns: the tail"

cdr :: LispVal -> ThrowsError LispVal
cdr (List (_ : xs)) = return $ List xs
cdr (SimpleVal (SimpleList (_ : xs))) = return $ fromSimple $ SimpleList xs
cdr (DottedList [_] x) = return x
cdr (DottedList (_ : xs) x) = return $ DottedList xs x
cdr badArg = throwError $ TypeMismatch "pair" badArg

consDoc :: String
consDoc = "construct a list or append a head to a list.\n\
           If only one element is given, a singleton list will be created.\n\
\n\
\n\
  params:\n\
    - head: the head element\n\
    - tail: the tail element (optional)\n\
  complexity: O(1)\n\
  returns: a new list of the form <zepto>(++ [head] tail)</zepto>"

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [SimpleVal x, SimpleVal (SimpleList (_ : xs))] = return $ fromSimple $ SimpleList (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x] = return $ DottedList [] x
cons badArgList = throwError $ NumArgs 2 badArgList

makeVector, makeByteVector, buildVector, buildByteVector, vectorRef, byteVectorRef, subByteVector, subVector, stringRef, stringFind :: [LispVal] -> ThrowsError LispVal
makeVector [n@(SimpleVal (Number _))] = makeVector [n, List []]
makeVector [SimpleVal (Number (NumI n)), a] = do
    let l = replicate (fromInteger n) a
    return $ Vector $ listArray (0, length l - 1) l
makeVector [SimpleVal (Number (NumS n)), a] = do
    let l = replicate n a
    return $ Vector $ listArray (0, length l - 1) l
makeVector [badType] = throwError $ TypeMismatch "integer" badType
makeVector badArgList = throwError $ NumArgs 1 badArgList

buildVector l = return $ Vector $ listArray (0, length l - 1) l

buildByteVector l = case verify l of
  Right x  -> return $ ByteVector $ BS.pack x
  Left err -> throwError $ TypeMismatch "integer" err
  where verify :: [LispVal] -> Either LispVal [Word8]
        verify [] = Right []
        verify (SimpleVal (Number (NumI x)) : xs) =
          case verify xs of
            Right z  -> Right ((fromInteger x :: Word8) : z)
            Left err -> Left err
        verify (SimpleVal (Number (NumS x)) : xs) =
          case verify xs of
            Right z  -> Right ((fromIntegral x :: Word8) : z)
            Left err -> Left err
        verify (x : _) = Left x

makeByteVector [n@(SimpleVal (Number _))] = makeByteVector [n, fromSimple $ Number $ NumI 0]
makeByteVector [SimpleVal (Number (NumI n)), SimpleVal (Number (NumI x))] =
    return $ ByteVector $ BS.replicate (fromInteger n) (fromInteger x :: Word8)
makeByteVector [SimpleVal (Number (NumS n)), SimpleVal (Number (NumI x))] =
    return $ ByteVector $ BS.replicate n (fromInteger x :: Word8)
makeByteVector [SimpleVal (Number (NumI n)), SimpleVal (Number (NumS x))] =
    return $ ByteVector $ BS.replicate (fromInteger n) (fromIntegral x :: Word8)
makeByteVector [SimpleVal (Number (NumS n)), SimpleVal (Number (NumS x))] =
    return $ ByteVector $ BS.replicate n (fromIntegral x :: Word8)
makeByteVector [badType] = throwError $ TypeMismatch "integer" badType
makeByteVector badArgList = throwError $ NumArgs 1 badArgList

vectorLength, byteVectorLength, vectorToList, listToVector :: LispVal -> ThrowsError LispVal
vectorLength (Vector v) = return $ fromSimple $ Number $ NumI $ toInteger $ length (elems v)
vectorLength badType = throwError $ TypeMismatch "vector" badType

byteVectorLength (ByteVector v) = return $ fromSimple $ Number $ NumI $ toInteger $ BS.length v
byteVectorLength badType = throwError $ TypeMismatch "bytevector" badType

vectorRef [Vector v, SimpleVal (Number (NumI n))] = return $ v ! fromInteger n
vectorRef [Vector v, SimpleVal (Number (NumS n))] = return $ v ! n
vectorRef [Vector _, badType] = throwError $ TypeMismatch "integer" badType
vectorRef [badType, _] = throwError $ TypeMismatch "vector" badType
vectorRef badArgList = throwError $ NumArgs 2 badArgList

byteVectorRef [ByteVector v, SimpleVal (Number (NumI n))] =
        return $ fromSimple $ Number $ NumI $ fromIntegral $ BS.index v (fromInteger n)
byteVectorRef [ByteVector v, SimpleVal (Number (NumS n))] =
        return $ fromSimple $ Number $ NumI $ fromIntegral $ BS.index v n
byteVectorRef [Vector _, badType] = throwError $ TypeMismatch "vector" badType
byteVectorRef [badType, _] = throwError $ TypeMismatch "vector" badType
byteVectorRef badArgList = throwError $ NumArgs 2 badArgList

subArray :: (Int, Int) -> Array Int LispVal -> Array Int LispVal
subArray (from, to) v = listArray (0,to-from) $ map (v!) [from..to]

subVector [Vector v, SimpleVal (Number (NumI n))] = return $ Vector $ subArray (0, fromInteger n) v
subVector [Vector v, SimpleVal (Number (NumS n))] = return $ Vector $ subArray (0, n) v
subVector [Vector v, SimpleVal (Number (NumI from)), SimpleVal (Number (NumI to))] =
        return $ Vector $ subArray (fromInteger from, fromInteger to) v
subVector [Vector v, SimpleVal (Number (NumS from)), SimpleVal (Number (NumS to))] =
        return $ Vector $ subArray (from, to) v
subVector [Vector v, SimpleVal (Number (NumI from)), SimpleVal (Number (NumS to))] =
        return $ Vector $ subArray (fromInteger from, to) v
subVector [Vector v, SimpleVal (Number (NumS from)), SimpleVal (Number (NumI to))] =
        return $ Vector $ subArray (from, fromInteger to) v
subVector [Vector _, SimpleVal (Number (NumI _)), badType] = throwError $ TypeMismatch "integer" badType
subVector [Vector _, SimpleVal (Number (NumS _)), badType] = throwError $ TypeMismatch "integer" badType
subVector (Vector _ : badType : _) = throwError $ TypeMismatch "integer" badType
subVector (badType : _) = throwError $ TypeMismatch "vector" badType
subVector badArgList = throwError $ NumArgs 2 badArgList

subByteVector [ByteVector v, SimpleVal (Number (NumI n))] =
        return $ ByteVector $ BS.take (fromInteger n) v
subByteVector [ByteVector v, SimpleVal (Number (NumS n))] =
        return $ ByteVector $ BS.take n v
subByteVector [ByteVector v, SimpleVal (Number (NumI from)), SimpleVal (Number (NumI to))] =
        return $ ByteVector $ BS.take (fromInteger (to - from)) $ BS.drop (fromInteger from) v
subByteVector [ByteVector v, SimpleVal (Number (NumS from)), SimpleVal (Number (NumS to))] =
        return $ ByteVector $ BS.take (to - from) $ BS.drop from v
subByteVector [ByteVector v, SimpleVal (Number (NumI from)), SimpleVal (Number (NumS to))] =
        return $ ByteVector $ BS.take (to - fromInteger from) $ BS.drop (fromInteger from) v
subByteVector [ByteVector v, SimpleVal (Number (NumS from)), SimpleVal (Number (NumI to))] =
        return $ ByteVector $ BS.take (fromInteger to - from) $ BS.drop from v
subByteVector [ByteVector _, SimpleVal (Number (NumI _)), badType] = throwError $ TypeMismatch "integer" badType
subByteVector [ByteVector _, SimpleVal (Number (NumS _)), badType] = throwError $ TypeMismatch "integer" badType
subByteVector (ByteVector _ : badType : _) = throwError $ TypeMismatch "integer" badType
subByteVector (badType : _) = throwError $ TypeMismatch "byte-vector" badType
subByteVector badArgList = throwError $ NumArgs 2 badArgList

vectorToList (Vector v) = return $ List $ elems v
vectorToList badType = throwError $ TypeMismatch "vector" badType

listToVector (List l) = return $ Vector $ listArray (0, length l - 1) l
listToVector (SimpleVal (SimpleList l)) = return $ Vector $ listArray (0, length l - 1) (map fromSimple l)
listToVector badType = throwError $ TypeMismatch "list" badType

buildString :: [LispVal] -> ThrowsError LispVal
buildString [] = return $ fromSimple $ String ""
buildString [SimpleVal (Character c)] = return $ fromSimple $ String [c]
buildString (SimpleVal (Character c) : rest) = do
    cs <- buildString rest
    case cs of
        SimpleVal (String s) -> return $ fromSimple $ String $ c : s
        badType -> throwError $ TypeMismatch "character" badType
buildString [s@(SimpleVal (String _))] = return s
buildString (SimpleVal (String h) : rest) = do
    cs <- buildString rest
    case cs of
        SimpleVal (String s) -> return $ fromSimple $ String $ h ++ s
        badType -> throwError $ TypeMismatch "character" badType
buildString [badType] = throwError $ TypeMismatch "character" badType
buildString badArgList = throwError $ NumArgs 1 badArgList

makeString :: [LispVal] -> ThrowsError LispVal
makeString [SimpleVal (Number n)] = return $ fromSimple $ _makeString n ' ' ""
    where _makeString count ch s =
            if count == 0
                then String s
                else _makeString (count - 1) ch (s ++ [ch])
makeString badArgList = throwError $ NumArgs 1 badArgList

stringLength :: LispVal -> ThrowsError LispVal
stringLength (SimpleVal (String s)) = return $ fromSimple $ Number $ foldr (const (+1)) 0 s
stringLength badType = throwError $ TypeMismatch "string" badType

stringRef [SimpleVal (String v), SimpleVal (Number (NumI n))] =
        if n >= 0
           then return $ fromSimple $ Character $ v !! fromInteger n
           else return $ fromSimple $ Character $ v !! (length v - fromInteger n)
stringRef [SimpleVal (String v), SimpleVal (Number (NumS n))] =
        if n >= 0
           then return $ fromSimple $ Character $ v !! n
           else return $ fromSimple $ Character $ v !! (length v - n)
stringRef [badType] = throwError $ TypeMismatch "string integer" badType
stringRef badArgList = throwError $ NumArgs 2 badArgList

stringFind [SimpleVal (String v), SimpleVal (Character x)] =
        case findIndex (\m -> x == m) v of
           Just n -> return $ SimpleVal $ Number $ NumI $ toInteger n
           _      -> return $ SimpleVal $ Number $ NumI $ -1
stringFind [SimpleVal (String match), SimpleVal (String sub)] =
        return $ SimpleVal $ Number $ NumI $ substr sub match
  -- TODO: Advance to next match of first element instead?
  where substr pat str = findStrHelp pat str 0
        findStrHelp _ [] _ = -1
        findStrHelp pat s@(_:xs) n
          | pat == take (length pat) s = n
          | otherwise = findStrHelp pat xs (n+1)
stringFind [badType, SimpleVal (Character _)] = throwError $ TypeMismatch "string" badType
stringFind [SimpleVal (String _), badType] = throwError $ TypeMismatch "character" badType
stringFind badArgList = throwError $ NumArgs 2 badArgList

substring :: [LispVal] -> ThrowsError LispVal
substring [SimpleVal (String s), SimpleVal (Number (NumI start)), SimpleVal (Number (NumI end))] = do
    let len = fromInteger $ end - start
    let begin = fromInteger start
    return $ fromSimple $ String $ (take len . drop begin) s
substring [SimpleVal (String s), SimpleVal (Number (NumS start)), SimpleVal (Number (NumS end))] = do
    let len = end - start
    return $ fromSimple $ String $ (take len . drop start) s
substring [badType] = throwError $ TypeMismatch "string integer integer" badType
substring badArgList = throwError $ NumArgs 3 badArgList

stringExtend :: [LispVal] -> ThrowsError LispVal
stringExtend v@(SimpleVal (String _) : _) = extend' v
  where extend' :: [LispVal] -> ThrowsError LispVal
        extend' [SimpleVal (Character s)] = return $ fromSimple $ String [s]
        extend' [SimpleVal (String s)] = return $ fromSimple $ String s
        extend' [s] = throwError $ TypeMismatch "string/character" s
        extend' (SimpleVal (String st) : sts) = do
          rest <- extend' sts
          case rest of
              SimpleVal (String s) -> return $ fromSimple $ String $ st ++ s
              SimpleVal (Character c) -> return $ fromSimple $ String $ st ++ [c]
              elsewise -> throwError $
                            TypeMismatch "string/character" elsewise
        extend' (a : _) = throwError $ TypeMismatch "string/character" a
        extend' x = throwError $ InternalError $ "this should not happen" ++ show x
stringExtend [badType] = throwError $ NumArgs 2 [badType]
stringExtend (badType : _) = throwError $ TypeMismatch "string" badType
stringExtend badArgList = throwError $ BadSpecialForms "Unable to process" badArgList

listAppend :: [LispVal] -> ThrowsError LispVal
listAppend (vec@(List _) : t) = append' [vec] t
  where append' :: [LispVal] -> [LispVal] -> ThrowsError LispVal
        append' [] [v] = return $ List [v]
        append' [x] [] = return x
        append' [List x] (st : sts) = do
          rest <- append' [] sts
          case rest of
              List s -> return $ List $ x ++ (st : s)
              elsewise -> throwError $ TypeMismatch "list/element" elsewise
        append' [] (st : sts) = do
          rest <- append' [] sts
          case rest of
              List s -> return $ List $ st : s
              elsewise -> throwError $ TypeMismatch "list/element" elsewise
        append' [] [] = return $ List []
        append' x y = throwError $ InternalError $ "This should not happen: " ++ show x ++ " " ++ show y
listAppend [badType] = throwError $ NumArgs 2 [badType]
listAppend (badType : _) = throwError $ TypeMismatch "list" badType
listAppend badArgList = throwError $ BadSpecialForms "Unable to process" badArgList

vectorAppend :: [LispVal] -> ThrowsError LispVal
vectorAppend (vec@(Vector _) : t) = append' [vec] t
  where append' :: [LispVal] -> [LispVal] -> ThrowsError LispVal
        append' [] [x] = return $ Vector $ listArray (0, 0) [x]
        append' [x@(Vector _)] [] = return x
        append' [Vector x] (st : sts) = do
          rest <- append' [] sts
          let ast = elems x
          case rest of
              Vector s ->
                  let z = st : elems s
                  in return $ Vector $
                              listArray (0, (length ast + length z) - 1) (ast ++ z)
              elsewise -> throwError $ TypeMismatch "vector/element" elsewise
        append' [] (st : sts) = do
          rest <- append' [] sts
          case rest of
              Vector s -> return $ Vector $
                          listArray (0, length (elems s)) (st : elems s)
              elsewise -> throwError $ TypeMismatch "vector/element" elsewise
        append' [] [] = return $ Vector $ listArray (0, -1) []
        append' x y = throwError $ InternalError $ "This should not happen" ++ show x ++ " " ++ show y
vectorAppend [badType] = throwError $ NumArgs 2 [badType]
vectorAppend (badType : _) = throwError $ TypeMismatch "vector" badType
vectorAppend badArgList = throwError $ BadSpecialForms "Unable to process" badArgList

byteVectorAppend :: [LispVal] -> ThrowsError LispVal
byteVectorAppend = append'
  where append' :: [LispVal] -> ThrowsError LispVal
        append' [x@(ByteVector _)] = return x
        append' [] = return $ ByteVector BS.empty
        append' [SimpleVal (Number (NumI n))] = return $ ByteVector $ BS.singleton (fromInteger n :: Word8)
        append' [SimpleVal (Number (NumS n))] = return $ ByteVector $ BS.singleton (fromIntegral n :: Word8)
        append' [x] = throwError $ TypeMismatch "bytevector/element" x
        append' (ByteVector st : sts) = do
          rest <- append' sts
          case rest of
              ByteVector s -> return $ ByteVector $ BS.append st s
              elsewise -> throwError $ TypeMismatch "bytevector/element" elsewise
        append' (SimpleVal (Number (NumI st)) : sts) = do
          rest <- append' sts
          case rest of
              ByteVector s -> return $ ByteVector $ BS.cons (fromInteger st :: Word8) s
              elsewise -> throwError $ TypeMismatch "bytevector/element" elsewise
        append' (SimpleVal (Number (NumS st)) : sts) = do
          rest <- append' sts
          case rest of
              ByteVector s -> return $ ByteVector $ BS.cons (fromIntegral st :: Word8) s
              elsewise -> throwError $ TypeMismatch "bytevector/element" elsewise
        append' (x : _) = throwError $ TypeMismatch "bytevector/element" x

allAppend :: [LispVal] -> ThrowsError LispVal
allAppend v@[SimpleVal (String _), _] = stringExtend v
allAppend v@(SimpleVal (String _) : _) = stringExtend v
allAppend v@[List _, _] = listAppend v
allAppend v@(List _ : _) = listAppend v
allAppend v@[Vector _, _] = vectorAppend v
allAppend v@(Vector _ : _) = vectorAppend v
allAppend v@[ByteVector _, _] = byteVectorAppend v
allAppend v@(ByteVector _ : _) = byteVectorAppend v
allAppend [badType] = throwError $ NumArgs 2 [badType]
allAppend (badType : _) = throwError $ TypeMismatch "string/list/vector/byte-vector" badType
allAppend badArgList = throwError $ BadSpecialForms "Unable to process" badArgList

listExtend :: [LispVal] -> ThrowsError LispVal
listExtend l@(List _ : _) = append' l
  where append' :: [LispVal] -> ThrowsError LispVal
        append' [v@(List _)] = return v
        append' [v] = return $ List [v]
        append' [] = return $ List []
        append' (List st : sts) = do
          rest <- append' sts
          case rest of
              List s -> return $ List $ st ++ s
              elsewise -> throwError $ TypeMismatch "list/element" elsewise
        append' (st : sts) = do
          rest <- append' sts
          case rest of
              List s -> return $ List $ st : s
              elsewise -> throwError $ TypeMismatch "list/element" elsewise
listExtend [badType] = throwError $ NumArgs 2 [badType]
listExtend (badType : _) = throwError $ TypeMismatch "list" badType
listExtend badArgList = throwError $ BadSpecialForms "Unable to process" badArgList

vectorExtend :: [LispVal] -> ThrowsError LispVal
vectorExtend v@(Vector _: _) = append' v
  where append' :: [LispVal] -> ThrowsError LispVal
        append' [x@(Vector _)] = return x
        append' [x] = return $ Vector $ listArray (0, 0) [x]
        append' (Vector st : sts) = do
          rest <- append' sts
          case rest of
              Vector s -> return $ Vector $
                          listArray (0, length (elems s) + length (elems st) - 1)
                                    (elems st ++ elems s)
              elsewise -> throwError $ TypeMismatch "vector/element" elsewise
        append' (st : sts) = do
          rest <- append' sts
          case rest of
              Vector s -> return $ Vector $
                          listArray (0, length (elems s)) (st : elems s)
              elsewise -> throwError $ TypeMismatch "vector/element" elsewise
        append' x = throwError $ InternalError $ "This should not happen: " ++ show x
vectorExtend [badType] = throwError $ NumArgs 2 [badType]
vectorExtend (badType : _) = throwError $ TypeMismatch "vector" badType
vectorExtend badArgList = throwError $ BadSpecialForms "Unable to process" badArgList

allExtend :: [LispVal] -> ThrowsError LispVal
allExtend v@[SimpleVal (String _), _] = stringExtend v
allExtend v@(SimpleVal (String _) : _) = stringExtend v
allExtend v@[List _, _] = listExtend v
allExtend v@(List _ : _) = listExtend v
allExtend v@[Vector _, _] = vectorExtend v
allExtend v@(Vector _ : _) = vectorExtend v
allExtend v@[ByteVector _, _] = byteVectorAppend v
allExtend v@(ByteVector _ : _) = byteVectorAppend v
allExtend [badType] = throwError $ NumArgs 2 [badType]
allExtend (badType : _) = throwError $ TypeMismatch "string/list/vector/bytevector" badType
allExtend badArgList = throwError $ BadSpecialForms "Unable to process" badArgList

inspectDoc :: String
inspectDoc = "inspect the source code of a zepto object\n\
\n\
  params:\n\
    - obj: the object to inspect\n\
  complexity: depends on the complexity of the object\n\
  returns: a string representation of the object"

inspect :: [LispVal] -> ThrowsError LispVal
inspect [a] = return $ fromSimple $ String $ pprint a
inspect a = throwError $ NumArgs 2 a

pprint :: LispVal -> String
pprint (List x) = "(" ++ unwords (map pprint x) ++ ")"
pprint (DottedList x y) = "(" ++ unwords (map pprint x) ++ " . " ++ pprint y ++ ")"
pprint (SimpleVal (Atom x)) = x
pprint (SimpleVal (Character x)) = "#\\" ++ [x]
pprint (SimpleVal (Bool x)) = if x then "#t" else "#f"
pprint (SimpleVal (Nil _)) = "nil"
pprint (SimpleVal (SimpleList x)) = "simple(" ++ unwords (map (pprint . fromSimple) x) ++ ")"
pprint (Vector x) = "{" ++ unwords (map pprint (elems x)) ++ "}"
pprint (ByteVector x) = "u8{" ++ show x ++ "}"
pprint (SimpleVal (String x)) = "\"" ++ x ++ "\""
pprint (SimpleVal (Regex x)) = "/" ++ show x ++ "/"
pprint (SimpleVal (Number x)) = show x
pprint (HashMap _) = "<hashmap>"
pprint (PrimitiveFunc s _) = "<primitive: " ++ s ++ ">"
pprint (IOFunc s _) = "<io primitive: " ++ s ++ ">"
pprint (EvalFunc s _) = "<eval primitive: " ++ s ++ ">"
pprint (Environ _) = "<environment>"
pprint (Error _) = "<error>"
pprint (Func s f) = "(define (" ++ s ++ " " ++ printFunc f ++ ")"
  where printFunc (LispFun p v b _ d) = let x = unwords p
                                            y = unwords (map pprint b)
                                        in case v of
                                          Nothing -> x ++") \"" ++ d ++ "\" (" ++ y ++ ")"
                                          Just m -> x ++ " . " ++ m ++ ") \"" ++ d ++ "\" (" ++ y ++ ")"
pprint (Port _) = "<port>"
pprint (Cont _) = "<continuation>"
pprint (Pointer _ _) = "<pointer>"
pprint (Opaque _) = "<opaque>"
pprint (ListComprehension a b c d) =
  let x = "[" ++ pprint a ++ " | " ++ pprint b ++ " <- " ++ pprint c
  in case d of
    Nothing -> x ++ "]"
    Just m -> x ++ ", " ++ pprint m ++ "]"
pprint (HashComprehension a b c d) =
  let x = "{" ++ pprint (fst a) ++ " " ++ pprint (snd a) ++
          " | " ++ pprint (fst b) ++ " " ++ pprint (snd b) ++ " <- " ++ pprint c
  in case d of
    Nothing -> x ++ "}"
    Just m -> x ++ ", " ++ pprint m ++ "}"

stringToByteVector :: LispVal -> ThrowsError LispVal
stringToByteVector (SimpleVal (String str)) = return $ ByteVector $ BS.fromString str
stringToByteVector x = throwError $ TypeMismatch "string" x

byteVectorToString :: LispVal -> ThrowsError LispVal
byteVectorToString (ByteVector bv) = return $ fromSimple $ String $ BS.toString bv
byteVectorToString x = throwError $ TypeMismatch "byte-vector" x
