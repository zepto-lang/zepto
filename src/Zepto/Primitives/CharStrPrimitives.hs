module Zepto.Primitives.CharStrPrimitives where
import Data.Char
import Data.List (findIndex)
import Data.List.Utils
import Control.Monad.Except

import Zepto.Types
import Zepto.Primitives.ListPrimitives

stringToList :: LispVal -> ThrowsError LispVal
stringToList (SimpleVal (String s)) = return $ List $ fmap (fromSimple . Character) s
stringToList badType = throwError $ TypeMismatch "string" badType

listToString :: LispVal -> ThrowsError LispVal
listToString (List []) = return $ fromSimple $ String ""
listToString (List l) = buildString l
listToString badType = throwError $ TypeMismatch "list" badType

strCharConvDoc :: String -> String -> String -> String
strCharConvDoc typ form cplx = "convert a " ++ typ ++ " to " ++ form ++ " case.\n\
\n\
  params:\n\
    - elem: the " ++ typ ++ " to " ++ form ++ "case\n\
  complexity: O(" ++ cplx ++ ")\n\
  returns: the " ++ form ++ "cased " ++ typ

charDowncase :: LispVal -> ThrowsError LispVal
charDowncase (SimpleVal (Character c)) = return $ fromSimple $ Character $ toLower c
charDowncase badType = throwError $ TypeMismatch "character" badType

charUpcase :: LispVal -> ThrowsError LispVal
charUpcase (SimpleVal (Character c)) = return $ fromSimple $ Character $ toUpper c
charUpcase badType = throwError $ TypeMismatch "character" badType

stringDowncase :: LispVal -> ThrowsError LispVal
stringDowncase (SimpleVal (String c)) = return $ fromSimple $ String $ map toLower c
stringDowncase badType = throwError $ TypeMismatch "string" badType

stringUpcase :: LispVal -> ThrowsError LispVal
stringUpcase (SimpleVal (String c)) = return $ fromSimple $ String $ map toUpper c
stringUpcase badType = throwError $ TypeMismatch "string" badType

stringSubDoc :: String
stringSubDoc = "substite all occurences of a pattern <par>pat</par> in a\n\
string <par>str</par> with a pattern <par>subst</par>.\n\
\n\
  params:\n\
    - str: the string in which the patterns should be substituted\n\
    - pat: the pattern to substitute out\n\
    - subst: the pattern to substitute in\n\
  complexity: O(n)\n\
  returns: the string with all occurences of <par>pat</par> substituted"

stringSub :: [LispVal] -> ThrowsError LispVal
stringSub [SimpleVal (String source), SimpleVal (String pattern), SimpleVal (String substitute)] =
  return $ fromSimple $ String $ replace pattern substitute source
stringSub [err@_, SimpleVal (String _), SimpleVal (String _)] = throwError $ TypeMismatch "string" err
stringSub [SimpleVal (String _), err@_, SimpleVal (String _)] = throwError $ TypeMismatch "string" err
stringSub [SimpleVal (String _), SimpleVal (String _), err@_] = throwError $ TypeMismatch "string" err
stringSub badArgList = throwError $ NumArgs 3 badArgList

stringFindDoc :: String
stringFindDoc = "find a substring or character <par>fnd</par> in a string\n\
<par>str</par> and return its index or -1 if nothing was found.\n\
\n\
  params:\n\
    - str: the string to search\n\
    - fnd: the pattern to find\n\
  complexity: O(n)\n\
  returns: the index or -1 if nothing was found"

stringFind :: [LispVal] -> ThrowsError LispVal
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

substringDoc :: String
substringDoc = "get a substring of <par>str</par> between indices\n\
<par>start</par> and <par>end</par>. If <par>end</par> is more than the\n\
length of <par>str</par>, it will behave as if the parameter was equal to\n\
the length instead (i.e. it will return the complete rest).\n\
\n\
  params:\n\
    - str: the string to get a substring from\n\
    - start: the start index\n\
    - end: the end index\n\
  complexity: O(n)\n\
  returns: the substring between <par>start</par> and <par>end</par>"

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

makeStringDoc :: String
makeStringDoc = "create an empty string. Optionally takes a number\n\
<par>n</par>, in which case a string containing only spaces of length\n\
<par>n</par> will be created.\n\
\n\
  params:\n\
    - n: the length of the string to create (optional)\n\
  complexity: O(n)\n\
  returns: an empty string or a string of spaces of length <par>n</par>"

makeString :: [LispVal] -> ThrowsError LispVal
makeString [SimpleVal (Number n)] = return $ fromSimple $ _makeString n ' ' ""
    where _makeString count ch s =
            if count == 0
                then String s
                else _makeString (count - 1) ch (s ++ [ch])
makeString [] = return $ fromSimple $ String ""
makeString badArgList = throwError $ NumArgs 1 badArgList

stringLength :: LispVal -> ThrowsError LispVal
stringLength (SimpleVal (String s)) = return $ fromSimple $ Number $ NumI $ fromIntegral $ length s
stringLength badType = throwError $ TypeMismatch "string" badType

stringRef :: [LispVal] -> ThrowsError LispVal
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

