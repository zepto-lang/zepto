module Zepto.Primitives.VersionPrimitives where
import Data.List (intercalate)
import Control.Monad.Except (throwError)

import Zepto.Types

version :: [Int]
version = [0, 7, 3]

versionStr :: String
versionStr = intercalate "." $ fmap show version

majorVersion :: Int
majorVersion = head version

minorVersion :: Int
minorVersion = version !! 1

patchVersion :: Int
patchVersion = version !! 2

getVersion :: [LispVal] -> ThrowsError LispVal
getVersion [] = return $ List $ fmap (String . show) version
getVersion badList = throwError $ NumArgs 0 badList

getVersionStr :: [LispVal] -> ThrowsError LispVal
getVersionStr [] = return $ String versionStr
getVersionStr badList = throwError $ NumArgs 0 badList

getMajVersion :: [LispVal] -> ThrowsError LispVal
getMajVersion [] = return $ Number $ NumI $ toInteger majorVersion
getMajVersion badList = throwError $ NumArgs 0 badList

getMinVersion :: [LispVal] -> ThrowsError LispVal
getMinVersion [] = return $ Number $ NumI $ toInteger minorVersion
getMinVersion badList = throwError $ NumArgs 0 badList

getPatchVersion :: [LispVal] -> ThrowsError LispVal
getPatchVersion [] = return $ Number $ NumI $ toInteger patchVersion
getPatchVersion badList = throwError $ NumArgs 0 badList

