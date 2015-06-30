module Zepto.Primitives.VersionPrimitives where
import Data.List (intercalate)
import Control.Monad.Except (throwError)

import Zepto.Types

version :: [Int]
version = [0, 8, 0]

versionStr :: String
versionStr = intercalate "." $ fmap show version

majorVersion :: Int
majorVersion = head version

minorVersion :: Int
minorVersion = version !! 1

patchVersion :: Int
patchVersion = version !! 2

getVersion :: [LispVal] -> ThrowsError LispVal
getVersion [] = return $ List $ fmap (fromSimple . String . show) version
getVersion badList = throwError $ NumArgs 0 badList

getVersionStr :: [LispVal] -> ThrowsError LispVal
getVersionStr [] = return $ fromSimple $ String versionStr
getVersionStr badList = throwError $ NumArgs 0 badList

getMajVersion :: [LispVal] -> ThrowsError LispVal
getMajVersion [] = return $ fromSimple $ Number $ NumI $ toInteger majorVersion
getMajVersion badList = throwError $ NumArgs 0 badList

getMinVersion :: [LispVal] -> ThrowsError LispVal
getMinVersion [] = return $ fromSimple $ Number $ NumI $ toInteger minorVersion
getMinVersion badList = throwError $ NumArgs 0 badList

getPatchVersion :: [LispVal] -> ThrowsError LispVal
getPatchVersion [] = return $ fromSimple $ Number $ NumI $ toInteger patchVersion
getPatchVersion badList = throwError $ NumArgs 0 badList

