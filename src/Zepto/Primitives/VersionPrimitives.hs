module Zepto.Primitives.VersionPrimitives where
import Data.List (intercalate)

import Zepto.Types

version :: [Int]
version = [0, 9, 2]

versionStr :: String
versionStr = intercalate "." $ fmap show version

majorVersion :: Int
majorVersion = head version

minorVersion :: Int
minorVersion = version !! 1

patchVersion :: Int
patchVersion = version !! 2

getVersion :: ThrowsError LispVal
getVersion = return $ List $ fmap (fromSimple . String . show) version

getVersionStr :: ThrowsError LispVal
getVersionStr = return $ fromSimple $ String versionStr

getMajVersion :: ThrowsError LispVal
getMajVersion = return $ fromSimple $ Number $ NumI $ toInteger majorVersion

getMinVersion :: ThrowsError LispVal
getMinVersion = return $ fromSimple $ Number $ NumI $ toInteger minorVersion

getPatchVersion :: ThrowsError LispVal
getPatchVersion = return $ fromSimple $ Number $ NumI $ toInteger patchVersion
