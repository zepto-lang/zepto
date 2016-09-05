module Zepto.Primitives.VersionPrimitives where
import Zepto.Types

version :: [Int]
version = [0, 9, 6]

getGhc :: ThrowsError LispVal
getGhc = return $ fromSimple $ Number $ NumI (__GLASGOW_HASKELL__::Integer)

getVersion :: ThrowsError LispVal
getVersion = return $ List $ fmap (fromSimple . String . show) version
