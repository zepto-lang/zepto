module Zepto.Primitives.VersionPrimitives where
import Zepto.Types

version :: [Integer]
version = [0, 9, 6]

ghcDoc :: String
ghcDoc = "get the GHC version as integer.\n\
\n\
\n\
  complexity: O(1)\n\
  returns: an integer representing the version of GHC"

getGhc :: ThrowsError LispVal
getGhc = return $ fromSimple $ Number $ NumI (__GLASGOW_HASKELL__::Integer)

versionDoc :: String
versionDoc = "get the zepto version as a list of integers.\n\
\n\
  complexity: O(1)\n\
  returns: a list of the form [major, minor, patch]"

getVersion :: ThrowsError LispVal
getVersion = return $ List $ fmap (fromSimple . Number . NumI) version
