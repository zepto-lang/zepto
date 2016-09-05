module Zepto.Primitives.RegexPrimitives where

import Control.Monad.Except (throwError)
import Data.ByteString.Char8 (unpack)
import qualified Text.Regex.PCRE.Light.Base as R

import Zepto.Types

regexPatternDoc :: String
regexPatternDoc = "gets the regex pattern as a string.\n\
\n\
  params:\n\
    - r: the regex to analyze\n\
  complexity: O(n)\n\
  returns: a string representing the match"

regexPattern :: LispVal -> ThrowsError LispVal
regexPattern (SimpleVal (Regex (R.Regex _ r))) = return $ fromSimple $ String $ unpack r
regexPattern x = throwError $ TypeMismatch "regex" x
