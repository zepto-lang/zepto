module Zepto.Primitives.RegexPrimitives where

import Control.Monad.Except (throwError)
import Data.ByteString.Char8 (unpack)
import Text.Regex.PCRE.Heavy
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

regexMatchesDoc :: String
regexMatchesDoc = "matches a regex against a a string.\n\
\n\
  params:\n\
    - r: the regex against which we match\n\
    - check: the string to match\n\
  complexity: heavily dependent on the input regex\n\
  returns: a boolean"

regexMatches :: [LispVal] -> ThrowsError LispVal
regexMatches [SimpleVal (Regex r), SimpleVal (String pattern)] =
  return $ fromSimple $ Bool $ pattern =~ r
regexMatches [SimpleVal (Regex _), x] = throwError $ TypeMismatch "string" x
regexMatches [x, SimpleVal (String _)] = throwError $ TypeMismatch "regex" x
regexMatches x = throwError $ NumArgs 2 x

regexScanDoc :: String
regexScanDoc = "scans a string <par>pattern</par> for occurences of the regex <par>r</par>.\n\
\n\
  params:\n\
    - r: the regex against which we match\n\
    - check: the string to scan\n\
  complexity: heavily dependent on the input regex\n\
  returns: a list of lists of the form <zepto>[match, [groups]]</zepto>"

regexScan :: [LispVal] -> ThrowsError LispVal
regexScan [SimpleVal (Regex r), SimpleVal (String pattern)] =
  return $  List $ map convert (scan r pattern)
    where convert (str, l) = List [fromSimple $ String str,
                                   List $ map (\x -> fromSimple $ String x) l]
regexScan [SimpleVal (Regex _), x] = throwError $ TypeMismatch "string" x
regexScan [x, SimpleVal (String _)] = throwError $ TypeMismatch "regex" x
regexScan x = throwError $ NumArgs 2 x


regexScanODoc :: String
regexScanODoc = "scans a string <par>pattern</par> for occurences of the regex <par>r</par>\n\
and returns a list of lists of the start and end indices.\n\
  params:\n\
    - r: the regex against which we match\n\
    - check: the string to scan\n\
  complexity: heavily dependent on the input regex\n\
  returns: a list of lists of the form <zepto>[start, end]</zepto>"

regexScanO :: [LispVal] -> ThrowsError LispVal
regexScanO [SimpleVal (Regex r), SimpleVal (String pattern)] =
  return $  List $ map convert (scanRanges r pattern)
    where convert (range, ranges) = List [build range,
                                          List $ map build ranges]
          build (start, end) = List [fromSimple $ Number $ NumS start,
                                     fromSimple $ Number $ NumS end]
regexScanO [SimpleVal (Regex _), x] = throwError $ TypeMismatch "string" x
regexScanO [x, SimpleVal (String _)] = throwError $ TypeMismatch "regex" x
regexScanO x = throwError $ NumArgs 2 x
