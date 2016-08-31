{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Zepto.Primitives.LoadPrimitives where

import FastString (unpackFS)
import Module (mkModuleName, moduleNameFS)

import Control.Monad.Except (throwError, liftIO)
import Data.Char (ord)
import GHCi.ObjLink
import GHC.Prim
import GHC.Ptr

import Zepto.Types

prefixUnderscore :: String
#if defined(__APPLE__) && defined(__MACH__)
prefixUnderscore = "_"
#else
prefixUnderscore = ""
#endif

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'

encode_ch :: Char -> [Char]
encode_ch c | unencodedChar c = [c]
encode_ch '('  = "ZL"
encode_ch ')'  = "ZR"
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"
encode_ch 'Z'  = "ZZ"
encode_ch 'z'  = "zz"
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '_'  = "zu"
encode_ch '%'  = "zv"
encode_ch c = 'z' : shows (ord c) "U"

encode :: [Char] -> [Char]
encode [] = ""
encode (c:cs) = encode_ch c ++ encode cs

mangleSymbol :: String -> String
mangleSymbol n = prefixUnderscore ++ encode module_part ++ "_" ++ encode "exports" ++ "_closure"
  where
        modu         = mkModuleName n
        module_part  = unpackFS (moduleNameFS modu)

loadNative :: LispVal -> IOThrowsError LispVal
loadNative (SimpleVal (String s)) = do
  liftIO $ initObjLinker
  liftIO $loadObj (s ++ ".o")
  _ret <- liftIO resolveObjs
  if _ret then do
      ptr  <- liftIO $ lookupSymbol (mangleSymbol s)
      case ptr of
        Nothing         -> return $ fromSimple $ String $ show "symbol not found"
        Just (Ptr addr) -> case addrToAny# addr of
                                   (# hval #) -> return hval
          else return $ fromSimple $ String "failed resolving the objects"
loadNative x = throwError $ TypeMismatch "string" x
