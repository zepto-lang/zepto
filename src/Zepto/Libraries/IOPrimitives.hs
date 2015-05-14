module Zepto.Libraries.IOPrimitives where
import Control.Monad (liftM)
import Control.Monad.Except (liftIO, throwError)
import Data.List (find)
import System.IO
import System.IO.Error (tryIOError)

import Zepto.Types

escapeProc :: [LispVal] -> IOThrowsError LispVal
escapeProc [Number (NumI n)] = writeProc print' [String $ "\x1b[" ++ show n ++ "m"]
escapeProc [Number (NumS n)] = writeProc print' [String $ "\x1b[" ++ show n ++ "m"]
escapeProc [badArg] = throwError $ TypeMismatch "integer" badArg
escapeProc badArgList = throwError $ NumArgs 1 badArgList

colorProc :: [LispVal] -> IOThrowsError LispVal
colorProc [Atom (':' : s)] =
        case lookupColor s of
           Just found -> escapeProc $ [Number (NumI $ snd found)]
           _          -> throwError $ BadSpecialForm "Color not found" $ String s
    where lookupColor color = find (\t -> color == fst t) colors
          colors = [ ("black", 30)
                   , ("red", 31)
                   , ("green", 32)
                   , ("yellow", 33)
                   , ("blue", 34)
                   , ("magenta", 35)
                   , ("cyan", 36)
                   , ("white", 37)
                   , ("reset", 0)
                   , ("none", 0)
                   , ("", 0)
                   ]
colorProc [badArg] = throwError $ TypeMismatch "atom" badArg
colorProc badArgs = throwError $ NumArgs 1 badArgs

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

writeProc :: (Handle -> LispVal -> IO a) -> [LispVal] -> IOThrowsError LispVal
writeProc fun [obj] = writeProc fun [obj, Port stdout]
writeProc fun [obj, Atom ":stdout"] = writeProc fun [obj, Port stdout]
writeProc fun [obj, Atom ":stderr"] = writeProc fun [obj, Port stderr]
writeProc fun [obj, Port port] = do
      out <- liftIO $ tryIOError (liftIO $ fun port obj)
      case out of
          Left _ -> throwError $ Default "IO Error writing to port"
          Right _ -> return $ Nil ""
writeProc _ badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

print' :: Handle -> LispVal -> IO ()
print' port obj =
      case obj of
          String str -> hPutStr port str
          _ -> hPutStr port $ show obj

errorProc :: [LispVal] -> IOThrowsError LispVal
errorProc [obj] = liftIO $ hPrint stderr obj >> return (Nil "")
errorProc badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

