module Zepto.Primitives.IOPrimitives where
import Control.Monad (liftM)
import Control.Monad.Except (liftIO, throwError)
import Data.List (find)
import System.Exit
import System.IO
import System.IO.Error (tryIOError)
import System.Process (system)

import Zepto.Types

escapeProc :: [LispVal] -> IOThrowsError LispVal
escapeProc [SimpleVal (Number (NumI n))] = writeProc print' [fromSimple $ String $ "\x1b[" ++ show n ++ "m"]
escapeProc [SimpleVal (Number (NumS n))] = writeProc print' [fromSimple $ String $ "\x1b[" ++ show n ++ "m"]
escapeProc [badArg] = throwError $ TypeMismatch "integer" badArg
escapeProc badArgList = throwError $ NumArgs 1 badArgList

colorProc :: [LispVal] -> IOThrowsError LispVal
colorProc [SimpleVal (Atom (':' : s))] =
        case lookupColor s of
           Just found -> escapeProc $ [SimpleVal (Number (NumI $ snd found))]
           _          -> throwError $ BadSpecialForm "Color not found" $ fromSimple $ String s
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
makePort mode [SimpleVal (String filename)] = liftM Port $ liftIO $ openFile filename mode
makePort _ badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (fromSimple (Bool True))
closePort _ = return $ fromSimple $ Bool False

writeProc :: (Handle -> LispVal -> IO a) -> [LispVal] -> IOThrowsError LispVal
writeProc fun [obj] = writeProc fun [obj, Port stdout]
writeProc fun [obj, SimpleVal (Atom ":stdout")] = writeProc fun [obj, Port stdout]
writeProc fun [obj, SimpleVal (Atom ":stderr")] = writeProc fun [obj, Port stderr]
writeProc fun [obj@(SimpleVal (Character _)), Port port] = do
      out <- liftIO $ tryIOError (liftIO $ fun port obj)
      case out of
          Left _ -> throwError $ Default "IO Error writing to port"
          Right _ -> return $ fromSimple $ Nil ""
writeProc _ [] = throwError $ NumArgs 1 []
writeProc _ badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

writeCharProc :: [LispVal] -> IOThrowsError LispVal
writeCharProc [obj] = writeCharProc [obj, Port stdout]
writeCharProc [obj, SimpleVal (Atom ":stdout")] = writeCharProc [obj, Port stdout]
writeCharProc [obj, SimpleVal (Atom ":stderr")] = writeCharProc [obj, Port stderr]
writeCharProc [obj, Port port] = do
      out <- liftIO $ tryIOError (liftIO $ hPutStr port (show obj))
      case out of
          Left _ -> throwError $ Default "IO Error writing to port"
          Right _ -> return $ fromSimple $ Nil ""
writeCharProc [wrong, _] = throwError $ TypeMismatch "char" wrong
writeCharProc [] = throwError $ NumArgs 1 []
writeCharProc badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

print' :: Handle -> LispVal -> IO ()
print' port obj =
      case obj of
          SimpleVal (String str) -> hPutStr port str
          _ -> hPutStr port $ show obj

errorProc :: [LispVal] -> IOThrowsError LispVal
errorProc [obj] = liftIO $ hPrint stderr obj >> return (fromSimple (Nil ""))
errorProc badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [SimpleVal (String filename)] = liftM (SimpleVal . String) $ liftIO $ readFile filename
readContents badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

exitProc :: [LispVal] -> IOThrowsError LispVal
exitProc [] = do _ <- liftIO $ tryIOError $ liftIO exitSuccess
                 return $ fromSimple $ Nil ""
exitProc [SimpleVal (Number (NumI 0))] = do _ <- liftIO $ tryIOError $ liftIO exitSuccess
                                            return $ fromSimple $ Nil ""
exitProc [SimpleVal (Number (NumS 0))] = do _ <- liftIO $ tryIOError $ liftIO exitSuccess
                                            return $ fromSimple $ Nil ""
exitProc [SimpleVal (Number (NumI x))] = do _ <- liftIO $ tryIOError $ liftIO $
                                              exitWith $ ExitFailure $ fromInteger x
                                            return $ fromSimple $ Nil ""
exitProc [SimpleVal (Number (NumS x))] = do _ <- liftIO $ tryIOError $ liftIO $
                                              exitWith $ ExitFailure x
                                            return $ fromSimple $ Nil ""
exitProc [x] = throwError $ TypeMismatch "integer" x
exitProc badArg = throwError $ NumArgs 1 badArg

systemProc :: [LispVal] -> IOThrowsError LispVal
systemProc [SimpleVal (Atom s)] = do
        x <- liftIO $ tryIOError $ liftIO $ system s
        case x of
          Right _ -> return $ fromSimple $ Number $ NumI 0
          Left w -> return $ fromSimple $ String $ show w
systemProc [SimpleVal (String s)] = do
        x <- liftIO $ tryIOError $ liftIO $ system s
        case x of
          Right _ -> return $ fromSimple $ Number $ NumI 0
          Left w -> return $ fromSimple $ String $ show w
systemProc [x] = throwError $ TypeMismatch "string" x
systemProc badArg = throwError $ NumArgs 1 badArg

