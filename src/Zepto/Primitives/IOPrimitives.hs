module Zepto.Primitives.IOPrimitives where
import Control.Monad (liftM)
import Control.Monad.Except (liftIO, throwError)
import Crypto.Number.Generate (generateBetween)
import System.Directory (doesFileExist, getHomeDirectory, getCurrentDirectory,
                         setCurrentDirectory, createDirectory,
                         getDirectoryContents, createDirectoryIfMissing)
import System.Environment (getEnv, setEnv)
import System.Exit
import System.IO
import System.IO.Error (tryIOError)
import System.Process (readProcessWithExitCode)
import System.Clock

import qualified System.IO.Strict as S (readFile)
import qualified Data.ByteString as BS (readFile)

import Zepto.Types

import Paths_zepto

changeDirDoc :: String
changeDirDoc = "changes directory of the current program.\n\
\n\
  params:\n\
    - dir: the directory to change to\n\
  complexity: O(1)\n\
  returns: boolean"

changeDir :: LispVal -> IOThrowsError LispVal
changeDir (SimpleVal (String dir)) = do
  _ <- liftIO $ setCurrentDirectory dir
  return $ fromSimple $ Bool True
changeDir x = throwError $ TypeMismatch "string" x

getHomeDirDoc :: String
getHomeDirDoc = "get user's home directory.\n\
\n\
  complexity: O(1)\n\
  returns: the home directory as string"

getHomeDir :: IOThrowsError LispVal
getHomeDir = do
  dir <- liftIO getHomeDirectory
  return $ fromSimple $ String dir

zeptoDirDoc :: String
zeptoDirDoc = "get zepto installation directory.\n\
\n\
  complexity: O(1)\n\
  returns: the directory as string"

getZeptoDir :: IOThrowsError LispVal
getZeptoDir = do
  dir <- liftIO $ getDataFileName ""
  return $ fromSimple $ String dir

getCurrentDirDoc :: String
getCurrentDirDoc = "get the current directory.\n\
\n\
  complexity: O(1)\n\
  returns: the current directory as string"

getCurrentDir :: IOThrowsError LispVal
getCurrentDir = do
  dir <- liftIO getCurrentDirectory
  return $ fromSimple $ String dir

openDoc :: String -> String
openDoc mode = "open a file for " ++ mode ++ ".\n\
\n\
  params:\n\
    - filename: the filename of the file to open\n\
  complexity: O(1)\n\
  returns: the file port"

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [SimpleVal (String filename)] = do
         fex <- liftIO $ doesFileExist filename
         if not fex && mode == ReadMode
           then return $ fromSimple $ Bool False
           else liftM Port $ liftIO $ openFile filename mode
makePort _ badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

closeFDoc :: String -> String
closeFDoc mode = "close a file that was opened for " ++ mode ++ ".\n\
\n\
  params:\n\
    - port: the port to close\n\
  complexity: O(1)\n\
  returns: nil"


closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (fromSimple (Bool True))
closePort _ = return $ fromSimple $ Bool False

displayDoc :: String
displayDoc = "write <par>obj</par> to file <par>f</par>. Does not append newline.\n\
\n\
  params:\n\
    - obj: the object to write\n\
    - file: the file to write to; also accepts <zepto>:stdout</zepto> or <zepto>:stderr</zepto> (optional, defaults to standard output)\n\
  complexity: O(1)\n\
  returns: nil"

writeDoc :: String
writeDoc = "write <par>obj</par> to file <par>f</par>. Appends newline.\n\
\n\
  params:\n\
    - obj: the object to write\n\
    - file: the file to write to; also accepts <zepto>:stdout</zepto> or <zepto>:stderr</zepto> (optional, defaults to standard output)\n\
  complexity: O(1)\n\
  returns: nil"

writeProc :: (Handle -> LispVal -> IO a) -> [LispVal] -> IOThrowsError LispVal
writeProc fun [obj] = writeProc fun [obj, Port stdout]
writeProc fun [obj, SimpleVal (Atom ":stdout")] = writeProc fun [obj, Port stdout]
writeProc fun [obj, SimpleVal (Atom ":stderr")] = writeProc fun [obj, Port stderr]
writeProc fun [obj, Port port] = do
      out <- liftIO $ tryIOError (liftIO $ fun port obj)
      case out of
          Left _ -> throwError $ Default "IO Error writing to port"
          Right _ -> return $ fromSimple $ Nil ""
writeProc fun [obj, a@(SimpleVal (Atom ":flush"))] = writeProc fun [obj, Port stdout, a]
writeProc fun [obj, SimpleVal (Atom ":stdout"), a@(SimpleVal (Atom ":flush"))] = writeProc fun [obj, Port stdout, a]
writeProc fun [obj, SimpleVal (Atom ":stderr"), a@(SimpleVal (Atom ":flush"))] = writeProc fun [obj, Port stderr, a]
writeProc fun [obj, Port port, SimpleVal (Atom ":flush")] = do
      out <- liftIO $ tryIOError (liftIO $ fun port obj)
      case out of
          Left _ -> throwError $ Default "IO Error writing to port"
          Right _ -> do
              flush <- liftIO $ tryIOError (liftIO $ hFlush port)
              case flush of
                Left _ -> throwError $ Default "IO Error flushing port"
                Right _ -> return $ fromSimple $ Nil ""
writeProc _ [] = throwError $ NumArgs 1 []
writeProc _ badArgs = throwError $ BadSpecialForm "Cannot evaluate " $ head badArgs

writeCharDoc :: String
writeCharDoc = "write <par>obj</par> to a file <par>f</par>, defaults to the\n\
standard output.\n\
\n\
  params:\n\
    - obj: the object to write\n\
    - f: the file to write to; also accepts <zepto>:stdout</zepto> and <zepto>:stderr</zepto> (optional)\n\
  complexity: O(1)\n\
  returns: nil"

writeCharProc :: [LispVal] -> IOThrowsError LispVal
writeCharProc [obj] = writeCharProc [obj, Port stdout]
writeCharProc [obj, SimpleVal (Atom ":stdout")] = writeCharProc [obj, Port stdout]
writeCharProc [obj, SimpleVal (Atom ":stderr")] = writeCharProc [obj, Port stderr]
writeCharProc [obj@(SimpleVal (Character _)), Port port] = do
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

readContentsDoc :: String
readContentsDoc = "read the contents of a file or port <par>f</par>.\n\
\n\
  params:\n\
    - f: the file or port to read\n\
  complexity: O(n)\n\
  returns: the contents as a string"

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [SimpleVal (String filename)] = liftM (SimpleVal . String) $ liftIO $ S.readFile filename
readContents [Port handle] = liftM (SimpleVal . String) $ liftIO $ hGetContents handle
readContents [x] = throwError $ TypeMismatch "string" x
readContents badArgs = throwError $ NumArgs 1 badArgs

readBinContentsDoc :: String
readBinContentsDoc = "read the contents of a file or port <par>f</par> as a byte vector.\n\
\n\
  params:\n\
    - f: the file or port to read\n\
  complexity: O(n)\n\
  returns: the contents as a byte vector"

readBinaryContents :: [LispVal] -> IOThrowsError LispVal
readBinaryContents [SimpleVal (String filename)] = liftM ByteVector $ liftIO $ BS.readFile filename
readBinaryContents [x] = throwError $ TypeMismatch "string" x
readBinaryContents badArgs = throwError $ NumArgs 1 badArgs

exitDoc :: String
exitDoc = "exit zepto.\n\
\n\
  params:\n\
    - return code: the program return code (optional); defaults to 0\n\
  complexity: O(1)\n\
  returns: this procedure exits the program"

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

timeDoc :: String
timeDoc = "get the UNIX timestamp as a pair of the seconds since the UNIX\n\
epoch and the nanoseconds since the last second.\n\
\n\
  complexity: O(1)\n\
  returns: a pair of the format <zepto>[secs, nanosecs]</zepto>"

timeProc :: IOThrowsError LispVal
timeProc = do x <- liftIO $ getTime Realtime
              return $ List [makeNum (sec x), makeNum (nsec x)]
    where makeNum n = fromSimple $ Number $ NumI $ fromIntegral n

systemDoc :: String
systemDoc = "call the command <par>cmd</par> with an optional list of arguments\n\
<par>args</par>.\n\
\n\
  params:\n\
    - cmd: the command to call\n\
    - args: the list of CLI arguments (optional)\n\
  complexity: the complexity of <par>cmd</par>\n\
  returns: <zepto>[return-code, std-output, error-output]</zepto>"

systemProc :: [LispVal] -> IOThrowsError LispVal
systemProc [SimpleVal (String s)] = do
        x <- liftIO $ tryIOError $ liftIO $ readProcessWithExitCode s [] ""
        case x of
          Right el ->
            case fst3 el of
              ExitSuccess -> return $ List $ fromSimple (Number $ NumI 0) : fromSimple (String $ snd3 el) : [fromSimple $ String $ thd3 el]
              ExitFailure w -> return $ List $ fromSimple (Number $ NumI $ fromIntegral w) : fromSimple (String $ snd3 el) : [fromSimple $ String $ thd3 el]
          Left w -> throwError $ Default $ show w
systemProc [SimpleVal (String s), List given] = do
        let margs = unpackList given
        case margs of
          Right args -> do
            x <- liftIO $ tryIOError $ liftIO $ readProcessWithExitCode s args ""
            case x of
              Right el ->
                case fst3 el of
                  ExitSuccess -> return $ List $ fromSimple (Number $ NumI 0) : fromSimple (String $ snd3 el) : [fromSimple $ String $ thd3 el]
                  ExitFailure w -> return $ List $ fromSimple (Number $ NumI $ fromIntegral w) : fromSimple (String $ snd3 el) : [fromSimple $ String $ thd3 el]
              Left w -> throwError $ Default $ show w
          Left err -> throwError $ TypeMismatch "string" err
    where unpackList :: [LispVal] -> Either LispVal [String]
          unpackList pack =
            if all isString pack
              then Right $ map unpack' pack
              else Left $ notString pack
          isString (SimpleVal (String _)) = True
          isString _ = False
          unpack' (SimpleVal (String e)) = e
          unpack' _ = ""
          notString [] = fromSimple $ Nil ""
          notString (SimpleVal (String _):l) = notString l
          notString (err:_) = err
systemProc [x] = throwError $ TypeMismatch "string" x
systemProc badArg = throwError $ NumArgs 1 badArg

randIntDoc :: String
randIntDoc = "generate a random integer in a cryptographically secure manner.\n\
Optionally provide an upper or lower bound. If none are provided, the bounds\n\
will be the minimum and maximum bound of the integer hardware data type.\n\
If only one is provided, it is interpreted as the upper bound.\n\
\n\
  params:\n\
    - lo: the lower bound (optional)\n\
    - hi: the upper bound (optional)\n\
  complexity: O(1)\n\
  returns: a random small-int"

randIntProc :: [LispVal] -> IOThrowsError LispVal
randIntProc [] = randIntProc [fromSimple $ Number $ NumI $ toInteger (minBound :: Int),
                              fromSimple $ Number $ NumI $ toInteger (maxBound :: Int)]
randIntProc [mx] = randIntProc [fromSimple $ Number $ NumI $ toInteger (minBound :: Int), mx]
randIntProc [SimpleVal (Number (NumI mn)), SimpleVal (Number (NumI mx))] = do
  n <- liftIO $ generateBetween mn mx
  return $ fromSimple $ Number $ NumI n
randIntProc [SimpleVal (Number (NumI _)), x] = throwError $ TypeMismatch "integer" x
randIntProc [x, _] = throwError $ TypeMismatch "integer" x
randIntProc x = throwError $ NumArgs 2 x

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

getEnvDoc :: String
getEnvDoc = "get an environment variable <par>var</par>.\n\
\n\
  params:\n\
    - var: the name of the environment variable\n\
  complexity: O(1)\n\
  returns: a string of the environment variable"

getEnvProc :: LispVal -> IOThrowsError LispVal
getEnvProc (SimpleVal (String var)) = do
  val <- liftIO $ getEnv var
  return $ fromSimple $ String val
getEnvProc x = throwError $ TypeMismatch "string" x

setEnvDoc :: String
setEnvDoc = "set an environment variable <par>var</par> to <par>val</par>.\n\
\n\
  params:\n\
    - var: the name of the environment variable\n\
    - val: the value that <par>var</par> should be set to (string)\n\
  complexity: O(1)\n\
  returns: nil"

setEnvProc :: [LispVal] -> IOThrowsError LispVal
setEnvProc [SimpleVal (String var), SimpleVal (String val)] = do
  _ <- liftIO $ setEnv var val
  return $ fromSimple $ Nil ""
setEnvProc [SimpleVal (String _), x] = throwError $ TypeMismatch "string" x
setEnvProc [x, _] = throwError $ TypeMismatch "string" x
setEnvProc x = throwError $ NumArgs 2 x

makeDirDoc :: String
makeDirDoc = "create a directory of the name <par>path</par>.\n\
\n\
  params:\n\
    - path: the directory to create (can be absolute or relative)\n\
  complexity: O(1)\n\
  returns: nil"

makeDir :: LispVal -> IOThrowsError LispVal
makeDir (SimpleVal (String path)) = do
    _ <- liftIO $ createDirectory path
    return $ fromSimple $ Nil ""
makeDir x = throwError $ TypeMismatch "string" x

maybeMakeDirDoc :: String
maybeMakeDirDoc = "behaves exactly like <fun>os:make-dir</fun>, but does not\n\
error if the directory already exists.\n\
\n\
  params:\n\
    - path: the directory to create (can be absolute or relative)\n\
    - rec: if this is set to true, <fun>os:make-dir?</fun> will also create parents if necessary\n\
  complexity: O(n)\n\
  returns: nil"

maybeMakeDir :: [LispVal] -> IOThrowsError LispVal
maybeMakeDir [(SimpleVal (String path))] = do
    _ <- liftIO $ createDirectoryIfMissing False path
    return $ fromSimple $ Nil ""
maybeMakeDir [(SimpleVal (String path)), (SimpleVal (Bool rec))] = do
    _ <- liftIO $ createDirectoryIfMissing rec path
    return $ fromSimple $ Nil ""
maybeMakeDir [(SimpleVal (String path)), x] =
    throwError $ TypeMismatch "boolean" x
maybeMakeDir [x, _] =
    throwError $ TypeMismatch "string" x
maybeMakeDir [x] = throwError $ TypeMismatch "string" x
maybeMakeDir x = throwError $ NumArgs 2 x

lsDoc :: String
lsDoc = "return the contents of the directory at path <par>path</par>.\n\
\n\
  params:\n\
    - path: the directory to inspect\n\
  complexity: O(1)\n\
  returns: a list of directory contents"

ls :: LispVal -> IOThrowsError LispVal
ls (SimpleVal (String path)) = do
  lst <- liftIO $ getDirectoryContents path
  return $ List $ map (fromSimple . String) lst
ls x = throwError $ TypeMismatch "string" x
