module Zepto.Primitives.SocketPrimitives where
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Except (throwError, liftIO)

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS (recv, send)
                                                   -- send, sendAll, sendTo, sendAllTo, recvFrom)

import Zepto.Types

lookupType :: String -> NS.SocketType
lookupType s = case s of
                "no-socket-type" -> NS.NoSocketType
                "stream"         -> NS.Stream
                "datagram"       -> NS.Datagram
                "raw"            -> NS.Raw
                "rdm"            -> NS.RDM
                "seq-packet"     -> NS.SeqPacket
                _                -> NS.NoSocketType

typeStr :: NS.SocketType -> String
typeStr NS.NoSocketType = "no-socket-type"
typeStr NS.Stream = "stream"
typeStr NS.Datagram = "datagram"
typeStr NS.Raw = "raw"
typeStr NS.RDM = "rdm"
typeStr NS.SeqPacket = "seq-packet"

socket :: [LispVal] -> IOThrowsError LispVal
socket [f@(SimpleVal (Number (NumS fam))),
        t@(SimpleVal (String type')),
        p@(SimpleVal (Number (NumS protonum)))] = do
        sock <- liftIO (NS.socket (NS.unpackFamily (fromIntegral fam))
                              (lookupType type')
                              (fromIntegral protonum))
        let fd = fromSimple $ Number $ NumS $ fromIntegral (NS.fdSocket sock)
        return $ List [fd, f, t, p]
socket [x, SimpleVal (String _), SimpleVal (Number (NumS _))] =
        throwError $ TypeMismatch "small-int" x
socket [_, x, SimpleVal (Number (NumS _))] = throwError $ TypeMismatch "string" x
socket [_, _, x] = throwError $ TypeMismatch "small-int" x
socket [t@(SimpleVal (String type'))] = do
        sock <- liftIO (NS.socket (NS.unpackFamily 2) (lookupType type') 0)
        let fd = fromSimple $ Number $ NumS $ fromIntegral (NS.fdSocket sock)
        let sp = \ x -> fromSimple $ Number $ NumS x
        return $ List [fd, sp 2, t, sp 0]
socket [t] = throwError $ TypeMismatch "string" t
socket x = throwError $ NumArgs 3 x

ls2AddrInfo :: LispVal -> Maybe NS.AddrInfo
ls2AddrInfo (List [(List flags),
                      (SimpleVal (Number (NumS family))),
                      (SimpleVal (String type')),
                      (SimpleVal (Number (NumS proto))),
                      address,
                      (SimpleVal cname)]) =
          Just $ NS.AddrInfo (map parsefl flags)
                          (NS.unpackFamily $ fromIntegral family)
                          (lookupType type')
                          (fromIntegral proto)
                          (parseaddr address)
                          (parsec cname)
      where parsec (String s) = Just s
            parsec _ = Nothing
            fromS (SimpleVal (Number (NumS x))) = fromIntegral x
            fromS _ = 0
            parseaddr (SimpleVal (Number (NumS x))) = NS.SockAddrCan (fromIntegral x)
            parseaddr (List [x, y]) = NS.SockAddrInet (fromS x) (fromS y)
            parseaddr (List [w, x, List [a, b, c, d], z]) =
                NS.SockAddrInet6 (fromS w) (fromS x) (fromS a, fromS b, fromS c, fromS d) (fromS z)
            parseaddr (SimpleVal (String x)) = NS.SockAddrUnix x
            parseaddr _ = NS.SockAddrUnix ""
            parsefl (SimpleVal (String f)) = read f
            parsefl _ = read ""
ls2AddrInfo _ = Nothing

addrInfo2Ls :: NS.AddrInfo -> LispVal
addrInfo2Ls (NS.AddrInfo flags family type' proto address cname) =
        List
          [
            parsefl flags
          , fromSimple $ Number $ NumS $ fromIntegral $ NS.packFamily family
          , fromSimple $ String $ typeStr type'
          , fromSimple $ Number $ NumS $ fromIntegral proto
          , parseaddr address
          , parsec cname
          ]
    where parsec (Just s) = fromSimple $ String s
          parsec Nothing  = fromSimple $ Nil ""
          toS x = fromSimple $ Number $ NumS $ fromIntegral x
          parseaddr (NS.SockAddrCan x) = fromSimple $ Number $ NumS $ fromIntegral x
          parseaddr (NS.SockAddrInet x y) = List [toS x, toS y]
          parseaddr (NS.SockAddrInet6 w x (a, b, c, d) z) =
              List [toS w, toS x, List $ map toS [a,b,c,d], toS z]
          parseaddr (NS.SockAddrUnix x) = fromSimple $ String x
          parsefl f = List $ map (\x -> fromSimple $ String $ show x) f

getAddrInfo :: [LispVal] -> IOThrowsError LispVal
getAddrInfo [addrInfo@(List _), (SimpleVal n), (SimpleVal s)] = do
        let info = ls2AddrInfo addrInfo
        resolved <- liftIO $ NS.getAddrInfo info (toMaybe n) (toMaybe s)
        return $ addrInfo2Ls $ head resolved
    where toMaybe (String sth) = Just sth
          toMaybe _ = Nothing
getAddrInfo [(SimpleVal n), (SimpleVal s)] = do
        resolved <- liftIO $ NS.getAddrInfo Nothing (toMaybe n) (toMaybe s)
        return $ addrInfo2Ls $ head resolved
    where toMaybe (String sth) = Just sth
          toMaybe _ = Nothing
getAddrInfo [x, (SimpleVal _), (SimpleVal _)] = throwError $ TypeMismatch "list" x
getAddrInfo [x, (SimpleVal _)] = throwError $ TypeMismatch "string/nil" x
getAddrInfo [_, x] = throwError $ TypeMismatch "string/nil" x
getAddrInfo [_, x, (SimpleVal _)] = throwError $ TypeMismatch "string/nil" x
getAddrInfo [_, _, x] = throwError $ TypeMismatch "string/nil" x
getAddrInfo x = throwError $ NumArgs 2 x

connect :: [LispVal] -> IOThrowsError LispVal
connect [List [(SimpleVal (Number (NumS fd))),
               (SimpleVal (Number (NumS fam))),
               (SimpleVal (String type')),
               (SimpleVal (Number (NumS protonum)))],
         addrInfo@(List _)] = do
        let info' = ls2AddrInfo addrInfo
        case info' of
          Just info -> do
            status <- liftIO (newMVar (NS.NotConnected))
            let sock = NS.MkSocket (fromIntegral fd)
                                   (NS.unpackFamily (fromIntegral fam))
                                   (lookupType type')
                                   (fromIntegral protonum)
                                   status
            _ <- liftIO $ NS.connect sock (getSockAddr info)
            return $ fromSimple $ Nil ""
          Nothing -> throwError $ Default "Could not construct addr-info type"
    where getSockAddr (NS.AddrInfo _ _ _ _ a _) = a
connect [x, (List _)] = throwError $ TypeMismatch "list" x
connect [_, x] = throwError $ TypeMismatch "list" x
connect x = throwError $ NumArgs 2 x

recv :: [LispVal] -> IOThrowsError LispVal
recv [List [(SimpleVal (Number (NumS fd))),
            (SimpleVal (Number (NumS fam))),
            (SimpleVal (String type')),
            (SimpleVal (Number (NumS protonum)))],
      (SimpleVal (Number (NumS n)))] = do
        status <- liftIO (newMVar (NS.Connected))
        let sock = NS.MkSocket (fromIntegral fd)
                               (NS.unpackFamily (fromIntegral fam))
                               (lookupType type')
                               (fromIntegral protonum)
                               status
        datum <- liftIO $ NBS.recv sock n
        return $ ByteVector datum
recv [x, (SimpleVal (Number (NumS _)))] = throwError $ TypeMismatch "list" x
recv [_, x] = throwError $ TypeMismatch "small-int" x
recv x = throwError $ NumArgs 2 x

send :: [LispVal] -> IOThrowsError LispVal
send [List [(SimpleVal (Number (NumS fd))),
            (SimpleVal (Number (NumS fam))),
            (SimpleVal (String type')),
            (SimpleVal (Number (NumS protonum)))],
      ByteVector b] = do
        status <- liftIO (newMVar (NS.Connected))
        let sock = NS.MkSocket (fromIntegral fd)
                               (NS.unpackFamily (fromIntegral fam))
                               (lookupType type')
                               (fromIntegral protonum)
                               status
        datum <- liftIO $ NBS.send sock b
        return $ fromSimple $ Number $ NumS datum
send [x, ByteVector _] = throwError $ TypeMismatch "list" x
send [_, x] = throwError $ TypeMismatch "byte-vector" x
send x = throwError $ NumArgs 2 x
