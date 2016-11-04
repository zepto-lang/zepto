module Zepto.Primitives.SocketPrimitives where
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Except (throwError, liftIO)

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS (recv, send)
                                                   -- send, sendAll, sendTo, sendAllTo, recvFrom)

import Zepto.Types

lookupType s = case s of
                "no-socket-type" -> NS.NoSocketType
                "stream"         -> NS.Stream
                "datagram"       -> NS.Datagram
                "raw"            -> NS.Raw
                "rdm"            -> NS.RDM
                "seq-packet"     -> NS.SeqPacket
                _                -> NS.NoSocketType

socketDoc :: String
socketDoc = "create a new socket; takes a family <par>f</par>,
a type <par>t</par>and a protocol number <par>p</par>.\n\
\n\
  params:\n\
    - f: the family (small-int)\n\
    - t: the socket type (string)\n\
    - p: the protocol number (small-int)\n\
  complexity: O(1)\n\
  returns: an opaque datatype representing a socket"

socket :: [LispVal] -> IOThrowsError LispVal
socket [f@(SimpleVal (Number (NumS fam))),
        t@(SimpleVal (String type')),
        p@(SimpleVal (Number (NumS protonum)))] = do
        sock <- liftIO (NS.socket (NS.unpackFamily (fromIntegral fam))
                              (lookupType type')
                              (fromIntegral protonum))
        return $ toOpaque sock
socket [x, SimpleVal (String _), SimpleVal (Number (NumS _))] =
        throwError $ TypeMismatch "small-int" x
socket [_, x, SimpleVal (Number (NumS _))] = throwError $ TypeMismatch "string" x
socket [_, _, x] = throwError $ TypeMismatch "small-int" x
socket [t@(SimpleVal (String type'))] = do
        sock <- liftIO (NS.socket (NS.unpackFamily 2) (lookupType type') 0)
        return $ toOpaque sock
socket [t] = throwError $ TypeMismatch "string" t
socket x = throwError $ NumArgs 3 x

ls2AddrInfo :: LispVal -> Maybe NS.AddrInfo
ls2AddrInfo (List [List flags,
                   SimpleVal (Number (NumS family)),
                   SimpleVal (String type'),
                   SimpleVal (Number (NumS proto)),
                   address,
                   SimpleVal cname]) =
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

getAddrInfo :: [LispVal] -> IOThrowsError LispVal
getAddrInfo [addrInfo@(List _), SimpleVal n, SimpleVal s] = do
        let info = ls2AddrInfo addrInfo
        resolved <- liftIO $ NS.getAddrInfo info (toMaybe n) (toMaybe s)
        return $ toOpaque $ head resolved
    where toMaybe (String sth) = Just sth
          toMaybe _ = Nothing
getAddrInfo [SimpleVal n, SimpleVal s] = do
        resolved <- liftIO $ NS.getAddrInfo Nothing (toMaybe n) (toMaybe s)
        return $ toOpaque $ head resolved
    where toMaybe (String sth) = Just sth
          toMaybe _ = Nothing
getAddrInfo [x, SimpleVal _, SimpleVal _] = throwError $ TypeMismatch "list" x
getAddrInfo [x, SimpleVal _] = throwError $ TypeMismatch "string/nil" x
getAddrInfo [_, x] = throwError $ TypeMismatch "string/nil" x
getAddrInfo [_, x, SimpleVal _] = throwError $ TypeMismatch "string/nil" x
getAddrInfo [_, _, x] = throwError $ TypeMismatch "string/nil" x
getAddrInfo x = throwError $ NumArgs 2 x

connect :: [LispVal] -> IOThrowsError LispVal
connect [sock@(Opaque _),
         addrInfo@(Opaque _)] = do
        case ((fromOpaque addrInfo) :: Maybe NS.AddrInfo) of
          Just info ->
            case ((fromOpaque sock) :: Maybe NS.Socket) of
              Just socket -> do
                _ <- liftIO $ NS.connect socket (getSockAddr info)
                return $ fromSimple $ Nil ""
              Nothing -> throwError $ TypeMismatch "opaque<socket>" sock
          Nothing -> throwError $ TypeMismatch "opaque<addrinfo>" addrInfo
    where getSockAddr (NS.AddrInfo _ _ _ _ a _) = a
connect [x, List _] = throwError $ TypeMismatch "list" x
connect [_, x] = throwError $ TypeMismatch "list" x
connect x = throwError $ NumArgs 2 x

recv :: [LispVal] -> IOThrowsError LispVal
recv [sock@(Opaque _),
      SimpleVal (Number (NumS n))] = do
    case ((fromOpaque sock) :: Maybe NS.Socket) of
      Just socket -> do
        datum <- liftIO $ NBS.recv socket n
        return $ ByteVector datum
      Nothing -> throwError $ TypeMismatch "opaque<socket>" sock
recv [x, SimpleVal (Number (NumS _))] = throwError $ TypeMismatch "list" x
recv [_, x] = throwError $ TypeMismatch "small-int" x
recv x = throwError $ NumArgs 2 x

send :: [LispVal] -> IOThrowsError LispVal
send [sock@(Opaque _),
      ByteVector b] = do
    case ((fromOpaque sock) :: Maybe NS.Socket) of
      Just socket -> do
        datum <- liftIO $ NBS.send socket b
        return $ fromSimple $ Number $ NumS datum
      Nothing -> throwError $ TypeMismatch "opaque<socket>" sock
send [x, ByteVector _] = throwError $ TypeMismatch "list" x
send [_, x] = throwError $ TypeMismatch "byte-vector" x
send x = throwError $ NumArgs 2 x

bindSocket :: [LispVal] ->IOThrowsError LispVal
bindSocket [sock@(Opaque _),
            addrInfo@(Opaque _)] = do
        case ((fromOpaque addrInfo) :: Maybe NS.AddrInfo) of
          Just info ->
            case ((fromOpaque sock) :: Maybe NS.Socket) of
              Just socket -> do
                _ <- liftIO $ NS.bind socket (getSockAddr info)
                return $ fromSimple $ Nil ""
              Nothing -> throwError $ TypeMismatch "opaque<socket>" sock
          Nothing -> throwError $ TypeMismatch "opaque<addrinfo>" addrInfo
    where getSockAddr (NS.AddrInfo _ _ _ _ a _) = a
bindSocket [x, List _] = throwError $ TypeMismatch "opaque<socket>" x
bindSocket [_, x] = throwError $ TypeMismatch "opaque<addrinfo>" x
bindSocket x = throwError $ NumArgs 2 x

listen :: [LispVal] -> IOThrowsError LispVal
listen [sock@(Opaque _),
        SimpleVal (Number (NumS n))] = do
    case ((fromOpaque sock) :: Maybe NS.Socket) of
      Just socket -> do
        _ <- liftIO $ NS.listen socket n
        return $ fromSimple $ Nil ""
      Nothing -> throwError $ TypeMismatch "opaque<socket>" sock
listen [sock@(Opaque _)] =
        listen [sock, fromSimple (Number (NumS 1))]
listen [x] = throwError $ TypeMismatch "opaque<socket>" x
listen [x, SimpleVal (Number (NumS _))] = throwError $ TypeMismatch "opaque<addrinfo>" x
listen [_, x] = throwError $ TypeMismatch "small-int" x
listen x = throwError $ NumArgs 2 x

accept :: [LispVal] -> IOThrowsError LispVal
accept [sock@(Opaque _)] = do
    case ((fromOpaque sock) :: Maybe NS.Socket) of
      Just socket -> do
        (conn, addr) <- liftIO $ NS.accept socket
        return $ List [toOpaque conn, toOpaque addr]
      Nothing -> throwError $ TypeMismatch "opaque<socket>" sock
accept [x] = throwError $ TypeMismatch "opaque<socket>" x
accept x = throwError $ NumArgs 1 x

close :: [LispVal] -> IOThrowsError LispVal
close [sock@(Opaque _)] = do
    case ((fromOpaque sock) :: Maybe NS.Socket) of
      Just socket -> do
        _ <- liftIO $ NS.close socket
        return $ fromSimple $ Nil ""
      Nothing -> throwError $ TypeMismatch "opaque<socket>" sock
close [x] = throwError $ TypeMismatch "opaque<socket>" x
close x = throwError $ NumArgs 1 x
