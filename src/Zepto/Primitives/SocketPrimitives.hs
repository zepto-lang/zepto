module Zepto.Primitives.SocketPrimitives where
import Control.Monad.Except (throwError, liftIO)
import Data.Array (listArray)

import qualified Network.Socket as NS
--import qualified Network.Socket.ByteString as NBS (send, sendAll, sendTo, sendAllTo,
--                                                   recv, recvFrom)

import Zepto.Types

socket :: [LispVal] -> IOThrowsError LispVal
socket [f@(SimpleVal (Number (NumS fam))),
        t@(SimpleVal (String type')),
        p@(SimpleVal (Number (NumS protonum)))] = do
        sock <- liftIO (NS.socket (NS.unpackFamily (fromIntegral fam))
                              (lookupType type')
                              (fromIntegral protonum))
        let fd = fromSimple $ Number $ NumS $ fromIntegral (NS.fdSocket sock)
        return $ Vector $ listArray (0, 3) [fd, f, t, p]
    where lookupType s = case s of
                          "no-socket-type" -> NS.NoSocketType
                          "stream"         -> NS.Stream
                          "datagram"       -> NS.Datagram
                          "raw"            -> NS.Raw
                          "rdm"            -> NS.RDM
                          "seq-packet"     -> NS.SeqPacket
                          _                -> NS.NoSocketType
socket [x, SimpleVal (String _), SimpleVal (Number (NumS _))] =
        throwError $ TypeMismatch "small-int" x
socket [_, x, SimpleVal (Number (NumS _))] = throwError $ TypeMismatch "string" x
socket [_, _, x] = throwError $ TypeMismatch "small-int" x
socket x = throwError $ NumArgs 3 x
