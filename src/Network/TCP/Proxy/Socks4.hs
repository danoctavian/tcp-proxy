{-# LANGUAGE ExistentialQuantification, RankNTypes, LambdaCase, OverloadedStrings #-}
module Network.TCP.Proxy.Socks4 (
    serverProtocol
  , clientProtocol 
  , Command (..)
) where

import Network.TCP.Proxy.Server hiding (remoteAddr)
import Data.List as L
import Data.Serialize
import Network.Socket as NS
import Control.Applicative
import Data.Word
import Data.ByteString as BS
import Control.Monad.NetworkProtocol
import Data.IP
import Network.TCP.Proxy.Common
import Control.Monad

{-
socks4 protocol according to 
http://www.openssh.com/txt/socks4.protocol
-}

protoVersion = 4
responseVersion = 0

serverProtocol = do
  req <- recvMsg 
  let defR = Response responseVersion 
  -- for a reject resp or for a resp to connect request the addr field is ignored by client
  let dummyAddr = toIPv4 $ L.replicate ipv4Bytes 0
  if (version req == protoVersion)
  then return $ ProxyAction (cmd req) (Right (IPv4 $ remoteIP req), remotePort req)
        $ \case
            Just (IPv4 ip, port) -> sendMsg $ defR RequestGranted ip port
            _ -> do -- either Nothing or an ipv6 (wrong)
               sendMsg $ defR RequestRejected dummyAddr 0
               throwException ConnectionFailed
  else sendMsg (defR RequestRejected dummyAddr 0) >> throwException HandshakeException

clientProtocol (remoteIP,  port) command = do
  sendMsg $ Request protoVersion command port remoteIP "irrelevant"
  response <- recvMsg 
  let valid = respVersion response == responseVersion && (code response == RequestGranted)
  when (not valid) $ throwException ConnectionFailed
  case command of
    CONNECT -> return Nothing
    BIND -> return $ Just (IPv4 $ respIP response, respPort response)
  

data Request = Request {
    version :: Word8
  , cmd :: Command
  , remotePort :: Word16
  , remoteIP :: IPv4
  , userid :: ByteString
 }

-- identd behaviour not implemented
data ResultCode = RequestGranted | RequestRejected  deriving (Show, Eq)
data Response = Response { respVersion :: Word8, code :: ResultCode
                         , respIP :: IPv4, respPort :: Word16}

instance Serialize ResultCode where
  put RequestGranted = putWord8 90
  put RequestRejected = putWord8 91
  get = (byte 90 *> return RequestGranted) <|> (byte 91 *> return RequestRejected)

instance Serialize Response where
  put (Response v code ip port) = put v >> put code >> put ip >> putWord16be port
  get = Response <$> get <*> get <*> get <*> getWord16be

instance Serialize Request where
  put (Request v cmd p ip uid)
    = put v >> put cmd >> putWord16be p >> put ip >> putByteString uid >> putWord8 0
  get = Request <$> get <*> get <*> getWord16be <*> get <*> bsTakeWhile (/= 0)


