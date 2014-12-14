{-# LANGUAGE ExistentialQuantification, RankNTypes, LambdaCase #-}
module Network.TCP.Proxy.Socks4 where

import Network.TCP.Proxy hiding (remoteAddr)
import Data.List as L
import Data.Serialize
import Network.Socket as NS
import Control.Applicative
import Data.Word
import Data.ByteString as BS
import Control.Monad.NetworkProtocol
import Data.IP
import Network.TCP.Proxy.Common

protocol = do
  req <- recvMsg 
  let defR = Response 0
  -- for a reject resp or for a resp to connect request the addr field is ignored by client
  let dummyAddr = toIPv4 $ L.replicate ipv4Bytes 0
  if (version req == 4)
  then return $ ProxyAction (cmd req) (Right (IPv4 $ remoteIP req), remotePort req)
        $ \case
            Just (IPv4 ip, port) -> sendMsg $ defR RequestGranted ip port
            _ -> do -- either Nothing or an ipv6 (wrong)
               sendMsg $ defR RequestRejected dummyAddr 0
               throwException ConnectionFailed
  else sendMsg (defR RequestRejected dummyAddr 0) >> throwException HandshakeException

data Request = Request {
    version :: Word8
  , cmd :: Command
  , remotePort :: Word16
  , remoteIP :: IPv4
  , userid :: ByteString
 }

-- identd behaviour not implemented
data ResultCode = RequestGranted | RequestRejected 
data Response = Response { respVersion :: Word8, code :: ResultCode
                         , respIP :: IPv4, respPort :: Word16}

instance Serialize ResultCode where
  put RequestGranted = putWord8 90
  put RequestRejected = putWord8 91
  get = undefined

instance Serialize Response where
  put (Response v code ip port) = put v >> put code >> put ip >> putWord16be port
  get = undefined

instance Serialize Request where
  get = Request <$> get <*> get <*> getWord16be <*> get <*> bsTakeWhile (/= 0)
  put = undefined


