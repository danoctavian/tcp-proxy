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

protocol = do
  req <- recvMsg 
  let defR = Response (version req)
  -- for a reject resp or for a resp to connect request the addr field is ignored by client
  let dummyAddr = SockAddrInet 0 0
  if (version req == 4)
  then return $ ProxyAction (cmd req) (Right (sockAddr req))
        $ \case
            Nothing -> do
               sendMsg $ defR RequestRejected dummyAddr
               throwException "connection to remote failed"
            Just a -> sendMsg $ defR RequestGranted a
  else sendMsg (defR RequestRejected dummyAddr) >> throwException "wrong version"

data Request = Request {
    version :: Word8
  , cmd :: Command
  , sockAddr :: SockAddr
  , userid :: ByteString
 }

-- identd behaviour not implemented
data ResultCode = RequestGranted | RequestRejected 
data Response = Response {respVersion :: Word8, code :: ResultCode, remoteAddr :: SockAddr}

instance Serialize ResultCode where
  put RequestGranted = putWord8 90
  put RequestRejected = putWord8 91
  get = undefined

instance Serialize Response where
  put (Response v code addr) = put v >> put code >> put addr
  get = undefined

-- serialize IPV4 only
instance Serialize SockAddr where
  get = SockAddrInet <$> (fmap PortNum getWord16host) <*> getWord32host
  put (SockAddrInet (PortNum p) ip) = putWord16host p >> putWord32host ip

instance Serialize Request where
  get = Request <$> get <*> get <*> get <*> bsTakeWhile (/= 0)
  put = undefined

instance Serialize Command where
  get = (byte 1 *> return CONNECT) <|> (byte 2 *> return BIND) 
  put = undefined

bsTakeWhile p = fmap (BS.pack . L.reverse) $ go []
  where
    go acc = do
      w <- getWord8
      if p w
      then go (w : acc)
      else return acc
      
byte :: Word8 -> Get Word8
byte w = do
    x <- lookAhead getWord8
    if x == w
        then getWord8
        else fail $ "Expected byte: '" ++ show w ++ "' got: '" ++ show x ++ "'"


