{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Network.TCP.Proxy.Common where

import Data.Serialize
import Data.IP
import Data.List as L
import Data.Serialize
import Network.Socket as NS
import Control.Applicative
import Data.ByteString as BS
import Data.Typeable
import Control.Exception
import Data.Word

-- Currently the proxy only supports CONNECT commands
-- TODO: implement BIND support
data Command = CONNECT | BIND
  deriving Show

data ProxyException = HandshakeException | UnsupportedFeature
                    | ConnectionFailed
  deriving (Show, Typeable)
instance Exception ProxyException

type PortNum = Word16
ipv4Bytes = 4

-- using network byte order (big endian)
instance Serialize IPv4 where
  get = toIPv4 . L.map fromIntegral . BS.unpack <$> getByteString ipv4Bytes
  put = putByteString . BS.pack . L.map fromIntegral . fromIPv4

-- WARNING: THIS IS PROBABLY WRONG. (it's not what other proxies expect)
-- TODO: correct
instance Serialize IPv6 where
  get = toIPv6 . L.map fromIntegral . BS.unpack <$> getByteString ipv4Bytes
  put = putByteString . BS.pack . L.map fromIntegral . fromIPv6

instance Serialize Command where
  get = (byte 1 *> return CONNECT) <|> (byte 2 *> return BIND)
  put CONNECT = putWord8 1 
  put BIND = putWord8 2

-- serialize utils
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

-- other utils
hoistEitherIO (Left e) = throwIO e
hoistEitherIO (Right v) = return v

try' :: IO a -> IO (Either SomeException a)
try' = try

handleAll :: (SomeException -> IO a) -> IO a -> IO a
handleAll = handle

toIP (SockAddrInet (PortNum p) a)
  = (IPv4 .fromRight . decode . runPut . putWord32host $ a, p)
toIP (SockAddrInet6 (PortNum p) _ a _) = undefined -- (IPv6 $ toIPv4 $ , p)

showAddr (Left host) = host
showAddr (Right ip) = show ip

mapLeft f (Left v) = Left $ f v
mapLeft f (Right v) = (Right v)

fromRight (Right v) = v

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

