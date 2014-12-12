module Network.TCP.Proxy.Common where

import Data.Serialize
import Data.IP
import Data.List as L
import Data.Serialize
import Network.Socket as NS
import Control.Applicative
import Data.Word
import Data.ByteString as BS

-- Currently the proxy only supports CONNECT commands
-- TODO: implement BIND support
data Command = CONNECT | BIND
  deriving Show

ipv4Bytes = 4

-- using network byte order (big endian)
instance Serialize IPv4 where
  get = toIPv4 . L.map fromIntegral . BS.unpack <$> getByteString ipv4Bytes
  put = putByteString . BS.pack . L.map fromIntegral . fromIPv4

instance Serialize Command where
  get = (byte 1 *> return CONNECT) <|> (byte 2 *> return BIND)
  put = undefined

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
