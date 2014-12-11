{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Network.TCP.Proxy.Socks4 where

import Network.TCP.Proxy
import Data.List as L
import Data.Serialize
import Network.Socket as NS
import Control.Applicative
import Data.Word
import Data.ByteString as BS
import Data.Conduit.Cereal as DCC
import Control.Monad.NetworkProtocol

data Request = Request {
    version :: Word8
  , cmd :: Command
  , sockAddr :: SockAddr
  , userid :: ByteString
 }

protocol = do
  req <- recvMsg 
  if (version req == 4)
  then undefined
  else undefined -- Reject (send )

-- identd behaviour not implemented
data ResultCode = RequestGranted | RequestRejected 
data Response = Response {respVersion :: Word8, code :: ResultCode, remoteAddr :: SockAddr}

instance Serialize ResultCode where
  put RequestGranted = putWord8 90
  put RequestRejected = putWord8 91
  get = undefined

-- serialize IPV4 only
instance Serialize SockAddr where
  get = SockAddrInet <$> (fmap PortNum getWord16host) <*> getWord32host
  put = undefined

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


{-
doSocks4Handshake :: GetConn
doSocks4Handshake conn = do
  connRequest <- getMessage conn parseSocks4Cmd (isValidSocks4Cmd, "invalid connect command"    )
  liftIO $ NBS.sendAll conn  $ DB.concat ["\0\90", DB.replicate 6 0]
  return connRequest

getMessage conn parse (validate, errMsg) = do
  tcpMsg <- liftIO $ NBS.recv conn msgSize
  handshakeParse <- return (parseOnly parse tcpMsg)
  hs <- case handshakeParse of
          Left err -> CME.throwError "failed parse "
          Right hs -> if (validate hs) then return hs else CME.throwError errMsg
  return hs


parseSocks4Cmd :: Parser Connection
parseSocks4Cmd = do
  versionByte <- anyChar
  cmd <- fmap fromJust $ satisfyWith (toCMD . word8ToChar) maybeToBool
  port <- anyWord16be

  -- this is architecture specific... TODO: figure out endianess properly
  address <- anyWord32le -- only IPV4
  let terminator = 0
  userid <- DA.takeTill (== terminator)
  word8 terminator
  let atyp = IPV4
  return (Connection cmd atyp $ SockAddrInet (portNumberle port) address)

parseHandshake :: Parser ClientHandshake
parseHandshake = do
  verB <- anyChar
  methCount <- fmap ord anyChar
  methods <- replicateM methCount anyChar
  return $ CH verB methods

isValidHandshake :: ClientHandshake -> Bool
isValidHandshake (CH ver methods) = ver == (chr 5) && L.elem '\0' methods
-}


