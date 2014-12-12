{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Network.TCP.Proxy (
    run
  , Command (..)
  , DataHooks (..)
  , DataHook
  , Config (..)
  , ProxyAction (..)
  , ProxyException (..)
) where

import Prelude as P
import Network.Socket as NS
import Network
import Data.ByteString as BS
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either
import Control.Concurrent
import Control.Monad
import System.Log.Logger
import Data.Serialize as DS
import Control.Monad.NetworkProtocol
import Control.Monad.NetworkProtocol.Conduit
import Data.Conduit
import Data.Conduit.Network
import Data.Streaming.Network
import Data.Word
import Control.Exception
import Data.Typeable
import Data.IP
import Data.Streaming.Network
import Network.TCP.Proxy.Common

{-
  Proxy with hooks on connection and on incoming/outgoing data
-}

data DataHooks = DataHooks {incoming :: DataHook, outgoing :: DataHook}
type DataHook = ByteString -> IO ByteString

type InitHook = SockAddr -> SockAddr -> IO DataHooks 

data Config = Config {
              proxyPort :: Word16 
            , initHook :: InitHook
            , handshake :: Protocol ProxyException ProxyAction
            }

type RemoteAddr = (Either HostName IP, Word16)
data ProxyAction = ProxyAction {
    command :: Command
  , remoteAddr :: RemoteAddr
   -- what to do when a remote connection is established
   -- Nothing means it failed
  , onConnection :: (Maybe (IP, Word16) -> Protocol ProxyException ())
 } 

data ProxyException = HandshakeException | UnsupportedFeature
                    | ConnectionFailed 
  deriving (Show, Typeable)
instance Exception ProxyException

logger = "tcp-proxy"

run :: Config -> IO ()
run config = runTCPServer (serverSettings (fromIntegral $ proxyPort config) "*")
             $ handleWithExceptions config

handleWithExceptions config appData = do
  res <- runEitherT $ handleConn config appData
  case res of 
    Left e -> errorM logger $ "failed with " P.++ (show e)
    Right _ -> debugM logger "finished succesfully"

-- TODO: implement bind
handleConn config appData = do
  let clientSrc = newResumableSource $ appSource appData    
  (postHSSrc, handshakeResult) <-runConduit
                              $ fuseProtocol clientSrc (appSink appData) (handshake config)
  proxyAction <- hoistEither handshakeResult
  case command proxyAction of
    BIND -> do
      left UnsupportedFeature
    CONNECT -> do
      let remote = remoteAddr proxyAction
      connResult <- liftIO $ try' $ getSocketGen Stream
                                   (showAddr . fst $ remote) (fromIntegral . snd $ remote)
      -- perform onConnection protocol
      (postConnSrc, afterConn) <- runConduit
         $ fuseProtocol postHSSrc (appSink appData)
            (onConnection proxyAction
              $ eitherToMaybe $ fmap (sockAddrToIP . addrAddress . snd) connResult)
      hoistEither afterConn

      (socket, addrInfo) <- hoistEither $ mapLeft (\e -> ConnectionFailed) connResult

      -- setup hooks
      dataHooks <-liftIO $ initHook config (appSockAddr appData) (addrAddress addrInfo) 
      return ()
            

-- utils

try' :: IO a -> IO (Either SomeException a)
try' = try

sockAddrToIP (SockAddrInet (PortNum p) a)
  = (IPv4 .fromRight . decode . runPut . putWord32be $ a, p)
sockAddrToIP (SockAddrInet6 (PortNum p) _ a _) = undefined -- (IPv6 $ toIPv4 $ , p)

showAddr (Left host) = host
showAddr (Right ip) = show ip

mapLeft f (Left v) = Left $ f v
mapLeft f (Right v) = (Right v)

fromRight (Right v) = v

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v
