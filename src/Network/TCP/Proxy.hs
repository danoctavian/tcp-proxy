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
import Data.Either
import Control.Concurrent
import Control.Monad
import System.Log.Logger
import Data.Serialize as DS
import Control.Monad.NetworkProtocol
import Control.Monad.NetworkProtocol.Conduit
import Control.Concurrent.Async
import Data.Conduit
import Data.Conduit.List as CL

import Data.Conduit.Network
import Data.Streaming.Network
import Data.Word
import Control.Exception
import Data.Typeable
import Data.IP
import Data.Streaming.Network
import Network.TCP.Proxy.Common
import Data.ByteString.Char8 as DBC

{-
  Proxy with hooks on connection and on incoming/outgoing data
-}

data DataHooks = DataHooks {incoming :: DataHook, outgoing :: DataHook}
type DataHook = ByteString -> IO ByteString

type InitHook = (IP, Word16) -> (IP, Word16) -> IO DataHooks 

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
             $ handleConn config

-- TODO: implement bind
-- runTCP server makes sure client sock is closed no matter what
handleConn :: Config -> AppData -> IO ()
handleConn config appData = do
  let clientSrc = (newResumableSource $ appSource appData)
  let clientSink = (appSink appData)
  (postHSSrc, handshakeResult) <- fuseProtocol clientSrc clientSink (handshake config)
  proxyAction <- hoistEitherIO handshakeResult

  case command proxyAction of
    BIND -> do
      throwIO UnsupportedFeature
    CONNECT -> do
      let remote = remoteAddr proxyAction
      connResult <- try' $ getSocketTCP
                    (DBC.pack . showAddr . fst $ remote) (fromIntegral . snd $ remote)
      (do
        -- perform protocol given connect outcome
        (postConnSrc, afterConn) <- fuseProtocol postHSSrc clientSink 
              (onConnection proxyAction $ eitherToMaybe $ fmap (toIP .  snd) connResult)
        hoistEitherIO afterConn

        (socket, addrInfo) <- hoistEitherIO  connResult
        debugM logger $ "succesfully connected to " P.++ (show addrInfo)
        let serverSrc = newResumableSource  $ sourceSocket socket
        let serverSink = sinkSocket socket
         -- setup hooks
        dataHooks <- initHook config (toIP $ appSockAddr appData) (toIP $ addrInfo)
        -- proxy data
        concurrently (pipeWithHook (outgoing dataHooks)  postConnSrc serverSink)
                     (pipeWithHook (incoming dataHooks) serverSrc clientSink)
        return ()
        ) `finally`  (hoistEitherIO connResult >>= NS.close . fst )

pipeWithHook hook src dest = src $$+- (CL.mapM hook) =$ dest

-- utils

hoistEitherIO (Left e) = throwIO e
hoistEitherIO (Right v) = return v

try' :: IO a -> IO (Either SomeException a)
try' = try

toIP (SockAddrInet (PortNum p) a)
  = (IPv4 .fromRight . decode . runPut . putWord32be $ a, p)
toIP (SockAddrInet6 (PortNum p) _ a _) = undefined -- (IPv6 $ toIPv4 $ , p)

showAddr (Left host) = host
showAddr (Right ip) = show ip

mapLeft f (Left v) = Left $ f v
mapLeft f (Right v) = (Right v)

fromRight (Right v) = v

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v
