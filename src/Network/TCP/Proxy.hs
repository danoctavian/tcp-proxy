module Network.TCP.Proxy (
    run
  , Command (..)
  , DataHooks (..)
  , DataHook
  , Config (..)
) where

import Prelude as P
import Network.Socket as NS
import Network
import Data.ByteString as BS
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad
import System.Log.Logger
import Data.Serialize as DS
import Control.Monad.NetworkProtocol

{-
  Proxy with hooks on connection and on incoming/outgoing data
-}

data DataHooks = DataHooks {incoming :: DataHook, outgoing :: DataHook}
type DataHook = ByteString -> IO ByteString

type InitHook = SockAddr -> SockAddr -> IO DataHooks 
data Config = Config {proxyPort :: PortID, initHook :: InitHook, handshake :: Protocol ProxyAction, addrMap :: SockAddr -> SockAddr}

-- Currently the proxy only supports CONNECT commands
-- TODO: implement BIND support
data Command = CONNECT | BIND
  deriving Show

type RemoteAddr = Either (HostName, PortNumber) SockAddr
data ProxyAction = ProxyAction {
    command :: Command
  , remoteAddr :: RemoteAddr
   -- what to do when a remote connection is established
  , onConnection :: (Maybe SockAddr -> Protocol ())
 } 
  | Reject (Protocol())

msgSize = 1024 -- magical? magical as fuck. may need more

logger = "tcp-proxy"

run :: Config -> IO ()
run config = withSocketsDo $  do
  debugM logger $ "running proxy server on port " P.++ (show $ proxyPort config)
  sock <- listenOn $ (proxyPort config)
  loop config sock
  return ()

loop config serverSock
  = forever $ NS.accept serverSock >>= liftIO . forkIO . (handleRequest config)

handleRequest config (sock, addr) = do
  debugM logger $ "handling request from " P.++ (show addr)
  return ()
{-
  eitherConnRequest <- runErrorT $ (getConn config $ sock)
  case eitherConnRequest of
    Left err -> debugM Socks5Proxy.logger err 
    Right c -> handleConnection c (sock, addr) config
  -- TODO: add code for catching exceptions when connections is broken
  debugM Socks5Proxy.logger "done showing"
  Network.Socket.sClose sock


fixAddr old@(SockAddrInet tport taddr) 
  | taddr == 16777216 = SockAddrInet tport 16777343 -- magical 127.0.0.1
  |otherwise = old

handleConnection conn@(Connection cmd atyp unfixedAddr) (clientSock, clientAddr) config = do
  let serverSockAddr = addrMap config unfixedAddr

  serverSock <- socket (toAddressFamily atyp) Stream defaultProtocol --
  debugM Socks5Proxy.logger $ "address is " ++ (show serverSockAddr)
  connect serverSock serverSockAddr
  handlers <- (initHook config) clientAddr serverSockAddr 

  forkIO $ forwardPackets clientSock serverSock (outgoing handlers)
  forwardPackets serverSock clientSock (incoming handlers)
  return ()

-}
