{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.TCP.Proxy.Server (
    run
  , Command (..)
  , DataHooks (..)
  , DataHook
  , Config (..)
  , ProxyAction (..)
  , ProxyException (..)
  , logger
  , RemoteAddr
  , directTCPConn
) where

import Prelude as P
import Network.Socket as NS
import Data.ByteString as BS
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
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
import Control.Monad

import Data.Map.Strict as Map

{-
  Proxy with hooks on connection and on incoming/outgoing data
-}

data DataHooks = DataHooks { incoming :: DataHook
                           , outgoing :: DataHook
                           , onDisconnect :: IO ()
                         }
type DataHook = Conduit ByteString IO ByteString

type InitHook = (IP, PortNum) -> (IP, PortNum) -> IO DataHooks 

type AppSource = Producer IO ByteString
type AppSink = Consumer ByteString IO ()

type MakeConn = RemoteAddr -> (AppSource -> AppSink -> (IP, PortNum) -> IO()) -> IO ()

data Config = Config {
              proxyPort :: PortNum 
            , initHook :: InitHook
            , handshake :: Protocol ProxyException ProxyAction

              -- the remote connect operation (customizable)
            , makeConn :: MakeConn

              --  redirect connections contained in the map
              -- to the specified value
              -- TODO: remove this and leave it to the customizable connect Op (?)
            , redirects :: Map RemoteAddr RemoteAddr 
            }

type RemoteAddr = (Either HostName IP, PortNum)

data ProxyAction = ProxyAction {
    command :: Command
  , remoteAddr :: RemoteAddr
   -- what to do when a remote connection is established
   -- Nothing means it failed
  , onConnection :: (Maybe (IP, PortNum) -> Protocol ProxyException ())
 } 

logger = "tcp-proxy"

run :: Config -> IO ()
run config = runTCPServer (serverSettings (fromIntegral $ proxyPort config) "*")
             $ handleConn config

-- TODO: implement bind
-- runTCP server makes sure client sock is closed no matter what
handleConn :: Config -> AppData -> IO ()
handleConn config appData = do
  let clientSrc = (newResumableSource $ appSource appData)
  let clientSink = appSink appData
  debugM logger $ "handling conn..."
  (postHSSrc, handshakeResult) <- fuseProtocol clientSrc clientSink (handshake config)
  proxyAction <- hoistEitherIO handshakeResult

  case command proxyAction of
    BIND -> do
      throwIO UnsupportedFeature
    CONNECT -> do
      let remote =  maybe (remoteAddr proxyAction) id
                          (Map.lookup (remoteAddr proxyAction) (redirects config))
                         
      debugM logger $ "attempting connect to " P.++ (show remote)

      handle
        (\ConnectionFailed -> -- when connection fails inform the client
           void $ fuseProtocol postHSSrc clientSink (onConnection proxyAction Nothing))
        (makeConn config remote $ \serverSrc serverSink ip -> do
          (postConnSrc, afterConn) <-
            fuseProtocol postHSSrc clientSink (onConnection proxyAction (Just ip))
          hoistEitherIO afterConn
          debugM logger $ "succesfully connected to " P.++ (show ip)

          dataHooks <- initHook config (toIP $ appSockAddr appData) ip 

          -- proxy with hooks
          race_ (postConnSrc $$+- (outgoing dataHooks) =$ serverSink)
                (serverSrc =$ (incoming dataHooks) $$ clientSink)
            `finally` (onDisconnect dataHooks)
          return ()        
        ) 

directTCPConn :: MakeConn
directTCPConn remote handler = do
  bracket
    (handleAll (\e -> throwIO ConnectionFailed) $
      getSocketTCP (DBC.pack . showAddr . fst $ remote) (fromIntegral . snd $ remote))
    (NS.close . fst)
    (\(socket, addrInfo) -> do
      handler (toProducer $ sourceSocket socket) (toConsumer $ sinkSocket socket)
              (toIP addrInfo)
    )
