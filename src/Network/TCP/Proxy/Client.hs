{-# LANGUAGE OverloadedStrings #-}

module Network.TCP.Proxy.Client where

import Data.Streaming.Network
import Data.Conduit.Network
import Data.Word
import Data.ByteString
import Data.IP
import Network.TCP.Proxy.Common
import Control.Monad.NetworkProtocol
import Control.Monad.NetworkProtocol.Conduit
import Data.Conduit

type RemoteConn = Maybe (IP, PortNum)

runProxyTCPClient :: ByteString -> PortNum -> Protocol ProxyException (RemoteConn)
       -> (AppData -> ResumableSource IO ByteString  -> RemoteConn -> IO a) -> IO a
runProxyTCPClient host port handshake handle = do
  runTCPClient (clientSettings (fromIntegral port) host) $ \appData -> do
    let resSrc = newResumableSource $ appSource appData
    let sink = appSink appData
    (postHSSrc, handshakeResult) <- fuseProtocol resSrc sink handshake
    remoteConn <- hoistEitherIO handshakeResult
    handle appData postHSSrc remoteConn
        
