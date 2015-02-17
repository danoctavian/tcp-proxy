module Main where

import Prelude as P
import Network.TCP.Proxy.Server as Proxy
import Network.TCP.Proxy.Socks4 as Socks4

import Data.Conduit as DC
import Data.Conduit.List as DC
import System.Log.Logger
import Data.Map as Map

main = do
  updateGlobalLogger Proxy.logger (setLevel DEBUG)
  runSimpleProxy

runSimpleProxy = do
  debugM logger "starting simple proxy"

  Proxy.run $  Proxy.Config { Proxy.proxyPort = 1080
          , Proxy.initHook = \_ _ -> do
            debugM logger "wtf this ran now"
            return Proxy.DataHooks {
                          Proxy.incoming = DC.map P.id
                        , Proxy.outgoing = DC.map P.id
                        , Proxy.onDisconnect = do
                            debugM Proxy.logger "disconnect happened"             
                      }
          , Proxy.handshake = Socks4.serverProtocol
          , Proxy.makeConn = Proxy.directTCPConn
     }




