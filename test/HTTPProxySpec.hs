{-# LANGUAGE OverloadedStrings #-}
module HTTPProxySpec (main, spec) where

import Prelude as P
import Test.Hspec
import Test.QuickCheck
import Data.Conduit as DC
import Data.Conduit.List as DC
import Network.Curl
import Network.Curl.Opts
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp as Warp
import Filesystem.Path.CurrentOS
import Data.Text
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

import Control.Concurrent

import Network.TCP.Proxy.Server as Proxy
import Network.TCP.Proxy.Socks4 as Socks4

simpleProxyConfig p =  Proxy.Config { proxyPort = p
            , initHook = \_ _ -> return DataHooks { incoming = DC.map P.id
                                                  , outgoing = DC.map P.id
                                                  , onDisconnect = return ()
                                                 }
            , handshake = Socks4.serverProtocol
       }

main :: IO ()
main = hspec spec

localhost = "127.0.0.1"

spec :: Spec
spec = do
  describe "http_proxy" $ do
    it "proxies http traffic" $ (runResourceT runFetchTest) >> P.putStrLn "wtf" >> return ()
  return ()

runFetchTest = do
  let root = "test-data/test-server-root"
  let serverPort = 3333
  let proxyPort = 1080
  let app = staticApp $ defaultWebAppSettings (fromText root)
  allocateThread $ forkIO $ Warp.run serverPort app
  allocateThread $ forkIO $ Proxy.run $ simpleProxyConfig proxyPort
  
  liftIO $ threadDelay $ 10 ^ 8
  liftIO $ forM ["tinyFile.txt"] $ \fileName -> do
    let fullPath = (fromText root) </> (fromText fileName)
    contents <- liftIO $ P.readFile (unpack $ fromRight $ toText fullPath)
    (_, proxiedContents) <- liftIO $ curlGetString 
       (localhost ++ ":" ++ (show serverPort) ++ "/" ++ unpack fileName)
       [Network.Curl.Opts.CurlProxy $ "socks4://127.0.0.1:" ++ (show proxyPort)]
    proxiedContents `shouldBe` contents
  return ()


fromRight (Right v) = v

allocateThread t = allocate t (\tid -> killThread tid)
