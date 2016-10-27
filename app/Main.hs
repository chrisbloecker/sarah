{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Control.Distributed.Process.Node                   (initRemoteTable, forkProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Network.HTTP.Client                                (newManager, defaultManagerSettings)
import Network.Wai                                        (Application, Middleware)
import Network.Wai.Handler.Warp                           (run)
import Network.Wai.Middleware.RequestLogger               (logStdoutDev)
import Network.Wai.Middleware.Cors                        (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant.Client                                     (BaseUrl (..), Scheme (Http))
--------------------------------------------------------------------------------
import Api
import Master
import Messages
import Types
--------------------------------------------------------------------------------

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }


main :: IO ()
main = do
  -- add a settings parser
  let host = "127.0.0.1"
      port = "50005"

  backend <- initializeBackend host port initRemoteTable
  node    <- newLocalNode backend

  putStrLn "[INFO] Starting master node..."

  -- start the master process
  masterPid <- forkProcess node master

  manager <- newManager defaultManagerSettings

  let config = Config { masterPid = masterPid
                      , localNode = node
                      , backend   = BaseUrl Http "192.168.0.7" 8080 ""
                      , manager   = manager
                      }

  run 8080 $ logStdoutDev $ corsPolicy $ app config
