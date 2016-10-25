{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Control.Distributed.Process.Node                   (initRemoteTable, forkProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Network.Wai                                        (Application, Middleware)
import Network.Wai.Handler.Warp                           (run)
import Network.Wai.Middleware.RequestLogger               (logStdoutDev)
import Network.Wai.Middleware.Cors                        (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
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

  let config = Config { masterPid = masterPid
                      , localNode = node
                      }

  run 8080 $ logStdoutDev $ corsPolicy $ app config
