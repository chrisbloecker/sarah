{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Control.Distributed.Process.Node                   (initRemoteTable, forkProcess, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Network.HTTP.Client                                (newManager, defaultManagerSettings)
import Network.Wai                                        (Application, Middleware)
import Network.Wai.Handler.Warp                           (run)
import Network.Wai.Middleware.RequestLogger               (logStdoutDev)
import Network.Wai.Middleware.Cors                        (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant.Client                                     (BaseUrl (..), Scheme (Http))
import System.Envy                                        (decodeEnv)
--------------------------------------------------------------------------------
import Api
import Master
import Messages
import Settings
import Types
--------------------------------------------------------------------------------

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }


main :: IO ()
main = do
  -- add a settings parser
  msettings <- decodeEnv :: IO (Either String Settings)

  case msettings of
    Left err -> putStrLn err
    Right settings@Settings{..} -> do
      backend <- initializeBackend nodeHost nodePort initRemoteTable
      node    <- newLocalNode backend

      case nodeRole of
        "slave" -> do
          putStrLn "[INFO] Starting slave node..."
          runProcess node undefined

        "master" -> do
          putStrLn "[INFO] Starting master node..."

          -- start the master process
          masterPid <- forkProcess node master

          manager <- newManager defaultManagerSettings

          let config = Config { masterPid = masterPid
                              , localNode = node
                              , backend   = BaseUrl Http backendHost backendPort ""
                              , manager   = manager
                              }

          run webPort $ logStdoutDev $ corsPolicy $ app config
