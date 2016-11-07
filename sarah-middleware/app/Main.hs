{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Control.Distributed.Process.Node     (initRemoteTable, forkProcess, runProcess)
import Network.HTTP.Client                  (newManager, defaultManagerSettings)
import Network.Transport.TCP                (createTransport, defaultTCPParameters)
import Network.Wai                          (Application, Middleware)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant.Client                       (BaseUrl (..), Scheme (Http))
import System.Envy                          (decodeEnv)
--------------------------------------------------------------------------------
import Api
import Master
import Settings
import Slave
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
      mtransport <- createTransport nodeHost nodePort initRemoteTable
      case mtransport of
        Left err -> putStrLn err
        Right transport -> do
          endpoint <- newEndpoint transport

          case nodeRole of
            "slave" -> do
              putStrLn "[INFO] Starting slave node..."
              runProcess node slave

            "master" -> do
              putStrLn "[INFO] Starting master node..."

              -- start the master process
              masterPid <- forkProcess node master

              -- start a discovery node

              manager <- newManager defaultManagerSettings

              let config = Config { masterPid = masterPid
                                  , localNode = node
                                  , backend   = BaseUrl Http backendHost backendPort ""
                                  , manager   = manager
                                  }

              run webPort $ logStdoutDev $ corsPolicy $ app config
