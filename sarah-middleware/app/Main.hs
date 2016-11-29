{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Control.Distributed.Process
import Control.Distributed.Process.Node     (initRemoteTable, forkProcess, runProcess, newLocalNode)
import Control.Exception                    (throw)
import Network.HTTP.Client                  (newManager, defaultManagerSettings)
import Network.Transport                    (EndPointAddress (..), newEndPoint)
import Network.Transport.TCP                (createTransport, defaultTCPParameters)
import Network.Wai                          (Application, Middleware)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant.Client                       (BaseUrl (..), Scheme (Http))
import System.Envy                          (decodeEnv)
--------------------------------------------------------------------------------
import Sarah.Middleware.Api
import Sarah.Middleware.Master
import Sarah.Middleware.Settings
import Sarah.Middleware.Slave
import Sarah.Middleware.Types
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
      mtransport <- createTransport nodeHost nodePort defaultTCPParameters
      case mtransport of
        Left err -> throw err
        Right transport -> do
          endpoint <- newEndPoint transport
          node     <- newLocalNode transport initRemoteTable

          case nodeRole of
            "slave" -> runProcess node (slave masterHost masterPort)

            "master" -> do
              masterPid <- forkProcess node master
              manager   <- newManager defaultManagerSettings

              let config = Config { masterPid = masterPid
                                  , localNode = node
                                  , backend   = BaseUrl Http backendHost backendPort ""
                                  , manager   = manager
                                  }

              run webPort $ logStdoutDev $ corsPolicy $ app config
