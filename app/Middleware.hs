{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Control.Concurrent.MVar
import Control.Distributed.Process
import Control.Distributed.Process.Node     (initRemoteTable, forkProcess, runProcess, newLocalNode)
import Control.Exception                    (throw)
import Data.Maybe                           (fromMaybe)
import Data.Monoid                          ((<>))
import Network.HTTP.Client                  (newManager, defaultManagerSettings)
import Network.Transport                    (EndPointAddress (..), newEndPoint)
import Network.Transport.TCP                (createTransport, defaultTCPParameters)
import Network.Wai                          (Application, Middleware)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Options.Applicative
import Servant.Client
--------------------------------------------------------------------------------
import Raspberry.Hardware
import Sarah.Middleware.Api
import Sarah.Middleware.Master
import Sarah.Middleware.Model
import Sarah.Middleware.Slave
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.Yaml       as Y
--------------------------------------------------------------------------------

data NodeRole = RoleMaster | RoleSlave

data Options = Options { settingsFile :: Maybe String
                       , nodeRole     :: NodeRole
                       }

options :: Parser Options
options = Options <$> optional settingsFile
                  <*> nodeRole
  where
    settingsFile = strOption . mconcat $ [long "settings", metavar "SETTINGS", help "Path to the settings file"]
    nodeRole     = subparser ( command "master" (info (pure RoleMaster) $ progDesc "Run as master")
                            <> command "slave"  (info (pure RoleSlave)  $ progDesc "Run as slave")
                             )

--------------------------------------------------------------------------------

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

go :: Options -> IO ()
go Options{..} = case nodeRole of
  RoleMaster -> do
    let masterSettingsFile = fromMaybe "master.yml" settingsFile
    mSettings <- Y.decodeEither <$> BS.readFile masterSettingsFile

    case mSettings of
      Left err ->
        putStrLn $ "Parsing error: " ++ masterSettingsFile ++ " is invalid. " ++ err
      Right settings@MasterSettings{..} -> do
        mTransport <- createTransport (host masterNode) (show . port $ masterNode) defaultTCPParameters
        case mTransport of
          Left err ->
            throw err
          Right transport -> do
            manager   <- newManager defaultManagerSettings
            let database = BaseUrl Http (host backend) (port backend) ""
            node      <- newLocalNode transport initRemoteTable
            masterPid <- forkProcess node (runMaster $ ClientEnv manager database)

            let config = Config { master     = Master masterPid
                                , localNode  = node
                                , runLocally = \p -> liftIO $ do m <- newEmptyMVar
                                                                 runProcess node $ do r <- p
                                                                                      liftIO $ putMVar m r
                                                                 takeMVar m
                                , backend    = database
                                , manager    = manager
                                }

            run webPort $ logStdoutDev $ corsPolicy $ app config


  RoleSlave -> do
    let slaveSettingsFile = fromMaybe "slave.yml" settingsFile
    mSettings <- Y.decodeEither <$> BS.readFile slaveSettingsFile
    case mSettings of
      Left err ->
        putStrLn $ "Parse error: " ++ slaveSettingsFile ++ " is invalid. " ++ err
      Right settings@SlaveSettings{..} -> do
        mTransport <- createTransport (host nodeAddress) (show . port $ nodeAddress) defaultTCPParameters
        case mTransport of
          Left err ->
            throw err
          Right transport -> do
            node <- newLocalNode transport initRemoteTable
            runProcess node (runSlave settings)


main :: IO ()
main = execParser opts >>= go
  where
    opts = info (helper <*> options)
                (mconcat [fullDesc, progDesc "Sarah Middleware", header ""])
