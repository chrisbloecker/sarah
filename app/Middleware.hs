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
import Control.Monad.Logger                 (runStderrLoggingT)
import Data.Maybe                           (fromMaybe)
import Data.Monoid                          ((<>))
import Database.Persist.MySQL               (ConnectInfo (..), createMySQLPool, defaultConnectInfo, runSqlPool)
import Network.Transport.TCP                (createTransport, defaultTCPParameters)
import Options.Applicative
import Servant.Client
--------------------------------------------------------------------------------
import Raspberry.Hardware
import Sarah.Middleware
--------------------------------------------------------------------------------
import qualified Data.ByteString    as BS
import qualified Data.Yaml          as Y
import qualified Network.WebSockets as WS
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
            serverState <- initState

            let connectionInfo = defaultConnectInfo { connectHost     =              dbHost
                                                    , connectPort     = fromIntegral dbPort
                                                    , connectUser     =              dbUser
                                                    , connectPassword =              dbPassword
                                                    , connectDatabase =              dbDatabase
                                                    }

            pool <- runStderrLoggingT (createMySQLPool connectionInfo 1)

            putStrLn "Running migration"
            runSqlPool doMigrations pool

            node <- newLocalNode transport initRemoteTable

            masterPid <- forkProcess node (runMaster (subscribers serverState) timeout pool)
            putStrLn $ "Master started at " ++ show masterPid

            let config = Config { master     = mkMaster masterPid
                                , localNode  = node
                                , runLocally = \p -> liftIO $ do m <- newEmptyMVar
                                                                 runProcess node $ do r <- p
                                                                                      liftIO $ putMVar m r
                                                                 takeMVar m
                                , getPool    = pool
                                }

            -- Launch a server for communication with clients using websockets
            putStrLn $ "Launching server at " ++ host masterNode ++ ":" ++ show webPort
            WS.runServer (host masterNode) webPort $ runServer config serverState


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
