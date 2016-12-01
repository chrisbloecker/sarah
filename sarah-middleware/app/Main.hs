{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import           Control.Distributed.Process
import           Control.Distributed.Process.Node     (initRemoteTable, forkProcess, runProcess, newLocalNode)
import           Control.Exception                    (throw)
import           Data.Maybe                           (fromMaybe)
import           Network.HTTP.Client                  (newManager, defaultManagerSettings)
import           Network.Transport                    (EndPointAddress (..), newEndPoint)
import           Network.Transport.TCP                (createTransport, defaultTCPParameters)
import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import           Options.Applicative
import           Servant.Client                       (BaseUrl (..), Scheme (Http))
--------------------------------------------------------------------------------
import           Sarah.Middleware.Api
import           Sarah.Middleware.Master
import           Sarah.Middleware.Model
import           Sarah.Middleware.Settings
import           Sarah.Middleware.Slave
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.Yaml       as Y
--------------------------------------------------------------------------------

data Options = Options { settingsFile :: Maybe String }

options :: Parser Options
options = Options <$> optional settingsFile
  where
    settingsFile = strOption . mconcat $ [long "settings", metavar "SETTINGS", help "Path to the settings file"]

--------------------------------------------------------------------------------

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }


go :: Options -> IO ()
go Options{..} = do
  mSettings <- Y.decode <$> BS.readFile (fromMaybe "settings.yml" settingsFile)

  case mSettings of
    Nothing ->
      putStrLn "settings.yml is invalid."
    Just settings@Settings{..} -> do
      mTransport <- createTransport nodeHost (show nodePort) defaultTCPParameters
      case mTransport of
        Left err ->
          throw err
        Right transport -> do
          endpoint <- newEndPoint transport
          node     <- newLocalNode transport initRemoteTable

          case nodeRole of
            "slave" -> do
              mSlaveSettings <- Y.decode <$> BS.readFile "slave.yml"
              case mSlaveSettings of
                Nothing ->
                  putStrLn "slave.yml is invalid."
                Just slaveSettings ->
                  runProcess node (runSlave slaveSettings)

            "master" -> do
              masterPid <- forkProcess node runMaster
              manager   <- newManager defaultManagerSettings

              let config = Config { masterPid = masterPid
                                  , localNode = node
                                  , backend   = BaseUrl Http backendHost backendPort ""
                                  , manager   = manager
                                  }

              run webPort $ logStdoutDev $ corsPolicy $ app config

            role ->
              putStrLn $ "Unknown role: " ++ role

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (helper <*> options)
           ( fullDesc
          <> progDesc "Sarah Middleware"
          <> header ""
           )
