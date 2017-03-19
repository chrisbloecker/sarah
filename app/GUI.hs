{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Main
  where
--------------------------------------------------------------------------------
import Control.Concurrent          (forkIO, threadDelay)
import Control.Concurrent.STM      (atomically, newTVar)
import Control.Monad               (forever)
import Control.Monad.Reader        (runReaderT)
import Data.Maybe                  (fromMaybe)
import Data.ByteString             (ByteString)
import Data.ByteString.Char8       (pack)
import Data.Text                   (Text, unpack)
import Graphics.UI.Threepenny.Core
import Network.HTTP.Client         (newManager, defaultManagerSettings)
import Options.Applicative
import Raspberry.IP
import Sarah.GUI
import Sarah.GUI.Model
import Sarah.GUI.Websocket
import Sarah.Middleware            (ConnectionMode (..), encodeAsText)
--import Servant.Client
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict       as M
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
--------------------------------------------------------------------------------

data Options = Options { appHost :: Maybe String
                       , appPort :: Maybe Int
                       , midHost :: Maybe String
                       , midPort :: Maybe Int
                       }

options :: Parser Options
options = Options
      <$> optional appHost
      <*> optional appPort
      <*> optional midHost
      <*> optional midPort
  where
    mkOption l m h = mconcat [long l, metavar m, help h]

    appHost = strOption   $ mkOption "appHost" "HOST"    "The app's hostname"
    appPort = option auto $ mkOption "appPort" "PORT"    "The app's port"
    midHost = strOption   $ mkOption "midHost" "MIDHOST" "Hostname of the middleware"
    midPort = option auto $ mkOption "midPort" "MIDPORT" "Port of the middleware"

--------------------------------------------------------------------------------

run :: Options -> IO ()
run Options{..} = do
  let config = defaultConfig { jsAddr       = Just $ fromMaybe "0.0.0.0" (pack <$> appHost)
                             , jsPort       = Just $ fromMaybe 8023      appPort
                             , jsStatic     = Just "static"
                             , jsCustomHTML = Just "sarah.html"
                             }
      middlewareHost   = fromMaybe "192.168.0.7" midHost
      middlewarePort   = fromMaybe 8090          midPort
      middleware       = WebAddress middlewareHost middlewarePort

  remoteEvents <- atomically $ newTVar M.empty
  counter      <- atomically $ newTVar 0

  let appEnv = AppEnv {..}

  -- use a websocket for communication with the middleware
  putStrLn $ "Connecting to middleware at " ++ middlewareHost ++ ":" ++ show middlewarePort
  forkIO $ do
    WS.runClient middlewareHost middlewarePort "/" (subscribeDeviceStateChanges remoteEvents)
    putStrLn "Websocket closed"

  -- start the threepeny-gui server
  startGUI config (setup appEnv)


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
                (mconcat [fullDesc, progDesc "Sarah GUI", header ""])
