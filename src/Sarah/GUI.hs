{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI
  ( setup
  ) where
--------------------------------------------------------------------------------
import Control.Concurrent                  (forkIO)
import Control.Concurrent.STM              (atomically, newTVar, readTVar, modifyTVar)
import Control.Monad                       (forM_, unless, void)
import Control.Monad.IO.Class              (liftIO)
import Control.Monad.Reader                (runReaderT, ask)
import Data.Maybe                          (fromJust)
import Data.Text                           (Text, unpack)
import Data.Sequence                       (empty, (|>))
import Data.Time                           (getZonedTime)
import Graphics.UI.Material                (button, getItem, getItemId)
import Graphics.UI.Threepenny              (UI, Window, CallBufferMode (..), newEvent, runUI, setCallBufferMode, flushCallBuffer)
import Graphics.UI.Threepenny.Core         (ffi, runFunction)
import Prelude                      hiding (div, span)
import Raspberry.IP
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Remote                    (Remote (..), fromDeviceRep)
import Sarah.GUI.Websocket                 (toMaster, subscribeDeviceStateChanges)
import Sarah.Middleware
import Servant.Client
import Text.Blaze.Html.Renderer.String
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.HashMap.Strict         as HM
import qualified Graphics.UI.Material        as Material
import qualified Network.WebSockets          as WS
--------------------------------------------------------------------------------

printWithTime :: String -> IO ()
printWithTime msg = do
  now <- show <$> getZonedTime
  putStrLn . unwords $ [now, msg]


setup :: AppEnv -> Window -> UI ()
setup appEnv@AppEnv{..} window = void $ do
    -- for building up the page tiles and UI actions
    pageTiles   <- liftIO . atomically $ newTVar empty
    pageActions <- liftIO . atomically $ newTVar empty

    -- a place to store the events for the remotes. let's use a TVar in case
    -- we want to add more events later on
    remoteEvents <- liftIO . atomically $ newTVar HM.empty

    -- get the status of the middleware, i.e. the connected nodes and their info
    liftIO $ printWithTime "Requesting status from master"
    status <- liftIO $ toMaster middleware GetStatusRequest
    liftIO $ case status of
        GetStatusReply Status{..} -> do
            printWithTime "Received status from master, building remotes"
            forM_ connectedNodes $ \NodeInfo{..} -> do
                printWithTime $ "Setting up node " ++ unpack nodeName
                forM_ nodeDevices $ \(deviceName, deviceRep) -> do
                    printWithTime $ "Setting up device " ++ unpack deviceName
                    case fromDeviceRep deviceRep of
                        Left err -> printWithTime $ "Can't decode device " ++ show deviceName
                        Right (Remote model) -> do
                            let deviceAddress = DeviceAddress nodeName deviceName

                            behaviour@(eventStateChanged, notifyStateChanged) <- newEvent
                            atomically $ modifyTVar remoteEvents (HM.insert deviceAddress behaviour)

                            let runRemote        = liftIO . flip runReaderT RemoteRunnerEnv{..}
                                remoteBuilderEnv = RemoteBuilderEnv{..}
                            runUI window $ runReaderT (buildRemote model) remoteBuilderEnv
                            printWithTime $ "Setup for " ++ unpack deviceName ++ " complete"

    -- ToDo: where and when should we clean up events for devices that don't exist
    --       or are not connected anymore?

    liftIO $ forkIO $
        WS.runClient (host middleware) (port middleware) "/" (subscribeDeviceStateChanges remoteEvents)

    remotesButton <- button Nothing (Just "Remotes")

    let navButtons = H.div $ do
                         getItem remotesButton

    -- add the nav buttons to the page
    runFunction $ ffi "document.getElementById('navigation').innterHTML = %1" (renderHtml navButtons)

    -- add the remotes by default
    liftIO $ printWithTime "Adding remotes"
    tilesHtml <- fmap (renderHtml . sequence_) <$> liftIO . atomically $ readTVar pageTiles
    runFunction $ ffi "document.getElementById('content').innerHTML = %1" tilesHtml

    -- when the remotes button is clicked, show the remotes
    let remotesButtonClickAction = onElementIDClick (getItemId remotesButton) $
                                     runFunction $ ffi "document.getElementById('content').innerHTML = %1" tilesHtml
    liftIO . atomically $ modifyTVar pageActions (|> remotesButtonClickAction)

    liftIO $ printWithTime "Upgrading DOM for Material"
    Material.upgradeDom

    -- register UI actions
    liftIO $ printWithTime "Registering UI actions"
    setCallBufferMode BufferRun
    actions <- liftIO . atomically $ readTVar pageActions
    sequence_ actions
    flushCallBuffer
    setCallBufferMode NoBuffering
