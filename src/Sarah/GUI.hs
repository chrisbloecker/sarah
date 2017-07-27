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
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Control.Monad.Reader                (runReaderT, ask)
import Data.Text                           (Text, unpack)
import Data.Sequence                       (empty, (|>))
import Data.Time                           (getZonedTime)
import Graphics.UI.Material                (button, navigationLink, getItem, getItemId)
import Graphics.UI.Threepenny              (UI, Window, CallBufferMode (..), newEvent, runUI, setCallBufferMode, flushCallBuffer)
import Graphics.UI.Threepenny.Core         (ffi, runFunction)
import Prelude                      hiding (div, span)
import Raspberry.IP
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Remote                    (Remote (..), fromDeviceRep)
import Sarah.GUI.Schedule                  (buildSchedule)
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

printWithTime :: (MonadIO m) => String -> m ()
printWithTime msg = liftIO $ do
  now <- show <$> getZonedTime
  putStrLn . unwords $ [now, msg]


setup :: AppEnv -> Window -> UI ()
setup appEnv@AppEnv{..} window = void $ do
    -- ALL page events
    pageActions <- liftIO . atomically $ newTVar empty

    -- remote tiles
    remoteTiles <- liftIO . atomically $ newTVar empty

    -- a place to store the events for the remotes. let's use a TVar in case
    -- we want to add more events later on
    remoteEvents <- liftIO . atomically $ newTVar HM.empty

    -- get the status of the middleware, i.e. the connected nodes and their info
    printWithTime "Requesting status from master"
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

    -- schedule items
    scheduleItems <- liftIO . atomically $ newTVar empty

    printWithTime "Requesting schedule from master"
    schedule <- liftIO $ toMaster middleware GetScheduleRequest
    liftIO $ case schedule of
      GetScheduleReply schedules -> do
        printWithTime "Received schedule from master, building schedule overview"
        forM_ schedules $ \Schedule{..} -> do
          print scheduleDevice
    printWithTime "Schedule built"


    items <- liftIO . atomically $ readTVar scheduleItems
    let scheduleTable = H.table H.! A.class_ "mdl-data-table mdl-js-data-table" $ do
                            H.thead $
                                H.tr $ do
                                    H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Device"
                                    H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Command"
                                    H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Timer"
                            H.tbody $ do
                                forM_ items id
                                H.tr $ do
                                    H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "D"
                                    H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "C"
                                    H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "T"

    -- ToDo: where and when should we clean up events for devices that don't exist
    --       or are not connected anymore?

    let addAction :: UI () -> UI ()
        addAction action = liftIO . atomically $ modifyTVar pageActions (|> action)

    liftIO $ forkIO $
        WS.runClient (host middleware) (port middleware) "/" (subscribeDeviceStateChanges remoteEvents)

    remotesButton  <- navigationLink "Remotes"
    scheduleButton <- navigationLink "Schedule"
    logsButton     <- navigationLink "Logs"

    let navButtons = H.div $ do
                         getItem remotesButton
                         getItem scheduleButton
                         getItem logsButton

    liftIO $ print (renderHtml navButtons)

    -- add the nav buttons to the page
    runFunction $ ffi "document.getElementById('navigation').innerHTML = %1" (renderHtml navButtons)

    -- add the remotes by default
    printWithTime "Adding remotes"
    remotesHtml <- fmap (renderHtml . sequence_) <$> liftIO . atomically $ readTVar remoteTiles
    runFunction $ ffi "document.getElementById('content').innerHTML = %1" remotesHtml

    -- when the remotes button is clicked, show the remotes
    addAction $
      onElementIDClick (getItemId remotesButton) $
        runFunction $ ffi "document.getElementById('content').innerHTML = %1" remotesHtml

    -- when the schedule button is clicked, show the schedule
    addAction $
      onElementIDClick (getItemId scheduleButton) $ do
        schedule <- buildSchedule appEnv
        runFunction $ ffi "document.getElementById('content').innerHtml = %1" (renderHtml schedule)

    printWithTime "Upgrading DOM for Material"
    Material.upgradeDom

    -- register UI actions
    printWithTime "Registering UI actions"
    setCallBufferMode BufferRun
    actions <- liftIO . atomically $ readTVar pageActions
    sequence_ actions
    flushCallBuffer
    setCallBufferMode NoBuffering
