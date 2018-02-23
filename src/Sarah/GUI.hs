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
import Control.Monad.Reader                (runReaderT, ask, lift)
import Data.Text                           (Text, pack, unpack)
import Data.Sequence                       (empty, (|>))
import Data.Time                           (getZonedTime)
import Graphics.UI.Material                (button, navigationLink, getItem, getItemId, upgradeDom, toast, mkTileLarge)
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
import qualified Text.Blaze.Internal         as H
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.HashMap.Strict         as HM
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

    -- a function to add actions
    let addAction :: UI () -> UI ()
        addAction action = liftIO . atomically $ modifyTVar pageActions (|> action)

    -- tiles
    remoteTiles   <- liftIO . atomically $ newTVar empty
    scheduleTiles <- liftIO . atomically $ newTVar empty

    -- dialogues
    pageDialogues <- liftIO . atomically $ newTVar empty

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

                            let getSchedule = lift $ do
                                    GetScheduleReply schedule <- toMaster middleware (GetScheduleRequest deviceAddress)
                                    return schedule
                                scheduleBuilderEnv = ScheduleBuilderEnv{..}
                            runUI window $ runReaderT (buildSchedule model) scheduleBuilderEnv

                            printWithTime $ "Setup for " ++ unpack deviceName ++ " complete"

    -- log
    logItems <- liftIO . atomically $ newTVar empty

    printWithTime "Requesting logs from master"
    logs <- liftIO $ toMaster middleware GetLogsRequest
    liftIO $ case logs of
      GetLogsReply logs -> do
        printWithTime "Received logs from master, building log table"
        forM_ logs $ \Log{..} -> do
          let item = H.tr $ do
                         H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text (pack . show $ logDate)
                         H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text (pack . show $ logTime)
                         H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text logSource
                         H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text logText
                         H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text (pack . show $ logLevel)
          liftIO . atomically $ modifyTVar logItems (|> item)

    logElements <- liftIO . atomically $ readTVar logItems
    let log = mkTileLarge "Log" Nothing $
                  H.table H.! A.class_ "mdl-data-table mdl-js-data-table"
                          H.! A.style "width: 100%;" $ do
                      H.thead H.! A.style "width: 100%;" $
                          H.tr $ do
                              H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Date"
                              H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Time"
                              H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Source"
                              H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Text"
                              H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Level"
                      H.tbody $
                          forM_ logElements id

    liftIO $ forkIO $
        WS.runClient (host middleware) (port middleware) "/" (subscribeDeviceStateChanges remoteEvents)

    -- display the remotes in the remotes tab
    printWithTime "Adding remotes"
    remotesHtml <- fmap (renderHtml . sequence_) <$> liftIO . atomically $ readTVar remoteTiles
    runFunction $ ffi "document.getElementById('remotes-content').innerHTML = %1" remotesHtml

    -- and the schedule in the schedule tab
    printWithTime "Adding schedules"
    scheduleHtml <- fmap (renderHtml . sequence_) <$> liftIO . atomically $ readTVar scheduleTiles
    runFunction $ ffi "document.getElementById('schedule-content').innerHTML = %1" scheduleHtml

    -- and the log in the log tab
    let logHtml = renderHtml log
    runFunction $ ffi "document.getElementById('log-content').innerHTML = %1" logHtml

    -- add the dialogues to the body
    printWithTime "Adding dialogues"
    dialoguesHtml <- fmap (renderHtml . sequence_) <$> liftIO . atomically $ readTVar pageDialogues
    runFunction $ ffi "document.getElementById('body').innerHTML += %1" dialoguesHtml

    printWithTime "Upgrading DOM for Material"
    upgradeDom

    -- register UI actions
    printWithTime "Registering UI actions"
    setCallBufferMode BufferRun
    actions <- liftIO . atomically $ readTVar pageActions
    sequence_ actions
    flushCallBuffer
    setCallBufferMode NoBuffering

    toast "Initialisation complete."
