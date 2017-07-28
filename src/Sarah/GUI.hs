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
import Data.Text                           (Text, pack, unpack)
import Data.Sequence                       (empty, (|>))
import Data.Time                           (getZonedTime)
import Graphics.UI.Material                (button, navigationLink, getItem, getItemId, upgradeDom, removeChildren)
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
          let item = H.tr $ do
                         H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text (pack . show $ scheduleDevice)
                         H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text (pack . show $ scheduleAction)
                         H.td H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text (pack . show $ scheduleTimer)
          liftIO . atomically $ modifyTVar scheduleItems (|> item)

    printWithTime "Schedule built"

    addScheduleButton <- button Nothing (Just "Add Schedule")
    dialogId          <- newIdent
    let addScheduleModal = H.customParent "dialog" H.! A.class_ "mdl-dialog"
                                                   H.! A.id (H.toValue dialogId) $ do
                               H.div H.! A.class_ "mdl-dialog__content" $
                                   H.p $
                                       H.text "Allow this site to collect usage data to improve your experience?"
                               H.div H.! A.class_ "mdl-dialog__actions mdl-dialog__actions--full-width" $ do
                                   H.button H.! A.type_ "button"
                                            H.! A.class_ "mdl-button" $
                                       H.text "Agree"
                                   H.button H.! A.type_ "button"
                                            H.! A.class_ "mdl-button close" $
                                       H.text "Disagree"

    items <- liftIO . atomically $ readTVar scheduleItems
    let schedule = H.div $ do
                       H.table H.! A.class_ "mdl-data-table mdl-js-data-table" $ do
                           H.thead $
                               H.tr $ do
                                   H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Device"
                                   H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Command"
                                   H.th H.! A.class_ "mdl-data-table__cell--non-numeric" $ H.text "Timer"
                           H.tbody $
                               forM_ items id
                       getItem addScheduleButton
                       addScheduleModal

    addAction $ do
      let js = "var dialog = document.getElementById(%1);"
            ++ "var showModalButton = document.getElementById(%2);"
            ++ "if (!dialog.showModal) { dialogPolyfill.registerDialog(dialog); }"
            ++ "showModalButton.addEventListener('click', function() { dialog.showModal(); });"
            ++ "dialog.querySelector('.close').addEventListener('click', function() { dialog.close(); });"
      runFunction $ ffi js dialogId (getItemId addScheduleButton)

    liftIO $ forkIO $
        WS.runClient (host middleware) (port middleware) "/" (subscribeDeviceStateChanges remoteEvents)

    -- display the remotes in the remotes tab
    printWithTime "Adding remotes"
    remotesHtml <- fmap (renderHtml . sequence_) <$> liftIO . atomically $ readTVar remoteTiles
    runFunction $ ffi "document.getElementById('remotes-content').innerHTML = %1" remotesHtml

    -- and the schedule in the schedule tab
    let scheduleHtml = renderHtml schedule
    runFunction $ ffi "document.getElementById('schedule-content').innerHTML = %1" scheduleHtml

    printWithTime "Upgrading DOM for Material"
    upgradeDom

    -- register UI actions
    printWithTime "Registering UI actions"
    setCallBufferMode BufferRun
    actions <- liftIO . atomically $ readTVar pageActions
    sequence_ actions
    flushCallBuffer
    setCallBufferMode NoBuffering
