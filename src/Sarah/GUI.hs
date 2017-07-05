{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
module Sarah.GUI
  ( setup
  ) where
--------------------------------------------------------------------------------
import Control.Concurrent.STM              (atomically, newTVar, readTVar, modifyTVar)
import Control.Monad                       (forM_, unless, void)
import Control.Monad.Reader                (runReaderT, ask)
import Data.Maybe                          (fromJust)
import Data.Text                           (Text, unpack)
import Data.Time                           (getZonedTime)
import Graphics.UI.Threepenny       hiding (map)
import Graphics.UI.Threepenny.Core         (ffi, runFunction)
import Graphics.UI.Threepenny.Extra
import Prelude                      hiding (div, span)
import Sarah.GUI.Model
import Sarah.GUI.Reactive
import Sarah.GUI.Remote                    (Remote (..), fromDeviceRep)
import Sarah.GUI.Websocket                 (toMaster)
import Sarah.Middleware
import Servant.Client
import Text.Blaze.Html.Renderer.String
--------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.HashMap.Strict         as HM
import qualified Graphics.UI.Material        as Material
--------------------------------------------------------------------------------

printWithTime :: String -> IO ()
printWithTime msg = do
  now <- show <$> getZonedTime
  putStrLn . unwords $ [now, msg]


setup :: AppEnv -> Window -> UI ()
setup appEnv@AppEnv{..} window = void $ do
    -- a place to store the remotes
    --remotes <- liftIO $ atomically $ newTVar HM.empty
    pageTiles   <- liftIO . atomically $ newTVar []
    pageActions <- liftIO . atomically $ newTVar []

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
                            -- create new gui update event for the device or reuse an existing one
                            (eventStateChanged, notifyStateChanged) <- do
                                behaviour <- newEvent
                                liftIO $ atomically $ do
                                    events <- readTVar remoteEvents
                                    unless (deviceAddress `HM.member` events) $
                                        modifyTVar remoteEvents (HM.insert deviceAddress behaviour)
                                    -- fromJust should really not fail now...
                                    fromJust . HM.lookup deviceAddress <$> readTVar remoteEvents

                            let remoteRunnerEnv  = RemoteRunnerEnv{..}
                                remoteBuilderEnv = RemoteBuilderEnv{..}
                            --    widget           = runReaderT (buildRemote model) remoteBuilderEnv
                            --remote <- runUI window $ mkTile (unpack nodeName ++ ":" ++ unpack deviceName) widget
                            --liftIO . atomically $ modifyTVar remotes (HM.insert deviceAddress remote)
                            runUI window $ runReaderT (buildRemote model) remoteBuilderEnv
                            printWithTime $ "Setup for " ++ unpack deviceName ++ " complete"

    -- ToDo: where and when should we clean up events for devices that don't exist
    --       or are not connected anymore?

    remotesButtonId <- newIdent
    let navButtons = H.div $
                         H.button H.! A.class_ "mdl-button mdl-js-button"
                                  H.! A.id (H.toValue remotesButtonId) $
                             H.text "Remotes"

    -- add the nav buttons to the page
    liftIO $ printWithTime "Adding nav buttons"
    runFunction $ ffi "document.getElementById('navigation').innterHTML = %1" (renderHtml navButtons)

    -- add the remotes by default
    liftIO $ printWithTime "Adding remotes"
    tilesHtml <- fmap (renderHtml . sequence_) <$> liftIO . atomically $ readTVar pageTiles
    runFunction $ ffi "document.getElementById('content').innerHTML = %1" tilesHtml

    liftIO $ printWithTime "Upgrading DOM for Material"
    Material.upgradeDom

    onElementIDClick remotesButtonId $ runFunction $ ffi "document.getElementById('content').innerHTML = %1" tilesHtml

    -- register UI actions
    setCallBufferMode BufferRun
    actions <- liftIO . atomically $ readTVar pageActions
    sequence_ actions
    flushCallBuffer
    setCallBufferMode NoBuffering


listGroup :: Text -> [H.Html] -> H.Html
listGroup title contents = H.ul H.! A.class_ "list-group" $
                                H.li H.! A.class_ "list-group-item" $ do
                                    H.span H.! A.class_ "glyphicon glyphicon-chevron-right" $
                                        H.text title
                                    H.ul H.! A.class_ "list-group" $
                                        sequence_ contents -- ToDo: does this swallow the content?

listGroupItem :: [H.Html] -> H.Html
listGroupItem es = H.li H.! A.class_ "list-group-item" $
                       sequence_ es

{-
mkTile :: String -> UI Element -> UI Element
mkTile title content = div # set class_ "mdl-card mdl-card-margin mdl-shadow--2dp"
                           #+ [ div # set class_ "mdl-card__title"
                                    #+ [ h2 # set class_ "mdl-card__title-text"
                                            # set text title
                                       ]
                              , div # set class_ "mdl-card__actions mdl-card--border mdl-typography--text-center"
                                    #+ [ content ]
                              ]


listGroup :: String -> [UI Element] -> UI Element
listGroup title contents = ul # set class_ "list-group"
                              #+ [ li # set class_ "list-group-item"
                                      #+ [ span # set class_ "glyphicon glyphicon-chevron-right"
                                                # set text title
                                         , ul # set class_ "list-group"
                                              #+ contents
                                         ]
                                 ]

listGroupItem :: [UI Element] -> UI Element
listGroupItem es = li # set class_ "list-group-item"
                      #+ es
-}
{-
renderStatus :: Status -> [H.Html]
renderStatus Status{..} = map renderNodeInfo connectedNodes -- $ \nodeInfo -> mkTile (nodeInfo^.nodeName & unpack) (nodeInfo^.nodeDevices & map )
  where
    renderNodeInfo :: NodeInfo -> UI Element
    renderNodeInfo NodeInfo{..} = mkTile nodeName (listGroup "Devices" $ map (uncurry showDevice) nodeDevices)

    showDevice :: DeviceName -> DeviceRep -> UI Element
    showDevice deviceName deviceRep = listGroupItem [ string ("a device: " ++ unpack deviceName) ]
-}
