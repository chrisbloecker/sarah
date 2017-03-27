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
import Data.Text                           (unpack)
import Graphics.UI.Threepenny       hiding (map)
import Graphics.UI.Threepenny.Core         (ffi, runFunction)
import Graphics.UI.Threepenny.Extra
import Prelude                      hiding (div, span)
import Sarah.GUI.Model
import Sarah.GUI.Remote                    (Remote (..), fromDeviceRep)
import Sarah.GUI.Widgets
import Sarah.GUI.Websocket                 (toMaster)
import Sarah.Middleware
import Servant.Client
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict  as HM
import qualified Graphics.UI.Material as Material
--------------------------------------------------------------------------------

setup :: AppEnv -> Window -> UI ()
setup appEnv@AppEnv{..} window = void $ do
  -- a place to store the remotes
  remotes <- liftIO $ atomically $ newTVar HM.empty

  -- get the status of the middleware, i.e. the connected nodes and their info
  --mStatus <- liftIO $ runClientM Middleware.getStatus clientEnv
  liftIO $ putStrLn "[setup] Requesting status from master..."
  status <- liftIO $ toMaster middleware GetStatusRequest
  liftIO $ case status of
    --Nothing -> void . putStrLn $ "[setup] Couldn't get status from master"
    --Just (GetStatusReply Status{..}) -> do
    GetStatusReply Status{..} -> do
      putStrLn "[setup] Received status from master, building remotes..."
      forM_ connectedNodes $ \NodeInfo{..} ->
        forM_ nodeDevices $ \(deviceName, deviceRep) ->
          case fromDeviceRep deviceRep of
            Left err -> return ()
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
                  widget           = runReaderT (buildRemote model) remoteBuilderEnv
              remote <- runUI window $ mkTile (unpack nodeName ++ ":" ++ unpack deviceName) widget
              liftIO . atomically $ modifyTVar remotes (HM.insert deviceAddress remote)

  -- ToDo: where and when should we clean up events for devices that don't exist
  --       or are not connected anymore?

  mnavigation <- getElementById window "navigation"
  mcontent    <- getElementById window "content"

  case (mnavigation, mcontent) of
    (Just navigation, Just content) -> do
      navRemotes <- button # set class_ "mdl-button mdl-js-button"
                           # set text "Remotes"

      element navigation # set children [ navRemotes ]

      on click navRemotes $ \_ -> do
        remoteWidgets <- liftIO . atomically $ readTVar remotes
        element content # set children (HM.elems remoteWidgets)

      -- add the navbar and render the remotes by default
      remoteWidgets <- liftIO . atomically $ readTVar remotes
      element content # set children (HM.elems remoteWidgets)

      Material.upgradeDom

    (_, _) -> return ()


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


renderStatus :: Status -> [UI Element]
renderStatus Status{..} = map renderNodeInfo connectedNodes -- $ \nodeInfo -> mkTile (nodeInfo^.nodeName & unpack) (nodeInfo^.nodeDevices & map )
  where
    renderNodeInfo :: NodeInfo -> UI Element
    renderNodeInfo NodeInfo{..} = mkTile (unpack nodeName) (listGroup "Devices" $ map (uncurry showDevice) nodeDevices)

    showDevice :: DeviceName -> DeviceRep -> UI Element
    showDevice deviceName deviceRep = listGroupItem [ string ("a device: " ++ unpack deviceName) ]
