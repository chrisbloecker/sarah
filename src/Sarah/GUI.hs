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
import Graphics.UI.Threepenny.Extra
import Prelude                      hiding (div, span)
import Sarah.GUI.Model
import Sarah.GUI.Remote                    (Remote (..), fromDeviceRep)
import Sarah.GUI.Widgets
import Sarah.GUI.Websocket                 (toMaster)
import Sarah.Middleware
import Servant.Client
--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HM
--------------------------------------------------------------------------------

data Navbar = Navbar { remotesButton :: Element
                     , devicesButton :: Element
                     , navbar        :: Element
                     }

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

  liftIO $ putStrLn "Finished building remotes"

  -- ToDo: where and when should we clean up events for devices that don't exist
  --       or are not connected anymore?

  Navbar{..} <- mkNavbar

  content <- div # set id_ "content"
                 # set class_ "container"

  getBody window #+ [ element navbar
                    , element content
                    ]

  -- add the navbar and render the remotes by default
  remoteWidgets <- liftIO . atomically $ readTVar remotes
  element content # set children (HM.elems remoteWidgets)

  on click remotesButton $ \_ -> do
    remoteWidgets <- liftIO . atomically $ readTVar remotes
    element content # set children (HM.elems remoteWidgets)


mkNavbar :: UI Navbar
mkNavbar = do
  remotesButton <- a # set text "Remotes"
  devicesButton <- a # set text "Devices"
  navbar <- nav # set class_ "navbar navbar-default"
                #+ [ div # set class_ "container"
                         #+ [ div # set class_ "navbar-header"
                                  #+ [ button # set type_ "button"
                                              # set class_ "navbar-toggle collapsed"
                                              # set datatoggle "collapse"
                                              # set datatarget "#the-navbar"
                                              # set ariaexpanded "false"
                                              #+ [ span # set class_ "icon-bar"
                                                 , span # set class_ "icon-bar"
                                                 , span # set class_ "icon-bar"
                                                 ]
                                     ]
                            , div # set class_ "collapse navbar-collapse"
                                  # set id_ "the-navbar"
                                  #+ [ ul # set class_ "nav navbar-nav"
                                          #+ [ li #+ [ element remotesButton ]
                                             , li #+ [ element devicesButton ]
                                             ]
                                     ]
                            ]
                   ]
  return Navbar{..}


mkTile :: String -> UI Element -> UI Element
mkTile title content = div # set class_ "col-lg-4 col-md-4 col-sm-6"
                           #+ [ div # set class_ "panel panel-default"
                                    #+ [ div # set class_ "panel-heading"
                                             #+ [ div # set class_ "panel-title"
                                                      # set text title
                                                ]
                                       , content
                                       ]
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
